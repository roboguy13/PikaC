{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PikaC.Benchmark.Benchmark
  where

import Dhall hiding (map)

import qualified PikaC.Backend.C.Syntax as C
import PikaC.Backend.SuSLik.Syntax (InductivePredicate, FnSig)
import PikaC.Backend.SuSLik.SuSLang.Syntax as SuSLang
import PikaC.Backend.SuSLik.CodeGen
import PikaC.Backend.SuSLik.Invoke
import PikaC.Backend.SuSLik.SuSLang.ToC (functionToC)
import PikaC.Syntax.Pika.Parser
import PikaC.Syntax.Pika.FnDef
import qualified PikaC.Syntax.PikaCore.FnDef as PikaCore
import PikaC.Syntax.ParserUtils
import PikaC.Stage.ToPikaCore.Monad
import PikaC.Stage.ToPikaCore.SimplifyM
import PikaC.Stage.ToPikaCore
import PikaC.Utils
import PikaC.Ppr

import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LBS

import Data.Bifunctor

import qualified System.Process.Typed as Typed

import Control.Monad.Identity
import Control.Monad
import Control.Arrow (first, second, (***), (&&&))
import Data.Tuple.Extra (firstM)

import System.Process
import System.Exit
import Control.Exception
import System.Directory (removeFile)

import System.FilePath
import System.IO
import Data.String
import Text.Printf

import Data.List

import Criterion
import Criterion.Types
import Criterion.Main.Options
import Statistics.Types

import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput

import Control.DeepSeq

benchmarkPath :: FilePath
benchmarkPath = "tests"

benchmarkPathC :: FilePath
benchmarkPathC = benchmarkPath </> "c"

benchmarkConfigPath :: FilePath
benchmarkConfigPath = benchmarkPathC </> "config.dhall"

cCompiler :: FilePath
cCompiler = "gcc"

includePath :: FilePath
includePath = "tests" </> "c"

haskellIncludePath :: FilePath
haskellIncludePath = "tests" </> "haskell"

haskellCompiler :: FilePath
haskellCompiler = "ghc"

data CType = CInt | CPtr String | CNoPtr String
  deriving (Generic, Show)

instance FromDhall CType

data CTest =
  CTest
  { haskellFile :: FilePath
  , inputGenerators :: [CType]
  , outputPrinter :: CType
  }
  deriving (Generic, Show)

data PikaTest' f =
  PikaTest
  { fileName :: FilePath
  , cTest :: f CTest
  }
  deriving (Generic)

sequencePikaTest' :: (Monad f, Applicative g) => PikaTest' f -> f (PikaTest' g)
sequencePikaTest' (PikaTest p ftest) = do
  test <- ftest
  pure $ PikaTest p (pure test)

deriving instance Show (f CTest) => Show (PikaTest' f)

type PikaTest = PikaTest' Maybe
type PikaTestC = PikaTest' Identity

data PikaCode =
  PikaCode
  { pikaCodeFileData :: String
  , pikaTest :: PikaTest
  }
  deriving (Generic, Show)

data PikaBenchmark a =
  PikaBenchmark
  { benchName :: String
  , benchContents :: a
  }
  deriving (Generic, Show, Functor, Foldable, Traversable)

type PikaCompileBenchmark = PikaBenchmark PikaCode
type PikaSynthBenchmark = PikaBenchmark (Compiled, PikaTest)
type PikaCBenchmark = PikaBenchmark (Synthed, PikaTestC)

data BenchmarkResult =
  BenchmarkResult
  { benchResultName :: String
  , benchResultCompileReport :: Report
  , benchResultSynthReport :: Report
  , benchResultCompileAstSize :: Int
  , benchResultSynthAstSize :: Int
  }
  deriving (Show)

data CBenchmarkResult a =
  CBenchmarkResult
  { cbenchResultName :: String
  , cbenchResultCTime :: a
  , cbenchResultHaskellTime :: a
  }

instance NFData CType
instance NFData CTest
instance NFData PikaTest
instance NFData PikaCode
instance NFData a => NFData (PikaBenchmark a)

instance FromDhall CTest
instance FromDhall PikaTest
instance FromDhall PikaCode
instance FromDhall (PikaBenchmark PikaCode)

type BenchmarkError = String

parseBenchmarks :: FilePath -> IO [PikaCompileBenchmark]
parseBenchmarks filePath = do
  configData <- readFile filePath
  tests <- input auto (fromString configData) :: IO [PikaTest]

  fileDataList <- mapM readFile (map ((benchmarkPath </>) . fileName) tests)
  let pikaCodeList = zipWith PikaCode fileDataList tests
  pure $ map go pikaCodeList
  where
    go pikaCode@PikaCode { .. } =
      PikaBenchmark
        { benchName = takeBaseName (fileName pikaTest)
        , benchContents = pikaCode
        }

benchmarkToCriterionCompile :: PikaCompileBenchmark -> Benchmarkable
benchmarkToCriterionCompile PikaBenchmark{ .. } =
  case benchContents of
    PikaCode { .. } ->
        nf (compileToSuSLik (fileName pikaTest)) pikaCodeFileData

benchmarkToCriterionSynth :: PikaSynthBenchmark -> Benchmarkable
benchmarkToCriterionSynth PikaBenchmark{ .. } =
    nfIO (synthesize (fst benchContents))

compileBenchmark :: PikaCompileBenchmark -> PikaSynthBenchmark
compileBenchmark = fmap (go &&& pikaTest)
  where
    go PikaCode { .. } =
      compileToSuSLik (fileName pikaTest) pikaCodeFileData

synthBenchmark :: PikaSynthBenchmark -> IO (Maybe PikaCBenchmark)
synthBenchmark = fmap go . traverse (firstM synthIt)
  where
    go :: PikaBenchmark (Synthed, PikaTest) -> Maybe (PikaBenchmark (Synthed, PikaTestC))
    go x0 = sequenceA $ flip fmap x0 $ \(x, y) -> do
      z <- sequencePikaTest' y
      pure (x, z)

toBenchmark :: (PikaBenchmark a -> Benchmarkable) -> PikaBenchmark a -> Benchmark
toBenchmark f theBenchmark@PikaBenchmark { .. } =
  bench benchName (f theBenchmark)

runBenchmarks :: [PikaCompileBenchmark] -> IO ([PikaSynthBenchmark], [BenchmarkResult])
runBenchmarks benchmarks = do
  let compiledBenchmarks = map compileBenchmark benchmarks
      synthConfig = defaultConfig { resamples = 1 }

      parsed :: [PikaModule]
      parsed = map toParsed  benchmarks

      compiled :: [Compiled]
      compiled = map (fst . benchContents) compiledBenchmarks

  compileReports <- traverse (benchmarkGo "compile" defaultConfig benchmarkToCriterionCompile) benchmarks
  synthReports <- compiledBenchmarks `deepseq` traverse (benchmarkGo "synthesize" synthConfig benchmarkToCriterionSynth) compiledBenchmarks

  pure (compiledBenchmarks
       ,zipWith5 BenchmarkResult (map benchName benchmarks) compileReports synthReports (map size parsed) (map size compiled))
  where
    benchmarkGo prefix config f x = do
      putStrLn $ "benchmark: " ++ prefix ++ "/" ++ benchName x
      benchmarkWith' config $ f x

cBenchmarkToLaTeX :: [CBenchmarkResult Report] -> String
cBenchmarkToLaTeX results =
  unlines $
    [cmd "begin{tabular}{|c|c|c|}"
    ,cmd "hline"
    ,"Name & C Time (s) & GHC Time (s)\\\\"
    ,cmd "hline"
    ]
    ++ map toRow results ++
    [cmd "hline"
    ,cmd "end{tabular}"
    ]
    where
      toRow CBenchmarkResult { .. } =
        cmd "verb|" ++ cbenchResultName ++ "| & " ++ fromReport cbenchResultCTime ++ " & " ++ fromReport cbenchResultHaskellTime ++ "\\\\"

toLaTeX :: [BenchmarkResult] -> String
toLaTeX results =
  unlines $
    [cmd "begin{tabular}{|c|c|c|c|c|c|}"
    ,cmd "hline"
    ,"Name & Pika AST & SuSLik AST & Pika AST / SuSLik AST & Compilation time (s) & Synthesis time (s)\\\\"
    ,cmd "hline"
    ]
    ++ map toRow results ++
    [cmd "hline"
    ,cmd "end{tabular}"
    ,""
    ,""
    ,""
    ]
  where
    toRow BenchmarkResult { .. } =
      let astRatio :: Double
          astRatio = fromIntegral benchResultCompileAstSize / fromIntegral benchResultSynthAstSize
      in
      cmd "verb|" ++ benchResultName ++ "| & "  ++ show benchResultCompileAstSize ++ " & " ++ show benchResultSynthAstSize ++ " & " ++ printf "%.3f" astRatio  ++ " & " ++ fromReport benchResultCompileReport ++ " & "  ++ fromReport benchResultSynthReport ++ "\\\\"

fromReport :: Report -> String
fromReport = printf "%.3f" . estPoint . anMean . reportAnalysis

cmd :: String -> String
cmd s = "\\" <> s

runCBenchmarksCompiled :: BenchCheck a -> COpts -> HaskellOpts -> [PikaSynthBenchmark] -> IO [CBenchmarkResult a]
runCBenchmarksCompiled diff cOpts haskellOpts = (runCBenchmarks diff cOpts haskellOpts . catMaybes) <=< traverse synthBenchmark

data HaskellOpts = HaskellUnoptimized | HaskellO2
  deriving (Show)

data COpts = CUnoptimized | CO3
  deriving (Show)

instance Ppr HaskellOpts where
  ppr HaskellUnoptimized = text "-O0"
  ppr HaskellO2 = text "-O2"

instance Ppr COpts where
  ppr CUnoptimized = text "-O0"
  ppr CO3 = text "-O3"

runCBenchmarks :: BenchCheck a -> COpts -> HaskellOpts -> [PikaCBenchmark] -> IO [CBenchmarkResult a]
runCBenchmarks diff cOpts haskellOpts =
  mapM $ \bench -> do
    benchC (benchName bench)
           diff
           (ppr' cOpts)
           (ppr' haskellOpts)
           (inputGenerators $ runIdentity $ cTest (snd (benchContents bench)))
           (outputPrinter $ runIdentity $ cTest (snd (benchContents bench)))
           (firstSynthed (fst (benchContents bench)))
           (haskellFile $ runIdentity $ cTest (snd (benchContents bench)))

benchC :: String -> BenchCheck a -> String -> String -> [CType] -> CType -> C.CFunction -> FilePath -> IO (CBenchmarkResult a)
benchC name diff cOpts ghcOpts inputGenerators outputPrinter fn haskellCodeFile =
  let params = zipWith const (map (("_x" ++) . show) [0..]) inputGenerators
      out = "_out"
      decls = ("  loc " ++ out ++ " = malloc(sizeof(loc));") : map (("  loc " <>) . (<> " = 0;")) (params)
      code =
        unlines $
          ["#include \"common/common.h\""
          ,"#include \"common/generators.h\""
          ,""
          ,ppr' fn
          ,""
          ,"int main() {"
          ]
          ++ decls
          ++ ["  for (int i = 0; i < TEST_ITERATIONS; ++i) {"]
          ++ map ("  " ++) (zipWith applyInputGenerator inputGenerators params)
          ++ ["  " ++ C.cfunctionName fn ++ "(" ++ (intercalate ", " params) ++ ", " ++ out ++ ");"]
          ++ ["  " ++ applyOutputPrinter outputPrinter out]
          ++ ["    printf(\"\\n\");"]
          ++ ["  }"]
          ++
          ["  return 0;"
          ,"}"
          ]
  in
  bracket (openTempFile "temp" "bench.c")
    (\(cCodeTempName, cCodeHandle) -> do
      hClose cCodeHandle
      removeFile cCodeTempName)

    (\(cCodeTempName, cCodeHandle) -> do
          let execTempName = "temp/benchC"
              execHaskellTempName = "temp/BenchHaskell"

          hPutStrLn cCodeHandle code
          hClose cCodeHandle

          let cleanUp = do
                          removeFile execTempName
                          removeFile execHaskellTempName

          -- putStrLn code

          flip finally cleanUp $ do
            systemQuiet $ cCompiler ++ " " ++ cOpts ++ " -I" ++ includePath ++ " " ++ cCodeTempName ++ " -o " ++ execTempName
            systemQuiet $ haskellCompiler ++ " -fforce-recomp -XCPP " ++ ghcOpts ++ " -I" ++ haskellIncludePath ++ " " ++ (haskellIncludePath </> "Common.hs") ++ " " ++ haskellCodeFile ++ " -o " ++ execHaskellTempName

            (cReport, haskellReport) <- diffBenchmarkResults name diff (execTempName, []) (execHaskellTempName, [])

            -- cReport <- benchmark' $ nfIO $ systemVeryQuiet $ execTempName -- ++ " > " ++ cOutName
            -- haskellReport <- benchmark' $ nfIO $ systemVeryQuiet $ execHaskellTempName -- ++ " > " ++ haskellOutName

            -- cOut <- hGetContents cOutHandle
            -- haskellOut <- hGetContents haskellOutHandle
            --
            -- when (cOut /= haskellOut) $ do
            --   putStrLn $ "ERROR: Benchmark results differ between C and Haskell."
            --   exitFailure

            pure $ CBenchmarkResult
              { cbenchResultName = name
              , cbenchResultCTime = cReport
              , cbenchResultHaskellTime = haskellReport
              })

data BenchCheck a where
  SanityCheck :: BenchCheck ()
  NoDiff :: BenchCheck Report

diffBenchmarkResults :: forall a. String -> BenchCheck a -> (String, [String]) -> (String, [String]) -> IO (a, a)
diffBenchmarkResults name diff cCmd haskellCmd = do
  let doSystem :: BenchCheck a -> (String, [String]) -> IO ()
      doSystem = \case
          SanityCheck -> \(cmd, args) -> systemVeryQuiet $ unwords (cmd : args)
          NoDiff -> \(cmd, args) -> systemVeryQuiet $ unwords (cmd : args)


  bracket (openTempFile "temp" "c-out.txt")
    (\(cOutName, cOutHandle) -> do
      hClose cOutHandle
      removeFile cOutName)
    (\(cOutName, cOutHandle) ->
      bracket (openTempFile "temp" "haskell-out.txt")
        (\(haskellOutName, haskellOutHandle) -> do
          hClose haskellOutHandle
          removeFile haskellOutName)
        (\(haskellOutName, haskellOutHandle) -> do
            let doBenchmark' :: BenchCheck a -> IO () -> IO a
                doBenchmark' = \case
                                 SanityCheck -> id
                                 NoDiff -> benchmark' . nfIO

            case diff of
              NoDiff -> do
                cReport <- doBenchmark' diff $ doSystem diff cCmd
                haskellReport <- doBenchmark' diff $ doSystem diff haskellCmd

                pure (cReport, haskellReport)
              SanityCheck -> do
                cOut <- uncurry readProcessText cCmd
                haskellOut <- uncurry readProcessText haskellCmd

                if (cOut /= haskellOut)
                  then do
                    putStrLn $ "ERROR: Benchmark results differ between C and Haskell for benchmark " ++ show name
                    putStrLn $ "Diff:"
                    putStrLn $ ppDiff $ getGroupedDiff (map (:[]) (show cOut)) (map (:[]) (show haskellOut))
                    putStrLn $ "=== C Output: " ++ showFirstChars 1000 cOut
                    putStrLn ""
                    putStrLn $ "=== Haskell Output: " ++ showFirstChars 1000 haskellOut
                    exitFailure
                  else pure ((), ())))

showFirstChars :: Int -> T.Text -> String
showFirstChars n txt
  | n >= T.length txt = show txt
  | otherwise         = show (T.take n txt) <> "..."


applyInputGenerator :: CType -> String -> String
applyInputGenerator CInt arg = "  " <> arg <> " = 7;"
applyInputGenerator (CNoPtr generator) arg = "  " <> arg <> " = " <> generator <> "();"
applyInputGenerator (CPtr _) _ = error "applyInputGenerator: CPtr: TODO: Implement"
-- applyInputGenerator (CNoPtr generator) arg = "  " <> arg <> " = " <> generator <> "();"

applyOutputPrinter :: CType -> String -> String
applyOutputPrinter CInt arg = "  printf(%d, " <> arg <> ");"
applyOutputPrinter (CNoPtr printer) arg = "  " <> printer <> "(" <> arg <> ");"
applyOutputPrinter (CPtr printer) arg =
  unlines
    ["  loc _derefOut = READ_LOC(" ++ arg ++ ", 0);"
    ,applyOutputPrinter (CNoPtr printer) "_derefOut"
    ]

synthesize :: Compiled -> IO [SuSLang.Function]
synthesize (Compiled indPreds (fnSigAttemptLists)) =
  fmap sequenceA (mapM (invokeSuSLikAttemptsWithTimeout (Just timeoutMilli) [] indPreds []) fnSigAttemptLists) >>= \case
    Left err -> error $ "=== SuSLik error: " ++ err
    Right r -> pure $ map head r

synthIt :: Compiled -> IO Synthed
synthIt = fmap (mconcat . map (Synthed . functionToC)) . synthesize

data Compiled = Compiled [InductivePredicate] [[FnSig]]
  deriving (Generic)

data Synthed = Synthed [C.CFunction]

instance Semigroup Synthed where
  Synthed xs <> Synthed ys = Synthed (xs ++ ys)

instance Monoid Synthed where
  mempty = Synthed []

instance Size Compiled where
  size (Compiled xs ys) = size xs + size (map head ys)

firstSynthed :: Synthed -> C.CFunction
firstSynthed (Synthed (x:_)) = x

instance Semigroup Compiled where
  Compiled xs1 ys1 <> Compiled xs2 ys2 =
    Compiled (xs1 <> xs2) (ys1 <> ys2)

instance Monoid Compiled where
  mempty = Compiled [] []

instance NFData Compiled

systemEchoed :: String -> IO ()
systemEchoed cmd = do
  putStrLn cmd
  systemQuiet cmd

systemVeryQuiet :: String -> IO ()
systemVeryQuiet cmd = systemQuiet (cmd ++ " >/dev/null")

-- systemWithOutHandle :: String -> [String] -> IO Text
-- systemWithOutHandle cmd args = do
--   readProcessText cmd args mempty
--   undefined

readProcessText :: FilePath -> [String] -> IO T.Text
readProcessText cmd args = do
  let input = mempty
      processConfig = Typed.setStdin (Typed.byteStringInput $ LBS.fromStrict $ T.encodeUtf8 input) $
                      Typed.setStdout Typed.byteStringOutput $
                      Typed.proc cmd args
  (exitCode, output) <- Typed.readProcessStdout processConfig
  return $ T.decodeUtf8 $ LBS.toStrict output

systemQuiet :: String -> IO ()
systemQuiet cmd = do
  hFlush stdout
  catch (system cmd) except >>= \case
    failure@(ExitFailure n) -> exitWith failure
    ExitSuccess -> pure ()
  where
    except :: SomeException -> IO a
    except e = do
      print e
      exitFailure

timeoutMilli :: Integral a => a
timeoutMilli =
  1000 *
  30 -- <- seconds

compileFnToSuSLik :: PikaModuleElaborated -> String -> Compiled
compileFnToSuSLik pikaModule fnName =
  let fnPreds = map (getPred pikaModule) (moduleGenerates pikaModule)
      pikaCore = getPikaCore pikaModule $ moduleLookupFn pikaModule fnName
      fnDefs = moduleFnDefs pikaModule
      layouts = moduleLayouts pikaModule
      convertedLayouts = map (runIdentity . runPikaConvert layouts [] (map getFnTypeSig fnDefs) . convertLayout) layouts
      layoutPreds = map (codeGenLayout False) convertedLayouts
      fnSigAttempts = map (`codeGenFnSig` pikaCore) possibleIndirectionLevels
      allPreds = fnPreds ++ layoutPreds
  in
  Compiled allPreds [fnSigAttempts]

toParsed :: PikaCompileBenchmark -> PikaModule
toParsed PikaBenchmark { .. } =
  parse'' "<>" parsePikaModule (pikaCodeFileData benchContents)

compileToSuSLik :: FilePath -> String -> Compiled
compileToSuSLik fileName fileData =
  let pikaModule' = parse'' fileName parsePikaModule fileData
      pikaModule = toPikaModuleElaborated_unsafe pikaModule'

      fnNames = moduleGenerates pikaModule'
  in
  mconcat $ map (compileFnToSuSLik pikaModule) fnNames

-- TODO: Move these somewhere better
getPikaCore :: PikaModuleElaborated -> FnDef -> PikaCore.FnDef
getPikaCore pikaModule fnDef =
  runQuiet $ toPikaCore Unlimited (moduleLayouts pikaModule) (map getFnTypeSig (moduleFnDefs pikaModule)) fnDef

getPred :: PikaModuleElaborated -> String -> InductivePredicate
getPred pikaModule fnName =
  let pikaCore = getPikaCore pikaModule $ moduleLookupFn pikaModule fnName
  in
  codeGenIndPred pikaCore

