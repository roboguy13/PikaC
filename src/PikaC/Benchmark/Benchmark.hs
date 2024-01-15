{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module PikaC.Benchmark.Benchmark
  where

import Dhall hiding (map)

import qualified PikaC.Backend.C.Syntax as C
import PikaC.Backend.SuSLik.Syntax (InductivePredicate, FnSig)
import PikaC.Backend.SuSLik.SuSLang.Syntax as SuSLang
import PikaC.Backend.SuSLik.CodeGen
import PikaC.Backend.SuSLik.Invoke
import PikaC.Syntax.Pika.Parser
import PikaC.Syntax.Pika.FnDef
import qualified PikaC.Syntax.PikaCore.FnDef as PikaCore
import PikaC.Syntax.ParserUtils
import PikaC.Stage.ToPikaCore.Monad
import PikaC.Stage.ToPikaCore.SimplifyM
import PikaC.Stage.ToPikaCore

import Control.Monad.Identity

import System.FilePath
import Data.String
import Text.Printf

import Criterion
import Criterion.Types
import Criterion.Main.Options
import Statistics.Types

import Control.DeepSeq

benchmarkPath :: FilePath
benchmarkPath = "tests"

benchmarkPathC :: FilePath
benchmarkPathC = benchmarkPath </> "c"

benchmarkConfigPath :: FilePath
benchmarkConfigPath = benchmarkPathC </> "config.dhall"

data PikaCode =
  PikaCode
  { pikaCodeFile :: FilePath
  , pikaCodeFileData :: String
  }
  deriving (Generic, Show)

data PikaBenchmark a =
  PikaBenchmark
  { benchName :: String
  , benchContents :: a
  }
  deriving (Generic, Show, Functor)

type PikaCompileBenchmark = PikaBenchmark PikaCode
type PikaSynthBenchmark = PikaBenchmark Compiled

data BenchmarkResult =
  BenchmarkResult
  { benchResultName :: String
  , benchResultCompileReport :: Report
  , benchResultSynthReport :: Report
  -- , benchResultCTime :: Double
  }
  deriving (Show)

instance NFData PikaCode
instance NFData a => NFData (PikaBenchmark a)

instance FromDhall PikaCode
instance FromDhall (PikaBenchmark PikaCode)

type BenchmarkError = String

parseBenchmarks :: FilePath -> IO [PikaCompileBenchmark]
parseBenchmarks filePath = do
  configData <- readFile filePath
  fileNames <- input auto (fromString configData) :: IO [String]

  fileDataList <- mapM readFile (map (benchmarkPath </>) fileNames)
  let pikaCodeList = zipWith PikaCode fileNames fileDataList
  pure $ map go pikaCodeList
  where
    go pikaCode@PikaCode { .. } =
      PikaBenchmark
        { benchName = takeBaseName pikaCodeFile
        , benchContents = pikaCode
        }

benchmarkToCriterionCompile :: PikaCompileBenchmark -> Benchmarkable
benchmarkToCriterionCompile PikaBenchmark{ .. } =
  case benchContents of
    PikaCode { .. } ->
        nf (compileToSuSLik pikaCodeFile) pikaCodeFileData

benchmarkToCriterionSynth :: PikaSynthBenchmark -> Benchmarkable
benchmarkToCriterionSynth PikaBenchmark{ .. } =
    nfIO (synthesize benchContents)

compileBenchmark :: PikaCompileBenchmark -> PikaSynthBenchmark
compileBenchmark = fmap go
  where
    go PikaCode { .. } =
      compileToSuSLik pikaCodeFile pikaCodeFileData

-- benchmarkToCriterionRun :: PikaBenchmark -> Benchmark
-- benchmarkToCriterionRun = undefined

toBenchmark :: (PikaBenchmark a -> Benchmarkable) -> PikaBenchmark a -> Benchmark
toBenchmark f theBenchmark@PikaBenchmark { .. } =
  bench benchName (f theBenchmark)

runBenchmarks :: [PikaCompileBenchmark] -> IO [BenchmarkResult]
runBenchmarks benchmarks = do
  let compiledBenchmarks = map compileBenchmark benchmarks
      synthConfig = defaultConfig { resamples = 3 }
  compileReports <- traverse (benchmarkGo "compile" defaultConfig benchmarkToCriterionCompile) benchmarks
  synthReports <- compiledBenchmarks `deepseq` traverse (benchmarkGo "synthesize" synthConfig benchmarkToCriterionSynth) compiledBenchmarks --(benchmarkWith' synthConfig . benchmarkToCriterionSynth) compiledBenchmarks

  pure $ zipWith3 BenchmarkResult (map benchName benchmarks) compileReports synthReports

  where
    benchmarkGo prefix config f x = do
      putStrLn $ "benchmark: " ++ prefix ++ "/" ++ benchName x
      benchmarkWith' config $ f x

toLaTeX :: [BenchmarkResult] -> String
toLaTeX results =
  unlines $
    [cmd "begin{table}"
    ,cmd "begin{tabular}{|c|c|c|}"
    ,cmd "hline"
    ,cmd "Name & Compilation time & Synthesis time\\\\"
    ,cmd "hline"
    ]
    ++ map toRow results ++
    [cmd "hline"
    ,cmd "end{tabular}"
    ,cmd "end{table}"
    ]
  where
    toRow BenchmarkResult { .. } =
      "verb|" ++ benchResultName ++ "| & " ++ fromReport benchResultCompileReport ++ " & "  ++ fromReport benchResultSynthReport ++ "\\\\"

    cmd :: String -> String
    cmd s = "\\" <> s

    fromReport = printf "%.2f" . estPoint . anMean . reportAnalysis

-- runBenchmarks :: [PikaCompileBenchmark] -> Benchmark
-- runBenchmarks benchmarks =
--   let compiledBenchmarks = map compileBenchmark benchmarks
--   in
--   bgroup "main"
--     [bgroup "Compilation"
--       (map (toBenchmark benchmarkToCriterionCompile) benchmarks)
--
--     ,compiledBenchmarks `deepseq`
--       bgroup "Synthesis"
--         (map (toBenchmark benchmarkToCriterionSynth) compiledBenchmarks)
--     ]

-- compileC :: [String] -> String -> C.CFunction -> FilePath -> IO ()
-- compileC inputGenerators outputPrinter fn execFileName = undefined

synthesize :: Compiled -> IO [SuSLang.Function]
synthesize (Compiled indPreds (fnSigAttemptLists)) =
  fmap sequenceA (mapM (invokeSuSLikAttemptsWithTimeout (Just timeoutMilli) [] indPreds []) fnSigAttemptLists) >>= \case
    Left err -> error $ "=== SuSLik error: " ++ err
    Right r -> pure $ map head r

data Compiled = Compiled [InductivePredicate] [[FnSig]]
  deriving (Generic)

instance Semigroup Compiled where
  Compiled xs1 ys1 <> Compiled xs2 ys2 =
    Compiled (xs1 <> xs2) (ys1 <> ys2)

instance Monoid Compiled where
  mempty = Compiled [] []

instance NFData Compiled

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

