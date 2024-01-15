{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Main
  where

import PikaC.Syntax.Pika.Expr
import PikaC.Syntax.Pika.FnDef
import PikaC.Syntax.Pika.Parser
import PikaC.Syntax.Pika.Layout

import PikaC.Syntax.Type
import PikaC.TypeChecker.Elaborate

import qualified PikaC.Syntax.PikaCore.Expr as PikaCore
import qualified PikaC.Syntax.PikaCore.FnDef as PikaCore

import PikaC.TypeChecker.Mode

-- import PikaC.Syntax.PikaCore.FnDef

import PikaC.Syntax.ParserUtils

import PikaC.Stage.ToPikaCore
import PikaC.Stage.ToPikaCore.Monad
import PikaC.Stage.ToPikaCore.SimplifyM
import qualified PikaC.Stage.ToPikaCore.Simplify as Simplify

import PikaC.Backend.C.CodeGen
import PikaC.Backend.C.Syntax (declFunction, CFunction)

import PikaC.Backend.SuSLik.CodeGen
import PikaC.Backend.SuSLik.Invoke

import qualified PikaC.Backend.SuSLik.SuSLang.Parser as SuSLang
import PikaC.Backend.SuSLik.SuSLang.ToC (functionToC)
import qualified PikaC.Backend.SuSLik.Syntax as SuSLik

import PikaC.Ppr

import PikaC.Tests.Pika.Test
import PikaC.Tests.Pika.Run
import PikaC.Tests.Pika.Printer

import qualified PikaC.Tests.Module as TestsModule

import Control.Lens

import Control.Arrow ((&&&), (***))

import Control.Monad
import Control.Exception (bracket)

import System.Environment
import System.Exit
import System.IO
import System.Process
import System.Directory (removeFile)

import Data.List
import Data.Maybe

import Data.Bifunctor

import Text.Printf

import Test.QuickCheck
import Data.Validity

data Options =
  Options
    { _optHelp :: Bool
    , _optNoC :: Bool
    , _optOnlyC :: Bool
    , _optNoSuSLik :: Bool
    , _optSimplifierFuel :: SimplifyFuel
    , _optSimplifierLog :: Bool
    , _optSelfTest :: Bool
    , _optGenTests :: Bool
    , _optRunTests :: Bool
    , _optRunPikaTests :: Bool
    , _optCompCert :: Bool
    , _optRunSuSLangTests :: Bool
    , _optDebugSuSLangTests :: Bool
    }
  deriving (Show)

makeLenses ''Options

defaultOpts :: Options
defaultOpts =
  Options False False False False Unlimited False False False False False False False False

type OptionUpdate = ([String], Options) -> ([String], Options)

data OptionHandler =
  OptionHandler
    { optHandlerName :: String
    , optHandlerArgDescription :: Maybe String
    , optHandlerDescription :: String
    , optHandlerUpdate :: OptionUpdate
    }

findOptionHandler :: String -> [OptionHandler] -> OptionHandler
findOptionHandler opt handlers =
  case find go handlers of
    Nothing -> error $ "Unrecognized option " ++ show opt
    Just h -> h
  where
    go h = optHandlerName h == opt

withOptParameter :: (String -> (Options -> Options)) -> String -> OptionUpdate
withOptParameter f optName ([], _) = error $ "Option " ++ optName ++ " expects a parameter"
withOptParameter f _optName (x:xs, opts) = (xs, f x opts)

nullaryOpt :: (Options -> Options) -> String -> OptionUpdate
nullaryOpt f _ p = second f p

option :: String -> Maybe String -> String -> (String -> OptionUpdate) -> OptionHandler
option optName argDescription description update =
  OptionHandler
    { optHandlerName = optName
    , optHandlerDescription = description
    , optHandlerArgDescription = argDescription
    , optHandlerUpdate = update optName
    }

optHandlers :: [OptionHandler]
optHandlers =
  [option "--help" Nothing "Display this message" $ nullaryOpt $
      optHelp .~ True

  ,option "--compcert" Nothing "Use ccomp (CompCert) instead of gcc" $ nullaryOpt $
      optCompCert .~ True

  ,option "--no-c" Nothing "Disable C generation" $ nullaryOpt $
      optNoC .~ True

  ,option "--no-suslik" Nothing "Disable SuSLik generation" $ nullaryOpt $
      optNoSuSLik .~ True

  ,option "--only-c" Nothing "Only print generated C. This will also generate the 'main' function for any Pika tests" $ nullaryOpt $
      optOnlyC .~ True

  ,option "--gen-tests" Nothing "Generate C to run tests in the Pika file. Implies --only-c" $ nullaryOpt $
      ((optGenTests .~ True) . (optOnlyC .~ True))

  ,option "--run-suslang-tests" Nothing "Use SuSLik to generate SuSLang then convert to C for tests. Implies --run-tests" $ nullaryOpt $
      ((optRunTests .~ True) . (optRunSuSLangTests .~ True))

  ,option "--run-tests" Nothing "Generate and run tests from Pika code. Does not show generated code" $ nullaryOpt $
      optRunTests .~ True

  ,option "--run-pika-tests" Nothing "Run .pika tests from the test directory by generatingg C code and running it" $ nullaryOpt $
      optRunPikaTests .~ True

  ,option "--debug-suslang-tests" Nothing "Compile based on SuSLik synthesis and run a debugger on the result (uses lldb)" $ nullaryOpt $
      ((optRunTests .~ True) . (optRunSuSLangTests .~ True) . (optDebugSuSLangTests .~ True))

  ,option "--simplifier-fuel" (Just "<n>") "Run <n> simplifier steps" $ withOptParameter $ \n ->
      optSimplifierFuel .~ Fuel (read n)

  ,option "--simplifier-log" Nothing "Enable logging for each simplifier stage" $ nullaryOpt $
      optSimplifierLog .~ True

  ,option "--self-test" Nothing "Run property tests" $ nullaryOpt $
      optSelfTest .~ True
  ]

printHelp :: IO ()
printHelp = mapM_ go optHandlers
  where
    namesWithArgs =
      map (\h -> optHandlerName h ++ " " ++ fromMaybe "" (optHandlerArgDescription h)) optHandlers
    longestOptLen = maximum $ map length namesWithArgs
    go h =
      printf ("%-" ++ show longestOptLen ++ "s   %s\n")
        (optHandlerName h ++ " " ++ fromMaybe "" (optHandlerArgDescription h))
        (optHandlerDescription h)

parseOptions' :: Options -> [String] -> ([String], Options)
parseOptions' opts [] = ([], opts)
parseOptions' opts (x:xs)
  | isOption x =
      let h = findOptionHandler x optHandlers
          (xs', opts') = optHandlerUpdate h (xs, opts)
      in
      parseOptions' opts' xs'

  | otherwise = first (x:) $ parseOptions' opts xs

parseOptions :: [String] -> ([String], Options)
parseOptions = parseOptions' defaultOpts

isOption :: String -> Bool
isOption ('-':_) = True
isOption _ = False

main :: IO ()
main = do
  args <- getArgs

  let (args', opts) = parseOptions args 

  if _optSelfTest opts
    then do
      TestsModule.checkAllProps
      Simplify.checkAllProps
      pure ()
    else case args' of
      [] | null args -> printHelp
      _ | _optHelp opts -> printHelp
      [fileName] -> do

        fileData <- readFile fileName

        let pikaModule' = parse'' fileName parsePikaModule fileData
              -- TODO: Do elaboration and type checking here:
            pikaModule = toPikaModuleElaborated_unsafe pikaModule'
            layouts = moduleLayouts pikaModule

        -- mapM_ (putStrLn . ppr') (moduleLayouts pikaModule)

        let checkEnv = mkCheckEnv pikaModule'
        print pikaModule'

        -- let (_:firstFnDef:_) = moduleFnDefs pikaModule'
        -- case elaborateFnDef checkEnv firstFnDef of
        --   Left err -> error $ "Elaboration error: " ++ err
        --   Right elaborated -> putStrLn $ ppr' elaborated

        case mapM_ (modeCheck layouts) layouts of
          Left e -> do
            putStrLn $ render $ text "Mode error:" <+> ppr e
            exitFailure
          Right () -> pure ()

        if | _optRunTests opts -> genAndRunTests opts pikaModule
           | otherwise -> withModule opts pikaModule
      _ -> error "Expected file name"

genAndRunTests :: Options -> PikaModuleElaborated -> IO ()
genAndRunTests opts pikaModule = do
  let compiler = if _optCompCert opts
                 then "ccomp"
                 else "gcc"
  let which = if _optRunSuSLangTests opts then SuSLang else C
  putStrLn =<< genAndRun which (_optSimplifierFuel opts) (_optDebugSuSLangTests opts) compiler pikaModule

withModule :: Options -> PikaModuleElaborated -> IO ()
withModule opts pikaModule = do
  -- print $ moduleSynths pikaModule
  -- mapM_ (putStrLn . ppr' . codeGenSynth convertedLayouts) $ moduleSynths pikaModule

    -- Generate the C preamble for testing
  when (_optGenTests opts) $ do
    putStrLn =<< readFile "tests/common/common.h"
    putStrLn . render . pprLayoutPrinters $ convertedLayouts

  forM_ (moduleGenerates pikaModule) $ \fnName ->
    generateFn opts pikaModule fnName

  -- main function for tests
  when (_optOnlyC opts) $ do
    let convertedTests = runQuiet $ runPikaConvert layouts convertedLayouts (map getFnTypeSig fnDefs) $ mkConvertedTests (moduleTests pikaModule)
    putStrLn $ ppr' $ genTestMain convertedLayouts convertedTests
  where
    fnDefs = moduleFnDefs pikaModule
    layouts = moduleLayouts pikaModule
    convertedLayouts = map (runIdentity . runPikaConvert layouts [] (map getFnTypeSig fnDefs) . convertLayout) layouts

    mkConvertedTests :: Logger m => [Test Expr] -> PikaConvert m [Test PikaCore.Expr]
    mkConvertedTests =
        ((traversed . testExpr)
          %%~
            convertExprAndSimplify [])

    generateFn :: Options -> PikaModuleElaborated -> String -> IO ()
    generateFn opts pikaModule fnName =
        if _optOnlyC opts
          then (putStrLn . ppr' . codeGenFn) =<< getPikaCore (moduleLookupFn pikaModule fnName)
          else do
            putStrLn $ "\n*** " <> fnName <> " ***"

            putStrLn "- PikaCore:"
            pikaCore <- getPikaCore $ moduleLookupFn pikaModule fnName
            putStrLn $ ppr' pikaCore
            -- putStrLn $ show pikaCore

            when (not (_optNoC opts)) $ do
              putStrLn "\n- C:"
              putStrLn $ ppr' $ codeGenFn pikaCore
              -- print $ codeGenFn pikaCore

            when (not (_optNoSuSLik opts)) $ do
              let layoutPreds = map (codeGenLayout False) convertedLayouts
                  fnIndPred = codeGenIndPred pikaCore
                  fnSigAttempts = map (`codeGenFnSig` pikaCore) possibleIndirectionLevels

              putStrLn "\n- SuSLik:"
              mapM_ (putStrLn . ppr') layoutPreds
              putStrLn $ ppr' fnIndPred
              mapM_ (putStrLn . ppr') fnSigAttempts

              putStrLn "\n- SuSLang:"
              invokeSuSLikAttempts [] (fnIndPred : layoutPreds) [] fnSigAttempts >>= \case
                Left err -> error $ "SuSLik error: " ++ err
                Right susLangFn -> do
                  -- putStrLn susLang
                  putStrLn $ render $ pprFns $ concatMap functionToC susLangFn

    fuel = _optSimplifierFuel opts

    getPikaCore :: FnDef -> IO PikaCore.FnDef
    getPikaCore fnDef
      | _optSimplifierLog opts =
          runLogIO $ toPikaCore fuel (moduleLayouts pikaModule) (map getFnTypeSig (moduleFnDefs pikaModule)) fnDef
      | otherwise =
          pure . runQuiet $ toPikaCore fuel (moduleLayouts pikaModule) (map getFnTypeSig (moduleFnDefs pikaModule)) fnDef

mkCheckEnv :: PikaModule -> CheckEnv
mkCheckEnv pikaModule =
  CheckEnv
    { _fnEnv = map goFnDef $ moduleFnDefs pikaModule
    , _layoutAdts = map goLayout $ moduleLayouts pikaModule
    , _constructorTypes = concatMap goConstructorType $ moduleAdts pikaModule
    }
  where
    goFnDef fnDef =
      let sig = fnDefTypedBranches fnDef
      in
      (fnDefName fnDef, typePairType sig)
    goLayout layout = (_layoutName layout, _layoutAdt layout)
    goConstructorType = map (_constructorName &&& _constructorType) . _adtConstructors

