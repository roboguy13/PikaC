{-# LANGUAGE LambdaCase #-}

module PikaC.Tests.Pika.Golden
  where

import Test.Tasty
import Test.Tasty.Ingredients.ConsoleReporter
import Test.Tasty.Ingredients
import Test.Tasty.Options
import Test.Tasty.Runners
import Test.Tasty.Golden

import qualified Data.IntMap as Map

import System.IO
import System.Environment
import System.Exit
import qualified System.Timeout as Timeout

import Control.Monad

import Control.Monad.Identity

import Control.Concurrent.Async

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.String

import System.FilePath

import GHC.Conc
import Control.Monad.STM
import Control.Concurrent.MVar

import Text.Printf

import Control.Exception

import PikaC.Tests.Pika.Run
import PikaC.Syntax.Pika.Parser
import PikaC.Syntax.Pika.FnDef
import PikaC.Backend.SuSLik.Syntax (InductivePredicate)
import qualified PikaC.Syntax.PikaCore.FnDef as PikaCore
import PikaC.Syntax.ParserUtils
import PikaC.Stage.ToPikaCore.SimplifyM
import PikaC.Stage.ToPikaCore.Monad
import PikaC.Stage.ToPikaCore
import PikaC.Backend.SuSLik.CodeGen
import PikaC.Backend.SuSLik.Invoke
import PikaC.Backend.SuSLik.SuSLang.ToC
import PikaC.TypeChecker.Elaborate
import PikaC.Ppr

testsPath :: FilePath
testsPath = "tests"

cCompiler :: String
cCompiler = "gcc"

useC :: Bool
useC = False

timeoutMilli :: Integral a => a
timeoutMilli =
  1000 *
  30 -- <- seconds

-- timeoutMicro :: Integral a => a
-- timeoutMicro =
--   1000 *
--   timeoutMilli

-- timeout :: Timeout
-- timeout =
--   mkTimeout timeoutMicro

data Summary = Summary

instance IsOption Summary where
 defaultValue = Summary
 parseValue _ = Just Summary
 optionName = pure "Summary"
 optionHelp = pure "Print a brief summary"

  -- (defaultMainWithIngredients ingredients . adjustOption (const timeout)) =<< goldenTestTreeWithTimeout

main :: IO ()
main = do
  testTree <- goldenTestTree
  doneVar <- newEmptyMVar
  -- let ingredients = composeReporters consoleTestReporter (summary doneVar (testsNames mempty testTree)) : defaultIngredients
  let ingredients = defaultIngredients
  defaultMainWithIngredients ingredients testTree
  -- testRunner <- async $ defaultMainWithIngredients ingredients testTree
  -- wait testRunner
  -- putMVar doneVar ()
  exitSuccess

summary :: MVar () -> [TestName] -> Ingredient
summary doneVar testNames = TestReporter [] $ \_ _ -> Just $ \statusMap -> do
  waitForDone statusMap
  void $ forkIO $ waitForDone statusMap >> putMVar doneVar ()
  pure $ const $ do
    takeMVar doneVar
    putStrLn "\n--- Summary ---"
    results <- mapM printStatus (Map.toList statusMap)
    printf "# of Successful tests: %s\n" (show (length (filter (isSuccess . resultOutcome) results)))
    pure True
  where
    printStatus (i, statusVar) = do
      Done result <- atomically $
        readTVar statusVar >>= \case
          NotStarted {} -> retry
          Executing {} -> retry
          x -> pure x
      printf "%-14s%s\n" (drop (length groupName + 1) (testNames !! i) ++ ":") (resultShortDescription result)
      pure result

    isSuccess Success = True
    isSuccess _ = False

    waitForDone statusMap = do
      atomically $ do
        statuses <- mapM (readTVar . snd) (Map.toList statusMap)
        check (all isDone statuses)

    isDone NotStarted{} = False
    isDone Executing{} = False
    isDone _ = True

groupName :: String
groupName = "golden tests"

goldenTestTree :: IO TestTree
goldenTestTree = do
  pikaFiles <- findByExtension [".pika"] testsPath
  pure $ testGroup groupName
      [ goldenVsString
              baseName
              outputFile
              (runTest pikaFile)
        | pikaFile <- pikaFiles
        , let baseName = takeBaseName pikaFile
        , let outputFile = replaceExtension pikaFile ".golden"
      ]

-- syncIngredient :: MVar () -> Ingredient
-- syncIngredient doneVar = TestReporter [] $ \_ _ -> Just $ \_ -> do
--   takeMVar doneVar  -- Block until the MVar is filled
--   pure (const $ pure True)

-- consoleReporterWithSync :: MVar () -> Ingredient
-- consoleReporterWithSync doneVar = composeReporters consoleTestReporter $ syncIngredient doneVar


runTest :: FilePath -> IO ByteString
runTest fileName = do
  fileData <- readFile fileName
  let pikaModule' = parse'' fileName parsePikaModule fileData
            -- TODO: Do elaboration and type checking here:
      pikaModule = toPikaModuleElaborated_unsafe pikaModule'

  if useC
    then fromString <$> genAndRun C Unlimited False cCompiler pikaModule
    else do
      let fnPreds = map (getPred pikaModule) (moduleGenerates pikaModule)
      xs <- forM (moduleGenerates pikaModule) $ \fnName ->
              generateFn pikaModule fnPreds fnName
      pure $ mconcat xs
      -- pure mempty -- TODO: Return a Maybe instead of doing this

  where
    getPred :: PikaModuleElaborated -> String -> InductivePredicate
    getPred pikaModule fnName =
      let pikaCore = getPikaCore pikaModule $ moduleLookupFn pikaModule fnName
      in
      codeGenIndPred pikaCore

    generateFn :: PikaModuleElaborated -> [InductivePredicate] -> String -> IO ByteString
    generateFn pikaModule fnIndPreds fnName = do
      let pikaCore = getPikaCore pikaModule $ moduleLookupFn pikaModule fnName
      let fnDefs = moduleFnDefs pikaModule
          layouts = moduleLayouts pikaModule
          convertedLayouts = map (runIdentity . runPikaConvert layouts [] (map getFnTypeSig fnDefs) . convertLayout) layouts
          layoutPreds = map (codeGenLayout False) convertedLayouts
          -- fnIndPred = codeGenIndPred pikaCore
          fnSigAttempts = map (`codeGenFnSig` pikaCore) possibleIndirectionLevels

      -- putStrLn "\n- SuSLik:"
      -- mapM_ (putStrLn . ppr') layoutPreds
      -- putStrLn $ ppr' fnIndPred
      -- putStrLn $ ppr' fnSig

      -- putStrLn "\n- SuSLang:"
      let allPreds = fnIndPreds ++ layoutPreds
      -- putStrLn $ "predicates = " ++ show allPreds
      (invokeSuSLikAttemptsWithTimeout (Just timeoutMilli) [] allPreds [] fnSigAttempts) >>= \case
          Left err -> error $
            unlines (map ppr' allPreds ++ map ppr' fnSigAttempts)
            ++
            "\n=== SuSLik error: " ++ err
          Right susLangFn ->
            pure . BS.pack . render . pprFns $ concatMap functionToC susLangFn
            -- putStrLn susLang
            -- putStrLn $ render $ pprFns $ concatMap functionToC susLangFn

getPikaCore :: PikaModuleElaborated -> FnDef -> PikaCore.FnDef
getPikaCore pikaModule fnDef =
  runQuiet $ toPikaCore Unlimited (moduleLayouts pikaModule) (map getFnTypeSig (moduleFnDefs pikaModule)) fnDef

