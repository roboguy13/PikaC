{-# LANGUAGE LambdaCase #-}

module PikaC.Tests.Pika.Golden
  where

import Test.Tasty
import Test.Tasty.Ingredients.ConsoleReporter
import Test.Tasty.Golden

import System.IO
import System.Environment
import System.Exit
import qualified System.Timeout as Timeout

import Control.Monad

import Control.Monad.Identity

import Control.Concurrent.Async

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.String

import System.FilePath

import PikaC.Tests.Pika.Run
import PikaC.Syntax.Pika.Parser
import PikaC.Syntax.Pika.FnDef
import qualified PikaC.Syntax.PikaCore.FnDef as PikaCore
import PikaC.Syntax.ParserUtils
import PikaC.Stage.ToPikaCore.SimplifyM
import PikaC.Stage.ToPikaCore.Monad
import PikaC.Stage.ToPikaCore
import PikaC.Backend.SuSLik.CodeGen
import PikaC.Backend.SuSLik.Invoke
import PikaC.Backend.SuSLik.SuSLang.ToC
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

timeoutMicro :: Integral a => a
timeoutMicro =
  1000 *
  timeoutMilli

timeout :: Timeout
timeout =
  mkTimeout timeoutMicro

main :: IO ()
main = do
  let ingredients = consoleTestReporter : defaultIngredients
  (defaultMainWithIngredients ingredients . adjustOption (const timeout)) =<< goldenTestTreeWithTimeout
  exitSuccess

goldenTestTreeWithTimeout :: IO TestTree
goldenTestTreeWithTimeout =
  withAsync goldenTestTree $ \act ->
    Timeout.timeout timeoutMilli (wait act) >>= \case
      Just r -> pure r
      Nothing -> do
        cancel act
        error "Timeout"

goldenTestTree :: IO TestTree
goldenTestTree = do
  pikaFiles <- findByExtension [".pika"] testsPath
  pure $ testGroup "golden tests"
      [ goldenVsString
              baseName
              outputFile
              (runTest pikaFile)
        | pikaFile <- pikaFiles
        , let baseName = takeBaseName pikaFile
        , let outputFile = replaceExtension pikaFile ".golden"
      ]

runTest :: FilePath -> IO ByteString
runTest fileName = do
  fileData <- readFile fileName
  let pikaModule' = parse'' fileName parsePikaModule fileData
            -- TODO: Do elaboration and type checking here:
      pikaModule = toPikaModuleElaborated_unsafe pikaModule'

  if useC
    then fromString <$> genAndRun C Unlimited False cCompiler pikaModule
    else do
      forM_ (moduleGenerates pikaModule) $ \fnName ->
        generateFn pikaModule fnName
      pure mempty -- TODO: Return a Maybe instead of doing this

  where
    generateFn :: PikaModuleElaborated -> String -> IO ()
    generateFn pikaModule fnName = do
      pikaCore <- getPikaCore pikaModule $ moduleLookupFn pikaModule fnName
      let fnDefs = moduleFnDefs pikaModule
          layouts = moduleLayouts pikaModule
          convertedLayouts = map (runIdentity . runPikaConvert layouts [] (map getFnTypeSig fnDefs) . convertLayout) layouts
          layoutPreds = map (codeGenLayout False) convertedLayouts
          fnIndPred = codeGenIndPred pikaCore
          fnSig = codeGenFnSig pikaCore

      putStrLn "\n- SuSLik:"
      mapM_ (putStrLn . ppr') layoutPreds
      putStrLn $ ppr' fnIndPred
      putStrLn $ ppr' fnSig

      putStrLn "\n- SuSLang:"
      (invokeSuSLik [] (fnIndPred : layoutPreds) [] fnSig) >>= \case
          Left err -> error $ "SuSLik error: " ++ err
          Right susLangFn -> do
            -- putStrLn susLang
            putStrLn $ render $ pprFns $ concatMap functionToC susLangFn

getPikaCore :: PikaModuleElaborated -> FnDef -> IO PikaCore.FnDef
getPikaCore pikaModule fnDef =
  pure . runQuiet $ toPikaCore Unlimited (moduleLayouts pikaModule) (map getFnTypeSig (moduleFnDefs pikaModule)) fnDef

