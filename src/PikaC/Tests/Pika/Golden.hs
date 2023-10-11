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
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.String

import System.FilePath

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

main :: IO ()
main = do
  let ingredients = consoleTestReporter : defaultIngredients
  -- (defaultMainWithIngredients ingredients . adjustOption (const timeout)) =<< goldenTestTreeWithTimeout
  defaultMainWithIngredients ingredients =<< goldenTestTree
  exitSuccess

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
          fnSig = codeGenFnSig pikaCore

      -- putStrLn "\n- SuSLik:"
      -- mapM_ (putStrLn . ppr') layoutPreds
      -- putStrLn $ ppr' fnIndPred
      -- putStrLn $ ppr' fnSig

      -- putStrLn "\n- SuSLang:"
      let allPreds = fnIndPreds ++ layoutPreds
      -- putStrLn $ "predicates = " ++ show allPreds
      (invokeSuSLikWithTimeout (Just timeoutMilli) [] allPreds [] fnSig) >>= \case
          Left err -> error $
            unlines (map ppr' allPreds ++ [ppr' fnSig])
            ++
            "\n=== SuSLik error: " ++ err
          Right susLangFn ->
            pure . BS.pack . render . pprFns $ concatMap functionToC susLangFn
            -- putStrLn susLang
            -- putStrLn $ render $ pprFns $ concatMap functionToC susLangFn

getPikaCore :: PikaModuleElaborated -> FnDef -> PikaCore.FnDef
getPikaCore pikaModule fnDef =
  runQuiet $ toPikaCore Unlimited (moduleLayouts pikaModule) (map getFnTypeSig (moduleFnDefs pikaModule)) fnDef

