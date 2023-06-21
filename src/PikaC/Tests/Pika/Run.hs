{-# LANGUAGE LambdaCase #-}

module PikaC.Tests.Pika.Run
  where

import PikaC.Syntax.Pika.Parser
import qualified PikaC.Syntax.PikaCore.Expr as PikaCore
import qualified PikaC.Syntax.PikaCore.FnDef as PikaCore
import PikaC.Syntax.Pika.FnDef
import PikaC.Syntax.Pika.Expr

import PikaC.Stage.ToPikaCore
import PikaC.Stage.ToPikaCore.SimplifyM
import PikaC.Stage.ToPikaCore.Monad

import PikaC.Backend.C.CodeGen
import PikaC.Backend.C.Syntax

import PikaC.Backend.SuSLik.SuSLang.ToC (functionToC)
import PikaC.Backend.SuSLik.Invoke
import PikaC.Backend.SuSLik.CodeGen as SuSLik
import qualified PikaC.Backend.SuSLik.SuSLang.Parser as SuSLang
import PikaC.Syntax.ParserUtils

import PikaC.Tests.Pika.Printer
import PikaC.Tests.Pika.Test
import PikaC.Ppr

import System.Environment
import System.Exit
import System.IO
import System.Process
import System.Directory (removeFile)

import Control.Exception (bracket)

import Control.Lens

-- | Add forward declarations
pprGenFns :: [PikaCore.FnDef] -> Doc
pprGenFns fns =
  let genFns = map codeGenFn fns
  in
  vcat (map declFunction genFns) $$ vcat (map ppr genFns)

-- | Answers the question: Straight to C or through SuSLik to SuSLang and then to C?
data WhichLang = C | SuSLang
  deriving (Show)

genAndRun :: WhichLang -> SimplifyFuel -> String -> PikaModule -> IO String
genAndRun which fuel compiler pikaModule = do
      -- Generate C file
  bracket (openTempFile "temp" "tests.c")
      (\(fileName, handle) -> do
        hClose handle
        removeFile fileName)
    $ \(fileName, handle) -> do
      hPutStrLn handle =<< readFile "tests/common/common.h"

      hPutStrLn handle . render . pprLayoutPrinters $ convertedLayouts
      mapM_ (hPutStrLn handle . ppr') =<< mapM convertSynths (moduleSynths pikaModule)

      -- invokeSuSLik [] (fnIndPred : layoutPreds) [] fnSig >>= \case

      case which of
        C -> (hPutStrLn handle . render . pprGenFns) =<< mapM generateFn (moduleGenerates pikaModule)
        SuSLang -> do
          mapM_ (hPutStrLn handle . ppr') =<<
            mapM (suslangConvert . moduleLookupFn pikaModule)
                 (moduleGenerates pikaModule)

      let convertedTests = runQuiet $ runPikaConvert layouts convertedLayouts fnDefs $ mkConvertedTests (moduleTests pikaModule)
      hPutStrLn handle $ ppr' $ genTestMain convertedLayouts convertedTests

      hClose handle

      bracket (openBinaryTempFile "temp" "tests")
          (\(execFile, execHandle) -> do
            hClose execHandle
            removeFile execFile)
        $ \(execFile, execHandle) -> do 
        hClose execHandle

        system $ compiler ++ " -w " ++ fileName ++ " -o " ++ execFile

        -- callProcess execFile []
        readProcess execFile [] ""
  where
    fnDefs = moduleFnDefs pikaModule
    layouts = moduleLayouts pikaModule

    mkConvertedTests :: [Test Expr] -> PikaConvert Quiet [Test PikaCore.Expr]
    mkConvertedTests =
        ((traversed . testExpr)
          %%~
            convertExprAndSimplify [])

    convertedLayouts = map (runIdentity . runPikaConvert layouts [] fnDefs . convertLayout) layouts

    -- Go via SuSLik to SuSLang then C
    suslangConvert :: FnDef -> IO CFunction
    suslangConvert fnDef = do
      pikaCore <- getPikaCore fnDef
      let layoutPreds = map (codeGenLayout False) convertedLayouts
          fnIndPred = codeGenIndPred pikaCore
          fnSig = codeGenFnSig pikaCore
      invokeSuSLik [] (fnIndPred : layoutPreds) [] fnSig >>= \case
        Left err -> error $ "SuSLik error: " ++ err
        Right susLang ->
          pure $ functionToC susLang

    getPikaCore :: FnDef -> IO PikaCore.FnDef
    getPikaCore fnDef
      -- | _optSimplifierLog opts =
      --     runLogIO $ toPikaCore fuel (moduleLayouts pikaModule) (moduleFnDefs pikaModule) fnDef
      | otherwise =
          pure . runQuiet $ toPikaCore fuel (moduleLayouts pikaModule) (moduleFnDefs pikaModule) fnDef

    generateFn = getPikaCore . moduleLookupFn pikaModule

    convertSynths :: Synth -> IO CFunction
    convertSynths synth = do
      let (layoutPreds, fnSig) = SuSLik.codeGenSynth convertedLayouts synth

      mapM_ (putStrLn . ppr') layoutPreds
      putStrLn $ ppr' fnSig

      invokeSuSLik [] layoutPreds [] fnSig >>= \case
        Left err -> error $ "SuSLik error: " ++ err
        Right susLang ->
          pure $ functionToC susLang


