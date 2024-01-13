{-# LANGUAGE LambdaCase #-}

module PikaC.Tests.Pika.Run
  where

import PikaC.Syntax.Pika.Parser
import qualified PikaC.Syntax.PikaCore.Expr as PikaCore
import qualified PikaC.Syntax.PikaCore.FnDef as PikaCore
import PikaC.Syntax.Pika.FnDef
import PikaC.Syntax.Pika.Expr
import PikaC.Syntax.Type

import PikaC.Utils

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
pprFns :: [CFunction] -> Doc
pprFns fns =
  vcat (map declFunction fns) $$ vcat (map ppr fns)

-- pprGenFns :: [PikaCore.FnDef] -> Doc
-- pprGenFns fns =
--   let genFns = map codeGenFn fns
--   in
--   vcat (map declFunction genFns) $$ vcat (map ppr genFns)

-- | Answers the question: Straight to C or through SuSLik to SuSLang and then to C?
data WhichLang = C | SuSLang
  deriving (Show)

genAndRun :: WhichLang -> SimplifyFuel -> Bool -> String -> PikaModule' Typed -> IO String
genAndRun which fuel debug compiler pikaModule = do
      -- Generate C file
  bracket (openTempFile "temp" "tests.c")
      (\(fileName, handle) -> do
        hClose handle
        removeFile fileName)
    $ \(fileName, handle) -> do
      hPutStrLn handle =<< readFile "tests/common/common.h"

      let synths = moduleSynths pikaModule

      hPutStrLn handle . render . pprLayoutPrinters $ convertedLayouts
      -- (hPutStrLn handle . render . pprFns . concat) =<< mapM (uncurry convertSynths) (removeOne synths)
      synthFns <- concat <$> mapM (uncurry convertSynths) (removeOne synths)

      let fnSigs = map getSynthTypeSig synths ++ map getFnTypeSig fnDefs

      -- invokeSuSLik [] (fnIndPred : layoutPreds) [] fnSig >>= \case

      pikaFns <- case which of
                  C -> map codeGenFn <$> mapM (generateFn fnSigs) (moduleGenerates pikaModule)
                  SuSLang -> do
                      concat <$> mapM (suslangConvert fnSigs . moduleLookupFn pikaModule)
                           (moduleGenerates pikaModule)

      let allFns = synthFns ++ pikaFns
      hPutStrLn handle . render $ pprFns allFns

      let convertedTests = runQuiet $ runPikaConvert layouts convertedLayouts fnSigs $ mkConvertedTests (moduleTests pikaModule)
      hPutStrLn handle $ ppr' $ genTestMain convertedLayouts convertedTests

      hClose handle

      bracket (openBinaryTempFile "temp" "tests")
          (\(execFile, execHandle) -> do
            hClose execHandle
            removeFile execFile)
        $ \(execFile, execHandle) -> do 
        hClose execHandle

        let debugOption = if debug then " -g " else ""

        system $ compiler ++ debugOption ++ " -w " ++ fileName ++ " -o " ++ execFile

        -- callProcess execFile []
        if debug
          then spawnProcess "lldb" [execFile] >>= waitForProcess >>= (pure . show)
          else readProcess execFile [] ""
  where
    fnDefs = moduleFnDefs pikaModule
    layouts = moduleLayouts pikaModule

    mkConvertedTests :: [Test Expr] -> PikaConvert Quiet [Test PikaCore.Expr]
    mkConvertedTests =
        ((traversed . testExpr)
          %%~
            convertExprAndSimplify [])

    convertedLayouts = map (runIdentity . runPikaConvert layouts [] [] . convertLayout) layouts

    -- Go via SuSLik to SuSLang then C
    suslangConvert :: [(String, Type)] -> FnDef -> IO [CFunction]
    suslangConvert fnSigs fnDef = do
      pikaCore <- getPikaCore fnSigs fnDef
      let layoutPreds = map (codeGenLayout False) convertedLayouts
          fnIndPred = codeGenIndPred pikaCore
          fnSigAttempts = map (`codeGenFnSig` pikaCore) possibleIndirectionLevels
      invokeSuSLikAttempts [] (fnIndPred : layoutPreds) [] fnSigAttempts >>= \case
        Left err -> error $ "SuSLik error: " ++ err
        Right susLang ->
          pure $ concatMap functionToC susLang

    getPikaCore :: [(String, Type)] -> FnDef -> IO PikaCore.FnDef
    getPikaCore fnSigs fnDef
      -- | _optSimplifierLog opts =
      --     runLogIO $ toPikaCore fuel (moduleLayouts pikaModule) (moduleFnDefs pikaModule) fnDef
      | otherwise =
          pure . runQuiet $ toPikaCore fuel (moduleLayouts pikaModule) fnSigs fnDef

    generateFn fnSigs = getPikaCore fnSigs . moduleLookupFn pikaModule

    convertSynths :: Synth -> [Synth] -> IO [CFunction]
    convertSynths synth otherSynths = do
      let (layoutPreds, fnSig) = SuSLik.codeGenSynth convertedLayouts synth
          otherFnSigs = map (snd . SuSLik.codeGenSynth convertedLayouts) otherSynths

      -- mapM_ (putStrLn . ppr') layoutPreds
      -- putStrLn $ ppr' fnSig

      invokeSuSLik [] layoutPreds otherFnSigs fnSig >>= \case
        Left err ->
          error $
            unlines
              ["SuSLik error: When trying to synthesize"
              ,unlines $ map ppr' layoutPreds
              ,ppr' fnSig
              ,"Got the error: " ++ err
              ]
        Right susLang ->
          pure $ concatMap functionToC susLang


