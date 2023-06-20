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

genAndRun :: SimplifyFuel -> String -> PikaModule -> IO String
genAndRun fuel compiler pikaModule = do
      -- Generate C file
  bracket (openTempFile "temp" "tests.c")
      (\(fileName, handle) -> do
        hClose handle
        removeFile fileName)
    $ \(fileName, handle) -> do
      hPutStrLn handle =<< readFile "tests/common/common.h"

      hPutStrLn handle . render . pprLayoutPrinters $ convertedLayouts

      (hPutStrLn handle . render . pprGenFns) =<< mapM generateFn (moduleGenerates pikaModule)

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

    getPikaCore :: FnDef -> IO PikaCore.FnDef
    getPikaCore fnDef
      -- | _optSimplifierLog opts =
      --     runLogIO $ toPikaCore fuel (moduleLayouts pikaModule) (moduleFnDefs pikaModule) fnDef
      | otherwise =
          pure . runQuiet $ toPikaCore fuel (moduleLayouts pikaModule) (moduleFnDefs pikaModule) fnDef

    generateFn = getPikaCore . moduleLookupFn pikaModule

