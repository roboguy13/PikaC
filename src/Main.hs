module Main
  where

import PikaC.Syntax.Pika.Expr
import PikaC.Syntax.Pika.FnDef
import PikaC.Syntax.Pika.Parser

import PikaC.TypeChecker.Mode

import PikaC.Syntax.PikaCore.FnDef

import PikaC.Syntax.ParserUtils

import PikaC.Stage.ToPikaCore
import PikaC.Stage.ToPikaCore.SimplifyM

-- import PikaC.Backend.C.CodeGen

import PikaC.Ppr

import Control.Monad

import System.Environment
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      fileData <- readFile fileName

      let pikaModule = parse'' fileName parsePikaModule fileData
          layouts = moduleLayouts pikaModule

      mapM_ (putStrLn . ppr') (moduleLayouts pikaModule)

      case mapM_ (modeCheck layouts) layouts of
        Left e -> do
          putStrLn $ render $ text "Mode error:" <+> ppr e
          exitFailure
        Right () -> pure ()

      withModule pikaModule
    _ -> error "Wrong number of arguments. Expected 1"

withModule :: PikaModule -> IO ()
withModule pikaModule = do
  forM_ (moduleGenerates pikaModule) $ \fnName ->
    generateFn pikaModule fnName

generateFn :: PikaModule -> String -> IO ()
generateFn pikaModule fnName = do
  putStrLn $ "*** " <> fnName <> " ***"

  putStrLn "- PikaCore:"
  let pikaCore = runQuiet $ toPikaCore Unlimited (moduleLayouts pikaModule) (moduleFnDefs pikaModule) $ moduleLookupFn pikaModule fnName -- TODO: Add command line options to expose the simplifier options
  putStrLn $ ppr' pikaCore
  -- putStrLn $ show pikaCore

  -- putStrLn "- C:"
  -- putStrLn $ ppr' $ codeGenFn pikaCore

