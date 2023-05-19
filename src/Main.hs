module Main
  where

import PikaC.Syntax.Pika.Expr
import PikaC.Syntax.Pika.FnDef
import PikaC.Syntax.Pika.Parser

import PikaC.Syntax.ParserUtils

import PikaC.Stage.ToPikaCore

import PikaC.Backend.C.CodeGen

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
      print pikaModule

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
  let pikaCore = toPikaCore (moduleLayouts pikaModule) $ moduleLookupFn pikaModule fnName
  putStrLn $ ppr' pikaCore

  putStrLn "- C:"
  putStrLn $ ppr' $ codeGenFn pikaCore

