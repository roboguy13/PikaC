module PikaC.Tests.Pika.Golden
  where

import Test.Tasty
import Test.Tasty.Ingredients.ConsoleReporter
import Test.Tasty.Golden

import System.IO
import System.Environment

import Control.Monad

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.String

import System.FilePath

import PikaC.Tests.Pika.Run
import PikaC.Syntax.Pika.Parser
import PikaC.Syntax.ParserUtils
import PikaC.Stage.ToPikaCore.SimplifyM

testsPath :: FilePath
testsPath = "tests"

cCompiler :: String
cCompiler = "gcc"

main :: IO ()
main =
  let ingredients = consoleTestReporter : defaultIngredients
  in
  defaultMainWithIngredients ingredients =<< goldenTestTree

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

  fromString <$> genAndRun C Unlimited False cCompiler pikaModule

