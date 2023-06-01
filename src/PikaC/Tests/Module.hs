{-# LANGUAGE TemplateHaskell #-}

module PikaC.Tests.Module
  where

import PikaC.Syntax.Pika.Expr
import PikaC.Syntax.Pika.Pattern
import PikaC.Syntax.Pika.Layout
import PikaC.Syntax.Pika.Parser

import PikaC.Syntax.PikaCore.FnDef
import PikaC.Syntax.Heaplet

import PikaC.Stage.ToPikaCore.SimplifyM
import PikaC.Stage.ToPikaCore

import PikaC.Ppr

import qualified PikaC.Syntax.PikaCore.Expr as PikaCore

import Test.QuickCheck

import Data.Validity

import Control.Exception
import System.IO
import System.Exit

-- | Converts to "basic args form"
prop_basicArgs_toPikaCore :: Property
prop_basicArgs_toPikaCore =
  -- forAllShrinkShow genModule (const []) show $ \pikaModule ->
  withMaxSuccess 700 $
  forAllShrinkShow genModule shrink ppr' $ \pikaModule ->
    let (fnName:_) = moduleGenerates pikaModule
        pikaCore = runQuiet $ toPikaCore Unlimited (moduleLayouts pikaModule) (moduleFnDefs pikaModule) $ moduleLookupFn pikaModule fnName
    in
    case prettyValidation (validateFnDefWith PikaCore.exprBasicArgs pikaCore) of
      Just msg -> counterexample ("++ Counterexample input:\n" ++ ppr' pikaModule ++ "\n++ Counterexample result:\n" ++ msg) False
      Nothing -> property True
  -- where
  --   go pikaModule x = catch x $ \(SomeException e) -> do
  --     hFlush stdout
  --     putStrLn $
  --       unlines
  --         ["Caught an exception during QuickCheck: "
  --         ,"Module was: " ++ show pikaModule
  --         ,"\n+++ Layouts:"
  --         , unlines $ map ppr' (moduleLayouts pikaModule)
  --         , "\n+++ Functions:"
  --         , unlines $ map ppr' (moduleFnDefs pikaModule)
  --         , "exception was: " ++ show e
  --         ]
  --     hFlush stdout
  --     exitFailure

return []
checkAllProps :: IO ()
checkAllProps = do
  -- quickCheckWith
  --   (stdArgs { chatty = False })
  --   prop_basicArgs_toPikaCore
  $(quickCheckAll)
  pure ()

