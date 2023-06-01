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

-- | Converts to "basic args form"
prop_basicArgs_toPikaCore :: Property
prop_basicArgs_toPikaCore =
  -- forAllShrinkShow genModule (const []) show $ \pikaModule ->
  forAllShow genModule show $ \pikaModule -> ioProperty $ do
    let (fnName:_) = moduleGenerates pikaModule
    pikaCore <- go pikaModule $ evaluate $ runQuiet $ toPikaCore Unlimited (moduleLayouts pikaModule) (moduleFnDefs pikaModule) $ moduleLookupFn pikaModule fnName
    case prettyValidation (validateFnDefWith PikaCore.exprBasicArgs pikaCore) of
      Just msg -> pure $ counterexample ("Counterexample result:\n" ++ msg) False
      Nothing -> pure $ property True
  where
    go pikaModule x = catch x $ \(SomeException e) -> do
      putStrLn $ "Caught exception: " ++ show e
      putStrLn $ "Module was: " ++ show pikaModule

      putStrLn $ "\n=== Layouts:\n"
      mapM_ (putStrLn . ppr') (moduleLayouts pikaModule)

      putStrLn $ "\n=== Functions:\n"
      mapM_ (putStrLn . ppr') (moduleFnDefs pikaModule)
      x

return []
checkAllProps :: IO Bool
checkAllProps =
  $(quickCheckAll)

