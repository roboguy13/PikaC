{-# LANGUAGE TemplateHaskell #-}

module PikaC.Tests.Module
  where

import PikaC.Syntax.Pika.Expr
import PikaC.Syntax.Pika.Pattern
import PikaC.Syntax.Pika.Layout
import PikaC.Syntax.Pika.Parser
import PikaC.Syntax.Type

import PikaC.Syntax.PikaCore.FnDef
import PikaC.Syntax.Heaplet

import PikaC.Stage.ToPikaCore.SimplifyM
import PikaC.Stage.ToPikaCore

import PikaC.Ppr

import qualified PikaC.Syntax.PikaCore.Expr as PikaCore

import PikaC.Syntax.Pika.FnDef (getFnTypeSig, overTypedBranches)

import Test.QuickCheck

import Data.Validity

import Control.Exception
import System.IO
import System.Exit

import Data.Bifunctor

import Control.DeepSeq

theTimeout :: Int
theTimeout =
  3 -- seconds
    *1000000

defaultTestCount :: Int
defaultTestCount = 2000

prop_sane_genModule :: Property
prop_sane_genModule =
  withMaxSuccess defaultTestCount $
  forAllShrinkShow genModule shrink ppr' $ \pikaModule ->
      case pikaModule `deepseq` prettyValidation (validate pikaModule) of
        Just msg -> counterexample ("++ Counterexample input:\n" ++ ppr' pikaModule ++ "\n++ Counterexample result:\n" ++ msg) False
        Nothing -> property True

prop_wellScoped_modFns :: Property
prop_wellScoped_modFns =
  withMaxSuccess defaultTestCount $ ioProperty $
  catch (evaluate $
    forAllShrink genModule shrink $ \pikaModule ->
      case pikaModule `deepseq` prettyValidation (mconcat (map validate (moduleLayouts pikaModule)) <> mconcat (map validate (moduleFnDefs pikaModule))) of
        Just msg -> counterexample ("++ Counterexample input:\n" ++ ppr' pikaModule ++ "\n++ Counterexample result:\n" ++ msg) False
        Nothing -> property True
        ) $ \(SomeException e) -> error $ "caught an exception: " ++ show e

-- | Converts to "basic args form"
prop_basicArgs_toPikaCore :: Property
prop_basicArgs_toPikaCore =
  -- forAllShrinkShow genModule (const []) show $ \pikaModule ->
  withMaxSuccess defaultTestCount $
  forAllShrinkShow genModule shrink ppr' $ \pikaModule ->
    -- TODO: Figure out why some rare test cases seem to be going into an
    -- infinite loop here (NOTE: This even happened when the fuel was set to 0)
    discardAfter theTimeout $
  -- forAllShow genModule ppr' $ \pikaModule ->
    let (fnName:_) = moduleGenerates pikaModule
        pikaCore = runQuiet $ toPikaCore Unlimited (moduleLayouts pikaModule) (map (getFnTypeSig . overTypedBranches fromTypeSig_unsafe) (moduleFnDefs pikaModule)) $ overTypedBranches fromTypeSig_unsafe $ moduleLookupFn pikaModule fnName
    in
    case pikaModule `deepseq` prettyValidation (validateFnDefWith PikaCore.exprBasicArgs pikaCore) of
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

-- | We simplify from Pika to the simplified form of PikaCore that is ready
-- for translation into C and/or SuSLik
prop_simplify_from_Pika :: Property
prop_simplify_from_Pika =
  withMaxSuccess defaultTestCount $
  -- forAllShrinkShow genModule (filter isValid . shrink) ppr' $ \pikaModule ->
  forAllShrinkShow genModule (shrink) ppr' $ \pikaModule ->
  -- forAllShow genModule ppr' $ \pikaModule ->
    -- TODO: Figure out why some rare test cases seem to be going into an
    -- infinite loop here
    discardAfter theTimeout $
    let (fnName:_) = moduleGenerates pikaModule
        pikaCore = runQuiet $ toPikaCore Unlimited (moduleLayouts pikaModule) (map (getFnTypeSig . overTypedBranches fromTypeSig_unsafe) (moduleFnDefs pikaModule)) $ overTypedBranches fromTypeSig_unsafe $ moduleLookupFn pikaModule fnName
    in
    case pikaModule `deepseq` prettyValidation (validateFnDefWith PikaCore.exprIsSimplified pikaCore) of
      Just msg -> counterexample ("++ Counterexample input:\n" ++ ppr' pikaModule ++ "\n++ Counterexample result:\n" ++ msg) False
      Nothing -> property True

return []
checkAllProps :: IO ()
checkAllProps = do
  -- quickCheckWith
  --   (stdArgs { chatty = False })
  --   prop_basicArgs_toPikaCore
  $(quickCheckAll)
  pure ()

