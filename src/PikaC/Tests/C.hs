{-# LANGUAGE TemplateHaskell #-}

module PikaC.Tests.C
  where

import PikaC.Backend.C.CodeGen
import PikaC.Stage.ToPikaCore
import PikaC.Stage.ToPikaCore.SimplifyM
import PikaC.Syntax.Pika.Parser
import PikaC.Syntax.Type

import qualified PikaC.Syntax.PikaCore.Expr as PikaCore
import qualified PikaC.Syntax.PikaCore.FnDef as PikaCore

import PikaC.Syntax.Pika.FnDef (getFnTypeSig, overTypedBranches)

import PikaC.Ppr

import Data.Validity
import Test.QuickCheck

import Control.DeepSeq

prop_toPikaCore_genC_wellScoped :: Property
prop_toPikaCore_genC_wellScoped =
  withMaxSuccess 700 $
  forAllShrinkShow genModule shrink pprIt $ \pikaModule ->
    within 2000000 $ -- 2 seconds
    let pikaCore = moduleToPikaCore pikaModule
        cFn = codeGenFn pikaCore
    in
    case pikaModule `deepseq` prettyValidation (validate cFn) of
      Just msg -> counterexample ("++ Counterexample input:\n" ++ ppr' pikaModule ++ "\n++ Counterexample result:\n" ++ msg) False
      Nothing -> property True

-- pprIt :: (PikaModule, PikaCore.FnDef) -> String
pprIt :: PikaModule -> String
pprIt pikaModule =
  unlines
    ["> Module:"
    ,ppr' pikaModule
    ,"> PikaCore:"
    ,ppr' $ moduleToPikaCore pikaModule
    ]

moduleToPikaCore :: PikaModule -> PikaCore.FnDef
moduleToPikaCore pikaModule = 
  let (fnName:_) = moduleGenerates pikaModule
  in
  (runQuiet $ toPikaCore Unlimited (moduleLayouts pikaModule) (map (getFnTypeSig . overTypedBranches fromTypeSig_unsafe) (moduleFnDefs pikaModule)) $ overTypedBranches fromTypeSig_unsafe $ moduleLookupFn pikaModule fnName)

return []
checkAllProps :: IO Bool
checkAllProps =
  $(quickCheckAll)

