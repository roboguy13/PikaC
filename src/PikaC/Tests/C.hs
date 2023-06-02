module PikaC.Tests.C
  where

import PikaC.Backend.C.CodeGen
import PikaC.Stage.ToPikaCore
import PikaC.Stage.ToPikaCore.SimplifyM
import PikaC.Syntax.Pika.Parser

import PikaC.Ppr

import Data.Validity
import Test.QuickCheck

import Control.DeepSeq

prop_toPikaCore_genC_wellScoped :: Property
prop_toPikaCore_genC_wellScoped =
  withMaxSuccess 700 $
  forAllShow genModule ppr' $ \pikaModule ->
    within 2000000 $ -- 2 seconds
    let (fnName:_) = moduleGenerates pikaModule
        pikaCore = runQuiet $ toPikaCore Unlimited (moduleLayouts pikaModule) (moduleFnDefs pikaModule) $ moduleLookupFn pikaModule fnName
        cFn = codeGenFn pikaCore
    in
    case pikaModule `deepseq` prettyValidation (validate cFn) of
      Just msg -> counterexample ("++ Counterexample input:\n" ++ ppr' pikaModule ++ "\n++ Counterexample result:\n" ++ msg) False
      Nothing -> property True
