module PikaC.Tests.C
  where

import PikaC.Backend.C.CodeGen
import PikaC.Stage.ToPikaCore
import PikaC.Stage.ToPikaCore.SimplifyM
import PikaC.Syntax.Pika.Parser

import qualified PikaC.Syntax.PikaCore.Expr as PikaCore
import qualified PikaC.Syntax.PikaCore.FnDef as PikaCore

import PikaC.Ppr

import Data.Validity
import Test.QuickCheck

import Control.DeepSeq

prop_toPikaCore_genC_wellScoped :: Property
prop_toPikaCore_genC_wellScoped =
  withMaxSuccess 700 $
  forAllShow (moduleToPikaCore <$> genModule) pprIt $ \(pikaModule, pikaCore) ->
    within 2000000 $ -- 2 seconds
    let cFn = codeGenFn pikaCore
    in
    case pikaModule `deepseq` prettyValidation (validate cFn) of
      Just msg -> counterexample ("++ Counterexample input:\n" ++ ppr' pikaModule ++ "\n++ Counterexample result:\n" ++ msg) False
      Nothing -> property True

pprIt :: (PikaModule, PikaCore.FnDef) -> String
pprIt (pikaModule, fn) =
  unlines
    ["> Module:"
    ,ppr' pikaModule
    ,"> PikaCore:"
    ,ppr' fn
    ]

moduleToPikaCore :: PikaModule -> (PikaModule, PikaCore.FnDef)
moduleToPikaCore pikaModule = 
  let (fnName:_) = moduleGenerates pikaModule
  in
  (pikaModule, runQuiet $ toPikaCore Unlimited (moduleLayouts pikaModule) (moduleFnDefs pikaModule) $ moduleLookupFn pikaModule fnName)
