{-# LANGUAGE TemplateHaskell #-}

module PikaC.Stage.ToPikaCore.Simplify
  (simplifyFnDef
  )
  where

import Control.Monad

import PikaC.Syntax.PikaCore.Expr
import PikaC.Syntax.PikaCore.FnDef
import PikaC.Syntax.Heaplet
import PikaC.Syntax.Pika.Layout

import PikaC.Stage.ToPikaCore.NestedCalls
import PikaC.Stage.ToPikaCore.WithOfWith
import PikaC.Stage.ToPikaCore.SubstWithLayoutVar
import PikaC.Stage.ToPikaCore.RenameResultLayout
import PikaC.Stage.ToPikaCore.LayoutToWith
import PikaC.Stage.ToPikaCore.WithLayoutV
import PikaC.Stage.ToPikaCore.CallOfWith
import PikaC.Stage.ToPikaCore.WithSubst
import PikaC.Stage.ToPikaCore.ReuseExistingPtrs
import PikaC.Stage.ToPikaCore.ReplaceClosedAssertions

import PikaC.Stage.ToPikaCore.Utils

import Unbound.Generics.LocallyNameless

import PikaC.Ppr

import Control.Lens

import Debug.Trace

import Test.QuickCheck
import Data.Validity

simplifyFnDef :: Fresh m => FnDef -> m FnDef
simplifyFnDef =
  renameResultLayout <=< -- NOTE: This should go last
  -- fixedPoint
    (
      reuseExistingPtrs <=<
      replaceClosedAssertions <=<
      callOfWith <=<
      onFnDef layoutToWith <=<
      -- withLayoutV <=< -- TODO: This doesn't seem to work
      withOfWith <=<
      withSubst <=<
      simplifyNestedCalls <=<
      substWithLayoutVar
    )
    -- .
  -- myTraceWith (("simplifying " ++) . ppr')

myTraceWith :: (a -> String) -> a -> a
myTraceWith f x = trace (f x) x

fixedPoint :: Fresh m => (FnDef -> m FnDef) -> FnDef -> m FnDef
fixedPoint f x = do
  y <- f x
  if aeq y x
    then pure y
    else fixedPoint f y

--
-- Property testing --
--

propPreserves_wellScoped :: (FnDef -> FreshM FnDef) -> Property
propPreserves_wellScoped pass =
  forAllShrinkShow genValidFnDef shrink ppr' $ \fnDef ->
    let result = runFreshM (pass fnDef)
    in
    case prettyValidate result of
      -- Left msg -> counterexample ("Counterexample result:\n" ++ ppr' result) False
      Left msg -> counterexample ("Counterexample result:\n" ++ msg) False
      Right _ -> property True

prop_genValidFnDef_sane :: Property
prop_genValidFnDef_sane =
  withMaxSuccess 5000 $ propPreserves_wellScoped pure

prop_wellScoped_reuseExistingPtrs :: Property
prop_wellScoped_reuseExistingPtrs =
  withMaxSuccess 5000 $ propPreserves_wellScoped reuseExistingPtrs

prop_wellScoped_replaceClosedAssertions :: Property
prop_wellScoped_replaceClosedAssertions =
  withMaxSuccess 5000 $ propPreserves_wellScoped replaceClosedAssertions

prop_wellScoped_simplifyNestedCalls :: Property
prop_wellScoped_simplifyNestedCalls =
  withMaxSuccess 5000 $ propPreserves_wellScoped simplifyNestedCalls

prop_wellScoped_callOfWith :: Property
prop_wellScoped_callOfWith = 
  withMaxSuccess 5000 $ propPreserves_wellScoped callOfWith

prop_wellScoped_withOfWith :: Property
prop_wellScoped_withOfWith =
  withMaxSuccess 5000 $ propPreserves_wellScoped withOfWith

prop_wellScoped_substWithLayoutVar :: Property
prop_wellScoped_substWithLayoutVar =
  withMaxSuccess 5000 $ propPreserves_wellScoped substWithLayoutVar

prop_wellScoped_withSubst :: Property
prop_wellScoped_withSubst =
  withMaxSuccess 5000 $ propPreserves_wellScoped withSubst

prop_wellScoped_layoutToWith :: Property
prop_wellScoped_layoutToWith =
  withMaxSuccess 5000 $ propPreserves_wellScoped (onFnDef layoutToWith)

prop_wellScoped_renameResultLayout :: Property
prop_wellScoped_renameResultLayout =
  withMaxSuccess 5000 $ propPreserves_wellScoped renameResultLayout

prop_wellScoped_simplifyFnDef :: Property
prop_wellScoped_simplifyFnDef =
  withMaxSuccess 350 $ propPreserves_wellScoped simplifyFnDef

testFn :: FnDef
testFn =
  FnDef
  { _fnDefName = FnName "a"
  , _fnDefBranches =
      let ak = string2Name "ak"
          a = string2Name "a"
          a1 = string2Name "a1"
      in
      bind [Moded In ak]
        $ bind [Moded Out a]
          [ FnDefBranch
            { _fnDefBranchInputAssertions =
                [[ (V ak :+ 1) :-> V ak
                ]]
            , _fnDefBranchBody =
                WithIn
                  (SslAssertion
                    (bind []
                      [ (V ak :+ 1) :-> V ak
                      ]))
                  (bind [Moded Out a1]
                    $ App (FnName "a")
                        [SslAssertion
                          (bind []
                            [ (V a :+ 5) :-> BoolLit True
                            ])
                        ,V a1
                        ])
            }
          ]
  }

return []
checkAllProps :: IO Bool
checkAllProps = do
  print (map fst $(allProperties))
  $(quickCheckAll)
  -- $(verboseCheckAll)

