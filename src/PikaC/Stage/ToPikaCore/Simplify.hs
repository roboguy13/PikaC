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

propPreserves_valid :: (FnDef -> FreshM FnDef) -> Property
propPreserves_valid pass =
  forAllShrinkShow genValidFnDef shrink ppr' $ \fnDef ->
    let result = runFreshM (pass fnDef)
    in
    case prettyValidate result of
      -- Left msg -> counterexample ("Counterexample result:\n" ++ ppr' result) False
      Left msg -> counterexample ("Counterexample result:\n" ++ msg) False
      Right _ -> property True

prop_genValidFnDef_sane :: Property
prop_genValidFnDef_sane =
  withMaxSuccess 5000 $ propPreserves_valid pure

prop_valid_reuseExistingPtrs :: Property
prop_valid_reuseExistingPtrs =
  withMaxSuccess 5000 $ propPreserves_valid reuseExistingPtrs

prop_valid_replaceClosedAssertions :: Property
prop_valid_replaceClosedAssertions =
  withMaxSuccess 5000 $ propPreserves_valid replaceClosedAssertions

prop_valid_simplifyNestedCalls :: Property
prop_valid_simplifyNestedCalls =
  withMaxSuccess 5000 $ propPreserves_valid simplifyNestedCalls

prop_valid_callOfWith :: Property
prop_valid_callOfWith = 
  withMaxSuccess 5000 $ propPreserves_valid callOfWith

prop_valid_withOfWith :: Property
prop_valid_withOfWith =
  withMaxSuccess 5000 $ propPreserves_valid withOfWith

prop_valid_substWithLayoutVar :: Property
prop_valid_substWithLayoutVar =
  withMaxSuccess 5000 $ propPreserves_valid substWithLayoutVar

prop_valid_withSubst :: Property
prop_valid_withSubst =
  withMaxSuccess 5000 $ propPreserves_valid withSubst

prop_valid_layoutToWith :: Property
prop_valid_layoutToWith =
  withMaxSuccess 5000 $ propPreserves_valid (onFnDef layoutToWith)

prop_valid_renameResultLayout :: Property
prop_valid_renameResultLayout =
  withMaxSuccess 5000 $ propPreserves_valid renameResultLayout

prop_valid_simplifyFnDef :: Property
prop_valid_simplifyFnDef =
  withMaxSuccess 5000 $ propPreserves_valid simplifyFnDef

testFn :: FnDef
testFn =
  FnDef
  { _fnDefName = FnName "a"
  , _fnDefBranches =
      let ljnh = string2Name "ljnh"
          pn = string2Name "pn"
          pn1 = string2Name "pn1"
          pn2 = string2Name "pn2"
      in
      bind [Moded In ljnh]
        $ bind [Moded Out pn]
          [ FnDefBranch
            { _fnDefBranchInputAssertions =
                [[ (V ljnh :+ 5) :-> V ljnh
                ]]
            , _fnDefBranchBody =
                WithIn
                  (V ljnh)
                  (bind [Moded Out pn1]
                    $ WithIn (V pn1)
                        $ bind [Moded Out pn2]
                            $ SslAssertion
                                $ bind []
                                    [(V pn2 :+ 3) :-> V pn2])
            }
          ]
  }

return []
checkAllProps :: IO Bool
checkAllProps = do
  print (map fst $(allProperties))
  $(quickCheckAll)
  -- $(verboseCheckAll)

