{-# LANGUAGE TemplateHaskell #-}

module PikaC.Stage.ToPikaCore.Simplify
  (simplifyFnDef
  ,simplifyExpr
  ,checkAllProps
  )
  where

import Control.Monad

import PikaC.Syntax.PikaCore.Expr
import PikaC.Syntax.PikaCore.FnDef
import PikaC.Syntax.Heaplet
import PikaC.Syntax.Pika.Layout

import PikaC.Utils

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
import PikaC.Stage.ToPikaCore.CallOfCall
import PikaC.Stage.ToPikaCore.AssertionOfCall
import PikaC.Stage.ToPikaCore.AssertionOfAssertion

import PikaC.Stage.ToPikaCore.SimplifyM

import PikaC.Stage.ToPikaCore.Utils

import Unbound.Generics.LocallyNameless

import PikaC.Ppr

import Control.Lens

import Debug.Trace

import Test.QuickCheck
import Data.Validity

simplifyFnDef :: Logger m => FnDef -> SimplifyM m FnDef
simplifyFnDef =
  fixedPoint
    (
    -- reuseExistingPtrs <=<
    onFnDef simplifyExprStep
    )
  -- renameResultLayout <=< -- NOTE: This should go last
    -- .
  -- myTraceWith (("simplifying " ++) . ppr')

simplifyExpr :: Logger m => Expr -> SimplifyM m Expr
simplifyExpr = fixedPoint simplifyExprStep

simplifyExprStep :: Logger m => Expr -> SimplifyM m Expr
simplifyExprStep =
    assertionOfAssertion <=<
    assertionOfCall <=<
    replaceClosedAssertions <=<
    callOfWith <=<
    layoutToWith <=<
    -- withLayoutV <=< -- TODO: This doesn't seem to work
    withOfWith <=<
    withSubst <=<
    simplifyNestedCalls <=<
    callOfCall <=<
    substWithLayoutVar

myTraceWith :: (a -> String) -> a -> a
myTraceWith f x = trace (f x) x

-- fixedPoint :: Fresh m => (FnDef -> m FnDef) -> FnDef -> m FnDef
-- fixedPoint f x = do
--   y <- f x
--   if aeq y x
--     then pure y
--     else fixedPoint f y

--
-- Property testing --
--

propPreserves_validation ::
  (Expr -> Validation) ->
  (FnDef -> Validation) ->
  (forall m. Logger m => FnDef -> SimplifyM m FnDef) ->
  Property
propPreserves_validation precond v pass =
  let precondFnDef = validateFnDefWith precond
      gen = genValidFnDef `suchThat` (validationIsValid . precondFnDef)
  in
  -- forAllShrinkShow gen (filter (validationIsValid . precondFnDef) . shrink) ppr' $ \fnDef ->
  forAllShrinkShow gen (const mempty) ppr' $ \fnDef ->
  -- forAllShrinkShow gen (const []) ppr' $ \fnDef ->
  -- forAllShow gen ppr' $ \fnDef ->
    -- let result = runSimplifyQuiet Unlimited pass fnDef
    let result = runErrorLog $ runSimplifyFn Unlimited pass fnDef
    in
    case result `seq` prettyValidation (v result) of
      -- Left msg -> counterexample ("Counterexample result:\n" ++ ppr' result) False
      Just msg -> counterexample ("Counterexample result:\n" ++ msg) False
      Nothing -> property True


propPreserves_basicArgs :: (forall m. Logger m => FnDef -> SimplifyM m FnDef) -> Property
propPreserves_basicArgs =
  propPreserves_validation
    exprBasicArgs
    (validateFnDefWith exprBasicArgs)

propPreserves_valid :: (forall m. Logger m => FnDef -> SimplifyM m FnDef) -> Property
-- propPreserves_valid = propPreserves_validation (wellScoped ([] :: [ExprName])) validate
propPreserves_valid = propPreserves_validation validate validate

  -- forAllShrinkShow genValidFnDef shrink ppr' $ \fnDef ->
  --   let result = runSimplifyQuiet Unlimited pass fnDef
  --   in
  --   case prettyValidate result of
  --     -- Left msg -> counterexample ("Counterexample result:\n" ++ ppr' result) False
  --     Left msg -> counterexample ("Counterexample result:\n" ++ msg) False
  --     Right _ -> property True

prop_genValidFnDef_sane :: Property
prop_genValidFnDef_sane =
  withMaxSuccess 1000 $ propPreserves_valid pure

-- prop_valid_reuseExistingPtrs :: Property
-- prop_valid_reuseExistingPtrs =
--   withMaxSuccess 1000 $ propPreserves_valid reuseExistingPtrs

prop_valid_replaceClosedAssertions :: Property
prop_valid_replaceClosedAssertions =
  withMaxSuccess 1000 $ propPreserves_valid (onFnDef replaceClosedAssertions)

prop_valid_simplifyNestedCalls :: Property
prop_valid_simplifyNestedCalls =
  withMaxSuccess 1000 $ propPreserves_valid (onFnDef simplifyNestedCalls)

prop_valid_callOfWith :: Property
prop_valid_callOfWith = 
  withMaxSuccess 1000 $ propPreserves_valid (onFnDef callOfWith)

prop_valid_withOfWith :: Property
prop_valid_withOfWith =
  withMaxSuccess 1000 $ propPreserves_valid (onFnDef withOfWith)

prop_valid_substWithLayoutVar :: Property
prop_valid_substWithLayoutVar =
  withMaxSuccess 1000 $ propPreserves_valid (onFnDef substWithLayoutVar)

prop_valid_withSubst :: Property
prop_valid_withSubst =
  withMaxSuccess 1000 $ propPreserves_valid (onFnDef withSubst)

prop_valid_layoutToWith :: Property
prop_valid_layoutToWith =
  withMaxSuccess 1000 $ propPreserves_valid (onFnDef layoutToWith)

prop_valid_renameResultLayout :: Property
prop_valid_renameResultLayout =
  withMaxSuccess 1000 $ propPreserves_valid renameResultLayout

prop_valid_assertionOfCall :: Property
prop_valid_assertionOfCall =
  withMaxSuccess 1000 $ propPreserves_valid (onFnDef assertionOfCall)

prop_valid_assertionOfAssertion :: Property
prop_valid_assertionOfAssertion =
  withMaxSuccess 1000 $ propPreserves_valid (onFnDef assertionOfAssertion)

prop_valid_simplifyFnDef :: Property
prop_valid_simplifyFnDef =
  withMaxSuccess 1000 $ propPreserves_valid simplifyFnDef

prop_preserves_basicArgs_simplifyFnDef :: Property
prop_preserves_basicArgs_simplifyFnDef =
  withMaxSuccess 2000 $ propPreserves_basicArgs simplifyFnDef

prop_simplified_simplifyFnDef :: Property
prop_simplified_simplifyFnDef =
  withMaxSuccess 5000 $
    propPreserves_validation
      exprBasicArgs
      (validateFnDefWith exprIsSimplified)
      simplifyFnDef

-- testFn :: FnDef
-- testFn =
--   FnDef
--   { _fnDefName = FnName "a"
--   , _fnDefBranches =
--       let ex = string2Name "ex"
--           ex1 = string2Name "ex1"
--       in
--       bind [Moded In ex]
--         $ bind [Moded Out ex]
--           [ FnDefBranch
--             { _fnDefBranchInputAssertions =
--                 [[ (V ex :+ 5) :-> V ex1
--                 ]]
--             , _fnDefBranchBody =
--                 App (FnName "a") []
--                   [ SslAssertion $ bind [] [((V ex :+ 0) :-> IntLit (-1))]
--                   ]
--             , _fnDefBranchInAllocs = [Alloc ex1 1]
--             }
--           ]
--   }

-- FnDef {_fnDefName = FnName "a", _fnDefBranches = <[Moded In mcu]> <[Moded Out xt]
-- > [FnDefBranch {_fnDefBranchInputAssertions = [[(V 0@0 :+ 5) :-> V 1@0]], _fnDefB
-- ranchInAllocs = [], _fnDefBranchBody = App (FnName "a") [] [App (FnName "a") [] [
-- ]]}]}


-- FnDef {_fnDefName = FnName "a", _fnDefBranches = <[Moded In sxpz]> <[Moded Out x]
-- > [FnDefBranch {_fnDefBranchInputAssertions = [[(V 1@0 :+ 2) :-> V x1]], _fnDefBr
-- anchInAllocs = [], _fnDefBranchBody = App (FnName "a") [1] [WithIn (V 0@0) (<[Mod
-- ed Out x1]> App (FnName "a") [1] [SslAssertion (<[]> [(V 3@0 :+ 2) :-> V 1@0])])]
-- }]}

-- testFnDef2 :: FnDef
-- testFnDef2 =
--   FnDef
--     { _fnDefName = FnName "a"
--     , _fnDefBranches =
--         let sxpz = string2Name "sxpz"
--             x = string2Name "x"
--             x1 = string2Name "x1"
--         in
--         bind [Moded In sxpz]
--         $ bind [Moded Out x]
--         [ FnDefBranch
--           { _fnDefBranchInputAssertions =
--               [ [(V sxpz :+ 2) :-> V x1]
--               ]
--           , _fnDefBranchInAllocs = []
--           , _fnDefBranchBody =
--               App (FnName "a") [1]
--               [WithIn
--                 (V sxpz)
--                 $ bind [Moded Out x1]
--                   (App (FnName "a")
--                     [1]
--                     [SslAssertion
--                       $ bind []
--                         [ (V sxpz :+ 2) :-> V x
--                         ]
--                     ]
--                   )
--               ]
--           }
--         ]
--     }

-- testFnDef' :: FnDef
-- testFnDef' =
--   FnDef
--     { _fnDefName = FnName "a"
--     , _fnDefBranches =
--         let mcu = string2Name "mcu"
--             xt = string2Name "xt"
--         in
--         bind [Moded In mcu]
--         $ bind [Moded Out xt]
--             [ FnDefBranch
--               { _fnDefBranchInputAssertions =
--                   [[(V mcu :+ 5) :-> V xt
--                   ]]
--               , _fnDefBranchInAllocs = []
--               , _fnDefBranchBody =
--                   App (FnName "a") []
--                     [ App (FnName "a")
--                         []
--                         [IntLit 1]
--                     ]
--               }
--             ]
--     }

return []
checkAllProps :: IO Bool
checkAllProps =
  $(quickCheckAll)
  -- $(verboseCheckAll)

