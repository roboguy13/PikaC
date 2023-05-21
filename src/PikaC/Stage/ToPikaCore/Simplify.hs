module PikaC.Stage.ToPikaCore.Simplify
  (simplifyFnDef
  )
  where

import Control.Monad

import PikaC.Syntax.PikaCore.Expr
import PikaC.Syntax.PikaCore.FnDef

import PikaC.Stage.ToPikaCore.NestedCalls
import PikaC.Stage.ToPikaCore.WithOfWith
import PikaC.Stage.ToPikaCore.SubstWithLayoutVar
import PikaC.Stage.ToPikaCore.RenameResultLayout
import PikaC.Stage.ToPikaCore.LayoutToWith

import Unbound.Generics.LocallyNameless

import PikaC.Ppr

import Control.Lens

import Debug.Trace

simplifyFnDef :: Fresh m => FnDef -> m FnDef
simplifyFnDef =
  -- trace ("simplifying " ++ ppr' fn) $

  fixedPoint
    (onFnDef layoutToWith <=<
    (pure . renameResultLayout) <=<
    (pure . substWithLayoutVar) <=<
    withOfWith <=<
    simplifyNestedCalls)

fixedPoint :: Fresh m => (FnDef -> m FnDef) -> FnDef -> m FnDef
fixedPoint f x = do
  y <- f x
  if y == x
    then pure y
    else fixedPoint f y

onFnDef :: Fresh m => (Expr -> m Expr) -> FnDef -> m FnDef
onFnDef f =
  (fnDefBranches.traverse.fnDefBranchBody) %%~ f

