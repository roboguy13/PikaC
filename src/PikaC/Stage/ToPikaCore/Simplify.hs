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
simplifyFnDef fn =
  -- trace ("simplifying " ++ ppr' fn) $

  (onFnDef layoutToWith <=<
  (pure . renameResultLayout) <=<
  (pure . substWithLayoutVar) <=<
  withOfWith <=<
  simplifyNestedCalls) fn

onFnDef :: Fresh m => (Expr -> m Expr) -> FnDef -> m FnDef
onFnDef f =
  (fnDefBranches.traverse.fnDefBranchBody) %%~ f

