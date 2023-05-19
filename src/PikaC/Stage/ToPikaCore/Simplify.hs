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

import Unbound.Generics.LocallyNameless

simplifyFnDef :: Fresh m => FnDef -> m FnDef
simplifyFnDef =
  (pure . renameResultLayout) <=<
  (pure . substWithLayoutVar) <=<
  withOfWith <=<
  simplifyNestedCalls

