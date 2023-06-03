--
-- with {x ...} := {y ...}
-- in
-- e
--
--    ===>
--
-- e[x/y, ...]
--

{-# LANGUAGE LambdaCase #-}

module PikaC.Stage.ToPikaCore.WithSubst
  (withSubst)
  where

import PikaC.Syntax.PikaCore.Expr
import PikaC.Syntax.PikaCore.FnDef
import PikaC.Syntax.Pika.Layout

import PikaC.Stage.ToPikaCore.Utils
import PikaC.Stage.ToPikaCore.SimplifyM

import Control.Lens

import Unbound.Generics.LocallyNameless

import Debug.Trace

withSubst :: Logger m => FnDef -> SimplifyM m FnDef
withSubst = step "withSubst" $ onFnDef (rewriteM go)

go :: Fresh m => Expr -> m (Maybe Expr)
go (WithIn (LayoutV []) _) = pure Nothing
go (WithIn (LayoutV vs) bnd) = do
  (vars, body) <- unbind bnd
  pure . Just $ substs (zip (map modedNameName vars) vs) body

go e0@(WithIn (V v) bnd) = do
  (vars, body) <- unbind bnd
  let [var] = vars
  pure . Just $ subst (modedNameName var) (V v) body

go _ = pure Nothing

