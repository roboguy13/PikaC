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

import Control.Lens

import Unbound.Generics.LocallyNameless

withSubst :: Fresh m => FnDef -> m FnDef
withSubst = onFnDef (rewriteM go)

go :: Fresh m => Expr -> m (Maybe Expr)
go (WithIn (LayoutV vs) bnd) = do
  (vars, body) <- unbind bnd
  pure $ Just $ substs (zip (map getV vs) (map (V . modedNameName) vars)) body

go (WithIn (V v) bnd) = do
  (vars, body) <- unbind bnd
  pure $ Just $ substs (zip [v] (map (V . modedNameName) vars)) body

go _ = pure Nothing

