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

import PikaC.Ppr

import Control.Lens

import Unbound.Generics.LocallyNameless

import Debug.Trace

withSubst :: Logger m => FnDef -> SimplifyM m FnDef
withSubst = step "withSubst" $ onFnDef (rewriteM go)

go :: (Logger m, Fresh m) => Expr -> m (Maybe Expr)
go (WithIn (LayoutV []) _) = pure Nothing
go e0@(WithIn (LayoutV vs) bnd) = do
  (vars, body) <- unbind bnd
  if length vs /= length vars
    then logError $ "withSubst: length vs /= length vars: " ++ show vs ++ " /= " ++ show vars ++ " in the expression: " ++ ppr' e0
    else pure . Just $ substs (zip (map modedNameName vars) vs) body

go e0@(WithIn (V v) bnd) = do
  (vars, body) <- unbind bnd
  let [var] = vars
  pure . Just $ subst (modedNameName var) (V v) body

go _ = pure Nothing

