-- | Replace
--     > with {x ... } := layout {y ... } { h ... } in {x ...}
--   by
--     > layout {y ...} {h ...}
--
-- TODO: Do we need to make sure none of x ... are already in scope?

{-# LANGUAGE LambdaCase #-}

module PikaC.Stage.ToPikaCore.SubstWithLayoutVar
  (substWithLayoutVar)
  where

import PikaC.Syntax.PikaCore.Expr
import PikaC.Syntax.PikaCore.FnDef
import PikaC.Syntax.Pika.Layout

import PikaC.Stage.ToPikaCore.Utils
import PikaC.Stage.ToPikaCore.SimplifyM

import Control.Lens

import Unbound.Generics.LocallyNameless

import Debug.Trace

substWithLayoutVar :: Logger m => FnDef -> SimplifyM m FnDef
substWithLayoutVar =
  step "substWithLayoutVar" $ onFnDef (rewriteM substWithLayoutVar'Expr)
  -- (fnDefBranches.traverse.fnDefBranchBody) %~ rewrite substWithLayoutVar'Expr

substWithLayoutVar'Expr :: Fresh m => Expr -> m (Maybe Expr)
-- substWithLayoutVar'Expr (WithIn a@(SslAssertion {}) bnd) =
substWithLayoutVar'Expr (WithIn a bnd) =
  unbind bnd >>= \case
    (xs, LayoutV ys)
      | map getV ys == map modedNameName xs -> pure $ Just a
    (xs, V v)
      | map modedNameName xs == [v] -> pure $ Just a
    _ -> pure Nothing
substWithLayoutVar'Expr _ = pure Nothing

-- substWithLayoutVar'Expr :: Expr -> Maybe Expr
-- substWithLayoutVar'Expr (WithIn a@(SslAssertion {}) xs (LayoutV ys))
--   | ys == xs = Just a
-- substWithLayoutVar'Expr _ = Nothing
  

