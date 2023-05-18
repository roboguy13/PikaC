-- | Replace
--     > with {x ... } := layout {y ... } { h ... } in {x ...}
--   by
--     > layout {y ...} {h ...}
--
-- TODO: Do we need to make sure none of x ... are already in scope?
module PikaC.Stage.ToPikaCore.SubstWithLayoutVar
  (substWithLayoutVar)
  where

import PikaC.Syntax.PikaCore.Expr
import PikaC.Syntax.PikaCore.FnDef

import Control.Lens

substWithLayoutVar :: FnDef -> FnDef
substWithLayoutVar =
  (fnDefBranches.traverse.fnDefBranchBody) %~ rewrite substWithLayoutVar'Expr

substWithLayoutVar'Expr :: Expr -> Maybe Expr
substWithLayoutVar'Expr (WithIn a@(SslAssertion {}) xs (LayoutV ys))
  | ys == xs = Just a
substWithLayoutVar'Expr _ = Nothing
  

