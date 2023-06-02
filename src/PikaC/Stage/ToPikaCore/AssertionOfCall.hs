--
-- layout { } { ... ** (x+2) :-> with {y ...} := e1 in e2 ** ... }
--
--      ==>
--
-- with {y ...} := e1
-- in layout { } { ... ** (x+2) :-> e2 ** ... }
--
--

module PikaC.Stage.ToPikaCore.AssertionOfCall
  (assertionOfCall)
  where

import PikaC.Syntax.PikaCore.Expr
import PikaC.Syntax.PikaCore.FnDef
import PikaC.Syntax.Heaplet
import PikaC.Syntax.Pika.Layout

import PikaC.Utils

import PikaC.Stage.ToPikaCore.Utils
import PikaC.Stage.ToPikaCore.SimplifyM

import Control.Lens

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Bind

assertionOfCall :: Logger m => FnDef -> SimplifyM m FnDef
assertionOfCall = step "assertionOfCall" $ onFnDef (rewriteM go)

go :: Fresh m => Expr -> m (Maybe Expr)
go (SslAssertion bnd) = do
    (vars, asns) <- unbind bnd
    (withIns, newAsns) <- getAndReplaceWiths asns

    if null withIns
      then pure Nothing
      else pure $ Just $ buildWithIns withIns (SslAssertion (bind vars newAsns))
go e = pure Nothing

getAndReplaceWiths :: Fresh m =>
  ExprAssertion ->
  m ([(Expr, [ModedName Expr])], ExprAssertion)
getAndReplaceWiths [] = pure ([], [])
getAndReplaceWiths ((lhs :-> WithIn e bnd) : ps) = do
  (restWiths, restAsn) <- getAndReplaceWiths ps
  (vars, body) <- unbind bnd

  pure ((e, vars) : restWiths, (lhs :-> body) : restAsn)
getAndReplaceWiths (p : ps) = do
  (restWiths, restAsn) <- getAndReplaceWiths ps
  pure (restWiths, p : restAsn)

