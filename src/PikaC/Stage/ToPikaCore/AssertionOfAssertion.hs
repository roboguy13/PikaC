--
-- layout {x ...} { ... ** x :-> layout {y ...} { ... } ** ... }
--
--   ==>
--
-- with {z ...} := layout {y ...} { .... }
-- in
-- layout {x ...} { ... ** x :-> z ** ... }
--

module PikaC.Stage.ToPikaCore.AssertionOfAssertion
  (assertionOfAssertion)
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

assertionOfAssertion :: Logger m => FnDef -> SimplifyM m FnDef
assertionOfAssertion = step "assertionOfAssertion" $ onFnDef (rewriteM go)

go :: Fresh m => Expr -> m (Maybe Expr)
go (SslAssertion bnd) = do
  (vars, asn) <- unbind bnd
  (innerAsns, newAsn) <- getAndReplaceAsns asn

  case innerAsns of
    [] -> pure Nothing
    _ ->
      pure $ Just $
      buildWithIns innerAsns
        (SslAssertion (bind vars newAsn))

go _ = pure Nothing

getAndReplaceAsns :: Fresh m =>
  ExprAssertion ->
  m ([(Expr, [ModedName Expr])], ExprAssertion)
getAndReplaceAsns [] = pure ([], [])
getAndReplaceAsns ((x :-> e@(SslAssertion bnd)) : xs) = do
  (vars, _) <- unbind bnd
  freshVars <- mapM freshModed vars
  let unmodedFreshVars = map modedNameName freshVars

  (innerAsns, restPointsTo) <- getAndReplaceAsns xs

  pure
    ((e, freshVars) : innerAsns
    ,(x :-> mkVars unmodedFreshVars) : restPointsTo)
getAndReplaceAsns (x : xs) = do
  (innerAsns, restPointsTo) <- getAndReplaceAsns xs
  pure (innerAsns, x : restPointsTo)


