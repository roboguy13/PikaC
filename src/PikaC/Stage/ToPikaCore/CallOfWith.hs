--
-- f ... (with { x ...} := e in e2) ...
--
--   ===>
--
-- with {x ...} := e
-- in
-- f ... e2 ...
--

module PikaC.Stage.ToPikaCore.CallOfWith
  (callOfWith)
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

import Debug.Trace

callOfWith :: Logger m => Expr -> SimplifyM m Expr
callOfWith = step "callOfWith" $ rewriteM go

go :: Fresh m => Expr -> m (Maybe Expr)
go (App f sz args) = do
  (withs, newArgs) <- getAndReplaceWiths args

  case withs of
    [] -> pure Nothing
    _ ->
      -- let newArgs = instUsing $ zip withs newArgsBnd
      -- in
      let r = buildWithIns withs (App f sz newArgs)
      in
      pure . Just $ r
go _ = pure Nothing

getAndReplaceWiths :: Fresh m => [Expr] -> m ([(Expr, [ModedName Expr])], [Expr])
getAndReplaceWiths [] = pure ([], [])
getAndReplaceWiths (WithIn e bnd : xs) = do
  (vars, body) <- unbind bnd
  (withs, rest) <- getAndReplaceWiths xs

  pure ((e, vars) : withs, body : rest)

getAndReplaceWiths (e : xs) = do
  (withs, rest) <- getAndReplaceWiths xs
  pure (withs, e : rest)

