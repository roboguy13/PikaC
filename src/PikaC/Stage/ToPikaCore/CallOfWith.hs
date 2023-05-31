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

callOfWith :: Logger m => FnDef -> SimplifyM m FnDef
callOfWith = step "callOfWith" $ onFnDef (rewriteM go)

go :: Fresh m => Expr -> m (Maybe Expr)
go (App f sz args) = do
  (withs, newArgs) <- getAndReplaceWiths args

  case withs of
    [] -> pure Nothing
    _ ->
      -- let newArgs = instUsing $ zip withs newArgsBnd
      -- in
      let r = mkWithIns withs (App f sz newArgs)
      in
      pure . Just $ r
go _ = pure Nothing

mkWithIns :: [(Expr, [ModedName Expr])] -> Expr -> Expr
mkWithIns [] e = e
mkWithIns ((rhs, vars) : xs) e =
  WithIn
    rhs
    (bind vars (mkWithIns xs e))
    -- (B vars (mkWithIns xs e))

-- instUsing :: [((Expr, [ModedName Expr]), Bind [ModedName Expr] Expr)] -> [Expr]
-- instUsing [] = []
-- instUsing (((e, vars), bnd) : xs) =
--   instantiate bnd (map (V . modedNameName) vars) : instUsing xs

getAndReplaceWiths :: Fresh m => [Expr] -> m ([(Expr, [ModedName Expr])], [Expr])
getAndReplaceWiths [] = pure ([], [])
getAndReplaceWiths (WithIn e bnd : xs) = do
  (vars, body) <- unbind bnd
  (withs, rest) <- getAndReplaceWiths xs

  pure ((e, vars) : withs, body : rest)

getAndReplaceWiths (e : xs) = do
  (withs, rest) <- getAndReplaceWiths xs
  pure (withs, e : rest)

