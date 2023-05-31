--
--  f ... layout {x ...} { ... } ...
--
--    ===>
--
--  with {y ...} := layout {x ...} {x ...}
--  in
--  f ... {y ...} ...
--

{-# LANGUAGE ScopedTypeVariables #-}

module PikaC.Stage.ToPikaCore.LayoutToWith
  (layoutToWith)
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

layoutToWith :: Logger m => FnDef -> SimplifyM m FnDef
layoutToWith = step "layoutToWith" $ onFnDef layoutToWith'

layoutToWith' :: forall m. Fresh m => Expr -> m Expr
layoutToWith' = rewriteM go
  where
    go :: Expr -> m (Maybe Expr)
    go (App f sz args) = do
      (asns, newArgs) <- getAndReplaceAsns args

      case asns of
        [] -> pure Nothing
        _ -> pure . Just $ mkWithIns asns (App f sz newArgs)

    go _ = pure Nothing

mkWithIns :: [(LayoutArg Expr, ExprAssertion, LayoutArg Expr)] -> Expr -> Expr
mkWithIns [] e = e
mkWithIns ((vars, rhs, newVars) : xs) e =
  WithIn
    (SslAssertion (bind (map (Moded Out) vars) rhs))
    $ bind (map (Moded Out) newVars)
      (mkWithIns xs e)

getAndReplaceAsns :: Fresh m => [Expr] -> m ([(LayoutArg Expr, ExprAssertion, LayoutArg Expr)], [Expr])
getAndReplaceAsns [] = pure ([], [])
getAndReplaceAsns (SslAssertion bnd:xs)
  | B vs _ <- bnd
  , not (null vs) = do
      (arg, e) <- unbind bnd
      (asns, rest) <- getAndReplaceAsns xs

      freshArgs <- mapM fresh (map modedNameName arg)
      let arg' = argExpr freshArgs
          -- allocs =
          --   map allocSize $
          --   findAllocations (map modedNameName arg) e

      pure ((map modedNameName arg, e, freshArgs) : asns, arg' : rest)

getAndReplaceAsns (e : xs) = do
  (asns, rest) <- getAndReplaceAsns xs
  pure (asns, e : rest)

