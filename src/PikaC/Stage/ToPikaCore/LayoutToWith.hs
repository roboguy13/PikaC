--
--  f ... layout {x ...} { ... } ...
--
--    ===>
--
--  with {x ...} := layout {x ...} {x ...}
--  in
--  f ... {x ...} ...
--

{-# LANGUAGE ScopedTypeVariables #-}

module PikaC.Stage.ToPikaCore.LayoutToWith
  (layoutToWith)
  where

import PikaC.Syntax.PikaCore.Expr
import PikaC.Syntax.PikaCore.FnDef
import PikaC.Syntax.Heaplet

import PikaC.Utils

import Control.Lens

import Unbound.Generics.LocallyNameless

layoutToWith :: forall m. Fresh m => Expr -> m Expr
layoutToWith = rewriteM go
  where
    go :: Expr -> m (Maybe Expr)
    go (App f args) = do
      (asns, newArgs) <- getAndReplaceAsns args

      case asns of
        [] -> pure Nothing
        _ -> pure . Just $ mkWithIns asns (App f newArgs)

    go _ = pure Nothing

mkWithIns :: [(LayoutArg Expr, ExprAssertion)] -> Expr -> Expr
mkWithIns [] e = e
mkWithIns ((vars, rhs) : xs) e =
  WithIn
    (SslAssertion vars rhs)
    vars
    (mkWithIns xs e)

getAndReplaceAsns :: Fresh m => [Expr] -> m ([(LayoutArg Expr, ExprAssertion)], [Expr])
getAndReplaceAsns [] = pure ([], [])
getAndReplaceAsns (SslAssertion arg e:xs) = do
  (asns, rest) <- getAndReplaceAsns xs
  arg' <- argExpr arg
  pure ((arg, e) : asns, arg' : rest)
getAndReplaceAsns (e : xs) = do
  (asns, rest) <- getAndReplaceAsns xs
  pure (asns, e : rest)

argExpr :: Fresh m => LayoutArg Expr -> m Expr
argExpr [v] = V <$> fresh v
argExpr vs = LayoutV <$> mapM fresh vs

