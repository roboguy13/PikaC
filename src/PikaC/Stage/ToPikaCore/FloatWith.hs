-- Float a with-in through basic builtin operators. For example:
--
--   x && (with {y} := ... in ...)
--
--      ==>
--
--   with {y} := ... in x && ...
module PikaC.Stage.ToPikaCore.FloatWith
  (floatWith)
  where

import PikaC.Syntax.PikaCore.Expr
import PikaC.Syntax.PikaCore.FnDef
import PikaC.Syntax.Heaplet
import PikaC.Syntax.Pika.Layout

import PikaC.Utils
import PikaC.Stage.ToPikaCore.Utils
import PikaC.Stage.ToPikaCore.SimplifyM

import Control.Lens
import Control.Monad

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Unsafe

floatWith :: Logger m => Expr -> SimplifyM m Expr
floatWith = step "floatWith" $ rewriteOne go -- <=< rewriteM goApplies

goApplies :: Fresh m => Expr -> m (Maybe Expr)
goApplies (Add (App f allocs es) y) = Just <$> goApply f allocs es (`Add` y)
goApplies (Add x (App f allocs es)) = Just <$> goApply f allocs es (x `Add`)
goApplies (Mul (App f allocs es) y) = Just <$> goApply f allocs es (`Mul` y)
goApplies (Mul x (App f allocs es)) = Just <$> goApply f allocs es (x `Mul`)
goApplies (Sub x (App f allocs es)) = Just <$> goApply f allocs es (x `Sub`)
goApplies (Sub (App f allocs es) y) = Just <$> goApply f allocs es (`Sub` y)
goApplies (Div (App f allocs es) y) = Just <$> goApply f allocs es (`Div` y)
goApplies (Div x (App f allocs es)) = Just <$> goApply f allocs es (x `Div`)
goApplies (Equal (App f allocs es) y) = Just <$> goApply f allocs es (`Equal` y)
goApplies (Equal x (App f allocs es)) = Just <$> goApply f allocs es (x `Equal`)
goApplies (Lt (App f allocs es) y) = Just <$> goApply f allocs es (`Lt` y)
goApplies (Lt x (App f allocs es)) = Just <$> goApply f allocs es (x `Lt`)
goApplies (Le (App f allocs es) y) = Just <$> goApply f allocs es (`Le` y)
goApplies (Le x (App f allocs es)) = Just <$> goApply f allocs es (x `Le`)
goApplies (Not (App f allocs es)) = Just <$> goApply f allocs es Not
goApplies (And (App f allocs es) y) = Just <$> goApply f allocs es (`And` y)
goApplies (And x (App f allocs es)) = Just <$> goApply f allocs es (x `And`)
goApplies _ = pure Nothing

goApply :: Fresh m => FnName -> [Int] -> [Expr] -> (Expr -> Expr) -> m Expr
goApply f allocs args op = do
  x <- fresh (string2Name "v")
  pure $ WithIn (App f allocs args) (bind [Moded Out x] (op (V x)))

  -- TODO: Find a nicer way
go :: Fresh m => Expr -> m (Maybe Expr)
go (Add (WithIn e bnd) y) = Just <$> goWith e bnd (`Add` y)
go (Add x (WithIn e bnd)) = Just <$> goWith e bnd (x `Add`)
go (Mul (WithIn e bnd) y) = Just <$> goWith e bnd (`Mul` y)
go (Mul x (WithIn e bnd)) = Just <$> goWith e bnd (x `Mul`)
go (Sub x (WithIn e bnd)) = Just <$> goWith e bnd (x `Sub`)
go (Sub (WithIn e bnd) y) = Just <$> goWith e bnd (`Sub` y)
go (Div (WithIn e bnd) y) = Just <$> goWith e bnd (`Div` y)
go (Div x (WithIn e bnd)) = Just <$> goWith e bnd (x `Div`)
go (Equal (WithIn e bnd) y) = Just <$> goWith e bnd (`Equal` y)
go (Equal x (WithIn e bnd)) = Just <$> goWith e bnd (x `Equal`)
go (Lt (WithIn e bnd) y) = Just <$> goWith e bnd (`Lt` y)
go (Lt x (WithIn e bnd)) = Just <$> goWith e bnd (x `Lt`)
go (Le (WithIn e bnd) y) = Just <$> goWith e bnd (`Le` y)
go (Le x (WithIn e bnd)) = Just <$> goWith e bnd (x `Le`)
go (Not (WithIn e bnd)) = Just <$> goWith e bnd Not
go (And (WithIn e bnd) y) = Just <$> goWith e bnd (`And` y)
go (And x (WithIn e bnd)) = Just <$> goWith e bnd (x `And`)
go _ = pure Nothing

goWith :: Fresh m => Expr -> Bind [ModedName Expr] Expr -> (Expr -> Expr) -> m Expr
goWith e bnd f = do
  (vs, body) <- unbind bnd
  let vsUnmoded = map modedNameName vs
  -- pure $ WithIn e $ bind vs (f (argExpr vsUnmoded))
  pure $ WithIn e $ bind vs (f body)

