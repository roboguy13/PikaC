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

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Unsafe

floatWith :: Logger m => Expr -> SimplifyM m Expr
floatWith = step "floatWith" $ rewriteM go

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
  -- vs' <- mapM (fresh . modedNameName) vs
  -- let renameIt = substs (zip (map modedNameName vs) (map V vs'))
  -- let 
  pure $ WithIn e $ bind vs (f (argExpr vsUnmoded))

