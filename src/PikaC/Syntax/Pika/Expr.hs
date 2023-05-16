{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module PikaC.Syntax.Pika.Expr
  where

import PikaC.Syntax.Type

import Bound
import Text.Show.Deriving

import Control.Monad
import Data.Void

data Expr a
  = V a
  | IntLit Int
  | BoolLit Bool
  | LayoutLambda AdtName (Scope () Expr a)
  | ApplyLayout (Expr a) (LayoutTypeArg Expr a)
  | App String [Expr a]
  | Add (Expr a) (Expr a)
  | Sub (Expr a) (Expr a)
  | Equal (Expr a) (Expr a)
  | LayoutTypeArg (LayoutTypeArg Expr a)
  deriving (Functor)

deriveShow1 ''Expr
deriving instance Show a => Show (Expr a)

data ExprVars a
  = ClosedExpr (Expr a)
  | OpenExpr (Scope () ExprVars a)

instance Applicative Expr where
  pure = V
  (<*>) = ap

instance Monad Expr where
  return = pure
  V x >>= f = f x
  IntLit i >>= _ = IntLit i
  BoolLit b >>= _ = BoolLit b
  LayoutLambda a e >>= f = LayoutLambda a (e >>>= f)
  ApplyLayout x y >>= f = ApplyLayout (x >>= f) (layoutTypeArgSubst y f)
  App x ys >>= f = App x (fmap (>>= f) ys)
  Add x y >>= f = Add (x >>= f) (y >>= f)
  Sub x y >>= f = Sub (x >>= f) (y >>= f)
  Equal x y >>= f = Equal (x >>= f) (y >>= f)
  LayoutTypeArg x >>= f = LayoutTypeArg (layoutTypeArgSubst x f)

-- | No layout lambdas
isConcrete :: Expr a -> Bool
isConcrete (V {}) = True
isConcrete (IntLit {}) = True
isConcrete (BoolLit {}) = True
isConcrete (LayoutLambda {}) = False
isConcrete (ApplyLayout e _) = isConcrete e
isConcrete (App f xs) = all isConcrete xs

