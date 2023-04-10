{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module PikaC.Syntax.Expr
  where

data Stage = Parsed | Defun

-- | This gets removed in defunctionalization
type family ExprLamX x a where
  ExprLamX Parsed a = Expr Parsed a

-- | This gets removed when the layouts are specified
type family ExprAppX x a where
  ExprAppX Parsed a = ()

data Expr x a
  = Var a
  | App (ExprAppX x a) (Expr x a) (Expr x a)
  | Lit Int
  | Add (Expr x a) (Expr x a)
  | Sub (Expr x a) (Expr x a)
  | Equal (Expr x a) (Expr x a)
  | Lt (Expr x a) (Expr x a)
  | Not (Expr x a)
  | And (Expr x a) (Expr x a)
  | Lam (ExprLamX x a)

