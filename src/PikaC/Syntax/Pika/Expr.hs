{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module PikaC.Syntax.Pika.Expr
  where

import PikaC.Syntax.Type
import PikaC.Utils

-- import Bound
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Bind
import Text.Show.Deriving

import Control.Monad
import Data.Void

import GHC.Generics

import PikaC.Syntax.Heaplet

import Control.Lens
import Control.Lens.TH

import Data.Data

data Expr
  = V ExprName
  | LocV LocName
  | IntLit Int
  | BoolLit Bool
  | LayoutLambda AdtName (Bind LayoutName Expr)
  | ApplyLayout Expr TypeVar
  | App String [Expr]
  | Add Expr Expr
  | Sub Expr Expr
  | Equal Expr Expr
  | LName LayoutName
  -- | Not Expr
  -- | And Expr Expr
  deriving (Show, Generic)

-- example :: Expr
-- example =
--   ApplyLayout
--     (LayoutLambda (AdtName "A")
--       (bind (string2Name "alpha")
--         (ApplyLayout (IntLit 1) (LayoutName' (string2Name "alpha")))))
--     (LayoutName' (string2Name "TestLayout"))

instance Subst Expr AdtName

instance Alpha Expr
instance Subst Expr Expr where
  isvar (V n) = Just $ SubstName n
  isvar _ = Nothing

instance Subst Expr Loc

instance Subst TypeVar Expr where
instance Subst LocVar Expr where

type ExprName = Name Expr

type LayoutName = TypeName

-- newtype LayoutName' = LayoutName' LayoutName
--   deriving (Eq, Ord, Show, Generic)
-- type LayoutName = Name LayoutName'

instance Subst Expr TypeVar
  -- isvar = _

makePrisms ''Expr

-- | No layout lambdas
isConcrete :: Expr -> Bool
isConcrete (V {}) = True
isConcrete (IntLit {}) = True
isConcrete (BoolLit {}) = True
isConcrete (LayoutLambda {}) = False
isConcrete (ApplyLayout e _) = isConcrete e
isConcrete (App f xs) = all isConcrete xs

reduceLayouts :: Expr -> Expr
reduceLayouts = go
  where
    go :: Expr -> Expr
    go (V x) = V x
    go (LocV x) = LocV x
    go (IntLit i) = IntLit i
    go (BoolLit b) = BoolLit b
    go (LayoutLambda a (B p t)) =
      LayoutLambda a (B p (go t))
    go (ApplyLayout e arg) =
      case go e of
        (LayoutLambda _ bnd) -> substBind bnd arg
        e' -> ApplyLayout e' arg
    go (App f args) =
      App f (map go args)
    go (Add x y) = Add (go x) (go y)
    go (Sub x y) = Sub (go x) (go y)
    go (Equal x y) = Equal (go x) (go y)
    go (LName x) = LName x

