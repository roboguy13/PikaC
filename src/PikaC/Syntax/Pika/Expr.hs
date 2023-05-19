{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

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
import PikaC.Syntax.Pika.Layout
import PikaC.Syntax.Pika.Pattern

import PikaC.Ppr

import Control.Lens
import Control.Lens.TH

import Data.Data

type instance PType Expr = Expr

data Expr
  = V ExprName
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

instance Ppr Expr where
  ppr (V x) = ppr x
  ppr (IntLit i) = ppr i
  ppr (BoolLit b) = ppr b
  ppr (LayoutLambda a (B v p)) = hsep [text "/\\(" <> ppr v <> text " :~ " <> ppr a <> text ").", ppr p]
  ppr (ApplyLayout e ty) = hsep [ppr e, text "[" <> ppr ty <> text "]"]
  ppr (App f xs) = hsep (ppr f : map pprP xs)
  ppr (Add x y) = hsep [pprP x, text "+", pprP y]
  ppr (Sub x y) = hsep [pprP x, text "-", pprP y]
  ppr (Equal x y) = hsep [pprP x, text "==", pprP y]
  ppr (LName x) = ppr x

instance IsNested Expr where
  isNested (V _) = False
  isNested (IntLit _) = False
  isNested (BoolLit _) = False
  isNested (LayoutLambda {}) = True
  isNested (ApplyLayout {}) = True
  isNested (App {}) = True
  isNested (Add {}) = True
  isNested (Sub {}) = True
  isNested (Equal {}) = True
  isNested (LName {}) = False

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

instance Subst Expr (Loc a)

instance Subst TypeVar Expr where
-- instance Subst LocVar Expr where

type ExprName = Name Expr

type LayoutName = TypeName

-- newtype LayoutName' = LayoutName' LayoutName
--   deriving (Eq, Ord, Show, Generic)
-- type LayoutName = Name LayoutName'

instance Subst Expr TypeVar
  -- isvar = _

instance Subst Expr (LayoutBody Expr)
instance Subst Expr (LayoutHeaplet Expr)
instance Subst Expr (PointsTo Expr)
-- instance Subst Expr LocVar

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

