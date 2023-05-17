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
  -- | LocV LocName
  | LayoutTypeArg LocName
  | IntLit Int
  | BoolLit Bool
  | LayoutLambda AdtName (String, Expr)
  | ApplyLayout Expr LayoutName
  | Lower Expr LayoutName
  | App String [Expr]
  | Add Expr Expr
  | Sub Expr Expr
  | Equal Expr Expr
  -- | Not Expr
  -- | And Expr Expr
  deriving (Show, Generic, Data, Eq)

example :: Expr
example =
  ApplyLayout
    (LayoutLambda (AdtName "A")
      ("alpha",
        Lower (IntLit 1) (LayoutNameB "alpha")))
    (LayoutNameF "TestLayout")

-- instance Subst LayoutName Expr

instance Plated Expr

-- instance Plated Expr where
--   plate f (V x) = pure $ V x
--   plate f (LayoutTypeArg x) = pure $ LayoutTypeArg x
--   plate f (IntLit x) = pure $ IntLit x
--   plate f (BoolLit b) = pure $ BoolLit b
--   plate f (LayoutLambda adtName bnd) =
--     LayoutLambda adtName <$> _ f bnd

instance Alpha Expr
instance Subst ExprName Expr

instance Subst Expr a => Subst Expr (PointsTo a)
instance Subst Expr Loc

-- instance Subst LayoutName' Expr

type ExprName = Name Expr
-- type LocName = Name 

data LayoutName =
    LayoutNameF String
  | LayoutNameB String
  deriving (Show, Generic, Data, Eq)
-- type LayoutName = Name LayoutName'

instance Alpha LayoutName
instance Subst ExprName LayoutName

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
reduceLayouts =
  rewrite go
  where
    go :: Expr -> Maybe Expr
    go (ApplyLayout (LayoutLambda _ (v, e)) arg) =
      Just $ substLayoutName v arg e
      -- Just $ substBind e (LayoutName arg)
    go _ = Nothing

-- NOTE: We are not careful about capture avoidance here.
substLayoutName :: String -> LayoutName -> Expr -> Expr
substLayoutName old new =
  transform go
  where
    go (ApplyLayout f arg) =
      ApplyLayout f (renameLayoutName old new arg)
    go (Lower x layout) =
      Lower x (renameLayoutName old new layout)
    go e = e

renameLayoutName :: String -> LayoutName -> LayoutName -> LayoutName
renameLayoutName old new x0@(LayoutNameB x)
  | x == old = new
renameLayoutName _ _ x0 = x0

