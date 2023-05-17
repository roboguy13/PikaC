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
  | ApplyLayout Expr LayoutName'
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
--         (ApplyLayout (IntLit 1) (LayoutName' "alpha"))))
--     (LayoutName' "TestLayout")

-- instance Subst LayoutName Expr

-- instance Plated Expr

-- instance Plated Expr where
--   plate f (V x) = pure $ V x
--   plate f (IntLit x) = pure $ IntLit x
--   plate f (BoolLit b) = pure $ BoolLit b
--   plate f (LayoutLambda adtName bnd) =
--     LayoutLambda adtName <$> _ (fmap f bnd)

instance Subst Expr AdtName

instance Alpha Expr
instance Subst Expr Expr where
  isCoerceVar (V n) = Just $ SubstCoerce n Just
  isCoerceVar _ = Nothing
-- instance Subst ExprName Expr where
--   isCoerceVar (V n) = Just $ SubstCoerce _ _
  -- isvar (LName n) = Just $ SubstName n

-- instance Subst Expr a => Subst Expr (PointsTo a)
-- instance Subst Expr Loc

-- instance Subst LayoutName Expr where
--   isCoerceVar (LName n) = Just $ SubstCoerce _ undefined

instance Subst LayoutName' AdtName

instance Subst Expr Loc

instance Subst LayoutName' Expr where
  -- isCoerceVar (LName n) = Just $ SubstCoerce n (Just . LName . string2Name . unLayoutName')
  -- isCoerceVar (ApplyLayout e n) = Just $ SubstCoerce (string2Name (unLayoutName' n)) (Just . ApplyLayout e)
  -- isCoerceVar _ = Nothing

instance Subst LocVar Expr where
  -- isCoerceVar (LocV v) = Just $ SubstCoerce v (Just . LocV . string2Name . unLocVar)

instance Subst LocVar LayoutName'

instance Subst Expr LayoutName'
instance Subst LayoutName' LayoutName'

type ExprName = Name Expr
-- type LocName = Name 

newtype LayoutName' = LayoutName' LayoutName
  deriving (Eq, Ord, Show, Generic)
type LayoutName = Name LayoutName'

instance Alpha LayoutName'

instance Subst ExprName LayoutName'
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
reduceLayouts =
  undefined
  -- rewrite go
  where
    go :: Expr -> Maybe Expr
    go (ApplyLayout (LayoutLambda _ (B p t)) arg) =
      Just $ subst p arg t
    go _ = Nothing

