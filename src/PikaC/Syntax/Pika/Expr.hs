{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module PikaC.Syntax.Pika.Expr
  where

import PikaC.Syntax.Type

-- import Bound
import Unbound.Generics.LocallyNameless
import Text.Show.Deriving

import Control.Monad
import Data.Void

import GHC.Generics

import PikaC.Syntax.Heaplet (LocName)

data Expr
  = V ExprName
  -- | LocV LocName
  | LayoutTypeArg LocName
  | IntLit Int
  | BoolLit Bool
  | LayoutLambda AdtName (Bind LayoutName Expr)
  | ApplyLayout Expr LayoutName
  | App String [Expr]
  | Add Expr Expr
  | Sub Expr Expr
  | Equal Expr Expr
  deriving (Show, Generic)

instance Alpha Expr
instance Subst Expr Expr
instance Subst LayoutName Expr

type ExprName = Name Expr
-- type LocName = Name 

newtype LayoutName' = LayoutName String
type LayoutName = Name LayoutName'

-- | No layout lambdas
isConcrete :: Expr -> Bool
isConcrete (V {}) = True
isConcrete (IntLit {}) = True
isConcrete (BoolLit {}) = True
isConcrete (LayoutLambda {}) = False
isConcrete (ApplyLayout e _) = isConcrete e
isConcrete (App f xs) = all isConcrete xs

