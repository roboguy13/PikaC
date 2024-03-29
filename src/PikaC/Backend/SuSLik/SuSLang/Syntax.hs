{-# LANGUAGE DeriveGeneric #-}

module PikaC.Backend.SuSLik.SuSLang.Syntax
  where

import Unbound.Generics.LocallyNameless

import Control.DeepSeq

import GHC.Generics
import GHC.Stack

import PikaC.Syntax.Heaplet

type ExprName = Name Expr

data Expr
  = V ExprName
  | IntLit Int
  | BoolLit Bool

  | LocVal (Loc Expr)

  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Mod Expr Expr
  | Div Expr Expr
  | Equal Expr Expr
  | Lt Expr Expr
  | Le Expr Expr
  | Not Expr
  | And Expr Expr
  | If Expr Expr Expr
  deriving (Show, Generic)

data Command
  = LetMalloc Int (Bind ExprName [Command])
  | Let Expr (Bind ExprName [Command])
  | IfThenElse Expr [Command] [Command]
  | Call String [Expr]
  | Write (Loc Expr) Expr
  | Assert Expr [Command]
  | Free Expr
  deriving (Show, Generic)

data SuSType = IntS | BoolS | LocS
  deriving (Show, Generic)

data Function =
  Function
  { functionName :: String
  , functionParamTypes :: [SuSType]
  , functionBody :: Bind [ExprName] [Command]
  }
  deriving (Show, Generic)

instance NFData SuSType
instance NFData Command
instance NFData Expr
instance NFData Function

instance Alpha Expr
instance Alpha Command

getV :: HasCallStack => Expr -> ExprName
getV (V x) = x

