{-# LANGUAGE DeriveGeneric #-}

module PikaC.Backend.SuSLik.Syntax
  where

import PikaC.Ppr
import PikaC.Utils

import PikaC.Syntax.Heaplet

import Unbound.Generics.LocallyNameless

import GHC.Generics

import Data.Validity

type ExprName = Name Expr

data Expr
  = V ExprName
  | IntLit Int
  | BoolLit Bool
  | Add Expr Expr
  | Sub Expr Expr
  | Equal Expr Expr
  | And Expr Expr
  | Not Expr
  deriving (Show, Generic)

instance HasVar Expr where mkVar = V

instance IsBase Expr where
  isVar (V {}) = True
  isVar _ = False

  isLit (IntLit {}) = True
  isLit (BoolLit {}) = True
  isLit _ = False

  intLit = IntLit
  boolLit = BoolLit
  mkNot = Not
  mkEqual = Equal
  mkAnd = And

data InductivePredicate
  = InductivePredicate
    { _indPredName :: String
    , _indPredBody ::
        Bind [ExprName]
          [PredicateBranch]
    }
  deriving (Show, Generic)

data HeapletS
  = PointsToS (PointsTo Expr)
  | ApplyS String [Expr]
  deriving (Show, Generic)

type ExistsVar = ExprName

type Assertion =
  Bind [ExistsVar] [HeapletS]

data PredicateBranch
  = PredicateBranch
    { _predBranchCond :: Expr
    , _predBranchPure :: Expr
    , _predBranchAssertion :: Assertion
    }
  deriving (Show, Generic)

data FnSig
  = FnSig
    { _fnSigName :: String
    , _fnSigConds ::
        Bind [ExprName] -- Function parameters
          FnSpec
    }

data FnSpec
  = FnSpec
    { _fnSpecPrecond :: Assertion
    , _fnSpecPostcond :: Assertion
    }

