{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}

module PikaC.Syntax.Pika.Layout
  where

import PikaC.Syntax.Heaplet
import PikaC.Syntax.Pika.Pattern
import PikaC.Syntax.Pika.Expr
import PikaC.Ppr

import Control.Monad.Identity

-- import Bound
-- import Bound.Var

import Unbound.Generics.LocallyNameless

import Data.Void

import Control.Monad.Morph

import GHC.Generics

-- newtype Operand a b = Operand (Var a b)
--   deriving (Functor, Applicative, Monad, Show)

-- pattern Actual :: a -> Operand a b
-- pattern Actual x = Operand (B x)
-- pattern OperandV y = Operand (F y)

-- instance (Show a, Show b) => Ppr (Operand a b) where ppr = text . show

-- newtype Operand a = Operand (Var (Expr a) a)
--
-- evalOperand :: Operand a -> Expr a
-- evalOperand (Operand (B x)) = x
-- evalOperand (Operand (F y)) = V y

data Layout =
  Layout
    { layoutName :: String
    , layoutBranches :: [LayoutBranch]
    , layoutParams :: [LocName]
    }

-- type Layout = Layout' Identity

data LayoutBranch =
  LayoutBranch
    { layoutPattern :: Pattern
    , layoutBody :: LayoutBody
    }

-- instance Bound f => Functor (LayoutBranch f)
--
-- instance Bound f => Monad (LayoutBranch f)

-- mkLayoutBranch :: Pattern a -> LayoutBody Identity a -> LayoutBranch LayoutBody a
-- mkLayoutBranch pat body =
--   LayoutBranch
--     { layoutPattern = pat
--     , layoutBody = abstract Just (hoist generalize body)
--     }

-- type LayoutBranch = LayoutBranch' Identity
--
-- newtype LayoutBody operand a = LayoutBody [LayoutHeaplet operand a]
newtype LayoutBody = LayoutBody [LayoutHeaplet]
  deriving (Show)

data LayoutHeaplet
  = LPointsTo (PointsTo Expr)
  | LApply
      String -- Layout name
      Expr    -- Pattern variable
      [LayoutVar]    -- Layout variables
  deriving (Show, Generic)

type LayoutVar = Name Void
type PatternVar = Name Expr

-- instance Subst LayoutVar (f a) => Subst LayoutVar (LayoutHeaplet f a)

-- instance Bound LayoutHeaplet where
--   LPointsTo (x :-> y) >>>= f = LPointsTo $ fmap (>>= f) x :-> fmap (>>= f) y
--   LApply name patVar layoutVars >>>= f =
--     LApply name (patVar >>= f) (map (>>= f) layoutVars)

lookupLayout :: [Layout] -> String -> Layout
lookupLayout [] name = error $ "lookupLayout: Cannot find layout " ++ name
lookupLayout (x:xs) name
  | layoutName x == name = x
  | otherwise = lookupLayout xs name
--
-- lookupLayoutBranch :: Layout' operand a -> String -> LayoutBranch operand a
-- lookupLayoutBranch layout constructor = go $ layoutBranches layout
--   where
--     go [] = error $ "lookupLayoutBranch: Cannot find branch for constructor " ++ constructor
--     go (x:xs)
--       | patConstructor (layoutPattern x) == constructor = x
--       | otherwise = go xs
--
-- -- | Apply layout to a constructor value
-- applyLayout :: (Ppr (Expr a), Monad operand, Ppr (operand a), Ppr a, Eq a) => Layout' operand a -> String -> [operand a] -> operand (Expr a)
-- applyLayout layout constructor args =
--   let branch = lookupLayoutBranch layout constructor
--       body = layoutBody branch
--   in
--   patternMatch' (layoutPattern branch) body constructor (map (fmap V) args)
--
-- -- applyLayout' :: (Ppr (Expr a), Show a, Ppr a, Eq a) => Layout' (Operand (LayoutBody a)) a -> String -> [Operand (LayoutBody a) a] -> Operand (LayoutBody a) (Expr a)
-- -- applyLayout' = applyLayout
--
-- -- | Reduce layout lambda applications
-- reduceLayoutApps :: forall a. Expr a -> Expr a
-- reduceLayoutApps = error "TODO: Implement reduceLayoutApps" --go
--   where
--     go :: Expr a -> Expr a
--     go (ApplyLayout (LayoutLambda adt e) layout) = instantiate1 (LayoutTypeArg layout) e
--     -- go e = e
--
