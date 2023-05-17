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
{-# LANGUAGE TemplateHaskell #-}

module PikaC.Syntax.Pika.Layout
  where

import PikaC.Syntax.Heaplet
import PikaC.Syntax.Pika.Pattern
import PikaC.Syntax.Pika.Expr
import PikaC.Ppr

import Control.Monad.Identity

import GHC.Stack

-- import Bound
-- import Bound.Var

import Unbound.Generics.LocallyNameless

import Data.Void

import Control.Lens
import Control.Lens.TH

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
    { _layoutName :: String
    , _layoutBranches :: [LayoutBranch]
    , _layoutParams :: [LocName]
    }

-- type Layout = Layout' Identity

data LayoutBranch =
  LayoutBranch
    { _layoutPattern :: Pattern
    , _layoutBody :: LayoutBody
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
  deriving (Show, Generic)

data LayoutHeaplet
  = LPointsTo (PointsTo Expr)
  | LApply
      String -- Layout name
      Expr    -- Pattern variable
      [LayoutVar]    -- Layout variables
  deriving (Show, Generic)

type LayoutVar = Name Void
type PatternVar = Name Expr

makeLenses ''Layout
makeLenses ''LayoutBranch
makeLenses ''LayoutBody
makePrisms ''LayoutHeaplet


-- instance Subst LayoutVar (f a) => Subst LayoutVar (LayoutHeaplet f a)

-- instance Bound LayoutHeaplet where
--   LPointsTo (x :-> y) >>>= f = LPointsTo $ fmap (>>= f) x :-> fmap (>>= f) y
--   LApply name patVar layoutVars >>>= f =
--     LApply name (patVar >>= f) (map (>>= f) layoutVars)

lookupLayout :: [Layout] -> String -> Layout
lookupLayout [] name = error $ "lookupLayout: Cannot find layout " ++ name
lookupLayout (x:xs) name
  | _layoutName x == name = x
  | otherwise = lookupLayout xs name

lookupLayoutBranch :: Layout -> String -> Maybe LayoutBranch
lookupLayoutBranch layout constructor = go $ _layoutBranches layout
  where
    go [] = Nothing
    go (x:xs)
      | _patConstructor (_layoutPattern x) == constructor = Just x
      | otherwise = go xs

lookupLayoutBranch' :: HasCallStack => Layout -> String -> LayoutBranch
lookupLayoutBranch' layout c =
  case lookupLayoutBranch layout c of
    Nothing -> error $ "lookupLayoutBranch: Cannot find branch for constructor " ++ c
    Just r -> r

instance Subst Expr LayoutBody
instance Subst Expr LayoutHeaplet
instance Subst Expr a => Subst Expr (PointsTo a)

-- | Apply layout to a constructor value
applyLayout :: Layout -> String -> [Expr] -> Maybe LayoutBody
applyLayout layout constructor args = do
  branch <- lookupLayoutBranch layout constructor
  let body = _layoutBody branch
  pure $ patternMatch' (_layoutPattern branch) constructor args body

applyLayout' :: Layout -> String -> [Expr] -> LayoutBody
applyLayout' layout c args =
  case applyLayout layout c args of
    Nothing -> error $ "applyLayout': Cannot find branch for constructor " ++ c
    Just r -> r

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
