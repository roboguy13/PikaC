{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module PikaC.Syntax.Pika.Layout
  where

import PikaC.Syntax.Heaplet
import PikaC.Syntax.Pika.Pattern
import PikaC.Syntax.Type

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

data Layout a =
  Layout
    { _layoutName :: String
    , _layoutSig :: LayoutSig a
    , _layoutBranches :: [LayoutBranch a]
    -- , _layoutParams :: [Name a]
    }
    deriving (Show)

data LayoutSig a =
  LayoutSig
    { _layoutSigAdt :: AdtName
    , _layoutSigParams :: [Name a]
    }
    deriving (Show)

-- type Layout = Layout' Identity

data LayoutBranch a =
  LayoutBranch
    { _layoutPattern :: Pattern a
    , _layoutBody :: LayoutBody a
    }
    deriving (Show)

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
newtype LayoutBody a = LayoutBody { _unLayoutBody :: [LayoutHeaplet a] }
  deriving (Show, Generic, Semigroup, Monoid)

data LayoutHeaplet a
  = LPointsTo (PointsTo a)
  | LApply
      String -- Layout name
      a    -- Pattern variable
      [Name a]    -- Layout variables
      -- [LocVar]    -- Layout variables
  deriving (Show, Generic)

-- type PatternVar = Name Expr

makeLenses ''Layout
makeLenses ''LayoutBranch
makeLenses ''LayoutBody
makePrisms ''LayoutHeaplet

-- updateLayoutParams :: Fresh m =>
--   (Name a -> m (Name b)) -> [LayoutBranches
-- updateLayoutParams = undefined

-- instance Subst LayoutVar (f a) => Subst LayoutVar (LayoutHeaplet f a)

-- instance Bound LayoutHeaplet where
--   LPointsTo (x :-> y) >>>= f = LPointsTo $ fmap (>>= f) x :-> fmap (>>= f) y
--   LApply name patVar layoutVars >>>= f =
--     LApply name (patVar >>= f) (map (>>= f) layoutVars)

lookupLayout :: [Layout a] -> String -> Layout a
lookupLayout [] name = error $ "lookupLayout: Cannot find layout " ++ name
lookupLayout (x:xs) name
  | _layoutName x == name = x
  | otherwise = lookupLayout xs name

lookupLayoutBranch :: HasApp a => Layout a -> String -> Maybe (LayoutBranch a)
lookupLayoutBranch layout constructor = go $ _layoutBranches layout
  where
    go [] = Nothing
    go (x:xs) =
      case _layoutPattern x of
        PatternVar _ -> Just x
        Pattern c _
          | c == constructor -> Just x
          | otherwise -> go xs
    -- go (x:xs)
    --   | _patConstructor (_layoutPattern x) == constructor = Just x
    --   | otherwise = go xs

lookupLayoutBranch' :: (HasCallStack, HasApp a) => Layout a -> String -> LayoutBranch a
lookupLayoutBranch' layout c =
  case lookupLayoutBranch layout c of
    Nothing -> error $ "lookupLayoutBranch: Cannot find branch for constructor " ++ c
    Just r -> r

-- instance Subst a a => Subst a (LayoutBody a)
-- instance Subst a a => Subst a (LayoutHeaplet a)
-- instance Subst Expr a => Subst Expr (PointsTo a)

-- | Apply layout to a constructor value
applyLayout :: (HasApp a, HasApp (PType a), Subst (PType a) (LayoutBody a)) => Layout a -> String -> [PType a] -> Maybe (LayoutBody a)
applyLayout layout constructor args = do
  branch <- lookupLayoutBranch layout constructor
  let body = _layoutBody branch
  pure $ patternMatch' (_layoutPattern branch) constructor args body

applyLayout' :: (Show a, HasApp a, HasApp (PType a), Subst (PType a) (LayoutBody a)) => Layout a -> String -> [PType a] -> LayoutBody a
applyLayout' layout c args =
  case applyLayout layout c args of
    Nothing -> error $ "applyLayout': Cannot find branch for constructor " ++ c ++ " in " ++ show layout
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
