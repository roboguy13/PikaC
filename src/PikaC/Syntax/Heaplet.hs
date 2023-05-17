{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module PikaC.Syntax.Heaplet
  where

import Data.Semigroup
import Data.List
import Data.Function

import PikaC.Ppr
import PikaC.Syntax.Type (AdtName, TypeVar)

import Data.Maybe
import Control.Monad.Identity

import Unbound.Generics.LocallyNameless

import GHC.Generics
import Data.Data

type LayoutArg = [LocName]

instance Ppr LayoutArg where
  ppr xs =  text "{" <+> hsep (punctuate (text ",") (map ppr xs)) <+> text "}"

data PointsTo a = Loc :-> a
  deriving (Show, Foldable, Functor, Generic, Eq, Ord, Traversable)

-- data Loc = LocName :+ Int
data Loc = LocName :+ Int
  deriving (Show, Eq, Ord, Generic)

instance Alpha Loc

newtype LocVar = LocVar { unLocVar :: LocName } deriving (Show, Generic)
type LocName = Name LocVar

instance Alpha LocVar

instance Subst LocVar Loc
instance Subst LocVar a => Subst LocVar (PointsTo a)

instance Subst LocVar LocVar where
  isvar (LocVar n) = Just $ SubstName n

instance Subst LocVar AdtName
instance Subst LocVar TypeVar

instance Ppr LocVar where ppr (LocVar v) = text $ show v

-- instance Ppr LocVar where ppr (LocVar v) = text v
--
-- instance Alpha LocVar

class HasPointsTo a where
  getPointsTo :: a -> [PointsTo a]

instance (Alpha a, Show a) => Alpha (PointsTo a)

instance Ppr Loc where
  ppr (x :+ 0) = ppr x
  ppr (x :+ i) = parens $ hsep [ppr x, text "+", ppr i]

instance (Ppr a) => Ppr (PointsTo a) where
  ppr (lhs :-> rhs) = hsep [ppr lhs, text ":->", ppr rhs]

instance (Ppr a) => Ppr [PointsTo a] where
  ppr xs = braces $ sep $ punctuate (text "**") (map ppr xs)

pointsToLhs :: PointsTo a -> Loc
pointsToLhs (lhs :-> _) = lhs

pointsToRhs :: PointsTo a -> a
pointsToRhs (_ :-> rhs) = rhs

locBase :: Loc -> LocName
locBase (x :+ _) = x

locIx :: Loc -> Int
locIx (_ :+ i) = i

pointsToNames :: [PointsTo a] -> [LocName]
pointsToNames = nub . map (locBase . pointsToLhs)

findSetToZero :: [LocName] -> [PointsTo a] -> [LocName]
findSetToZero names xs =
    let modified = go xs
    in
    filter (`notElem` modified) names
  where
    go [] = []
    go ((x :-> y):rest) = locBase x : go rest

data Allocation = Alloc LocName Int
  deriving (Show)

findAllocations :: forall a. [LocName] -> [PointsTo a] -> [Allocation]
findAllocations names xs = map toAlloc locMaximums
  where
      -- Grouped by base name. Only include locations that have base names
      -- included in the 'names' argument
    locGroups :: [[Loc]]
    locGroups = groupBy ((==) `on` locBase) . filter ((`elem` names) . locBase) $ map pointsToLhs xs

    locMaximums :: [Loc]
    locMaximums = map (foldr1 go) locGroups

    go :: Loc -> Loc -> Loc
    go (name :+ i) (_ :+ j) = name :+ max i j

    toAlloc :: Loc -> Allocation
    toAlloc (x :+ i) = Alloc x i

