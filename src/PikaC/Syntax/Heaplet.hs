{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module PikaC.Syntax.Heaplet
  where

import Data.Semigroup
import Data.List
import Data.Function
import PikaC.Ppr

import Data.Maybe
import Control.Monad.Identity

-- data Heaplet a
--   deriving (Show, Functor)

newtype LayoutArg a = LayoutArg [a]
  deriving (Show, Functor, Semigroup, Monoid, Foldable, Eq, Ord)

class LayoutRename f where
  renameLayoutArg ::
    Ord a => LayoutArg a -> LayoutArg a -> f a -> f a

instance LayoutRename LayoutArg where
  renameLayoutArg old new (LayoutArg xs) =
      LayoutArg $ map (renameLayoutArg' old new) xs
    where
      go = renameLayoutArg' old new

instance LayoutRename Identity where
  renameLayoutArg old new (Identity x) =
    Identity $ renameLayoutArg' old new x

renameLayoutArg' :: Ord a =>LayoutArg a -> LayoutArg a -> a -> a
renameLayoutArg' (LayoutArg old) (LayoutArg new) x =
      fromMaybe x $ lookup x assocs
    where
      assocs = zip old new

data PointsTo f a = Loc a :-> f a
  deriving (Show, Foldable)

data Loc a = a :+ Int
  deriving (Show, Functor, Eq, Ord, Foldable)

class HasLocs f where
  getLocs :: f a -> [Loc a]

instance HasLocs Loc where
  getLocs x = [x]

instance HasLocs LayoutArg where
  getLocs (LayoutArg xs) = map (:+ 0) xs

instance HasLocs f => HasLocs (PointsTo f) where
  getLocs (lhs :-> rhs) = lhs : getLocs rhs

instance LayoutRename f => LayoutRename (PointsTo f) where
  renameLayoutArg old new (lhs :-> rhs) =
    renameLayoutArg old new lhs :-> renameLayoutArg old new rhs

instance LayoutRename Loc where
  renameLayoutArg old new (x :+ i) =
    renameLayoutArg' old new x :+ i

instance Ppr a => Ppr (Loc a) where
  ppr (x :+ 0) = ppr x
  ppr (x :+ i) = parens $ hsep [ppr x, text "+", ppr i]

instance (Ppr a, Ppr (f a)) => Ppr (PointsTo f a) where
  ppr (lhs :-> rhs) = hsep [ppr lhs, text ":->", ppr rhs]

instance (Ppr a, Ppr (f a)) => Ppr [PointsTo f a] where
  ppr xs = braces $ sep $ punctuate (text "**") (map ppr xs)

pointsToLhs :: PointsTo f a -> Loc a
pointsToLhs (lhs :-> _) = lhs

pointsToRhs :: PointsTo f a -> f a
pointsToRhs (_ :-> rhs) = rhs

locBase :: Loc a -> a
locBase (x :+ _) = x

locIx :: Loc a -> Int
locIx (_ :+ i) = i

pointsToNames :: Ord a => [PointsTo f a] -> [a]
pointsToNames = nub . map (locBase . pointsToLhs)

data Allocation a = Alloc a Int
  deriving (Show)

findAllocations :: forall f a b. Eq a => [a] -> [PointsTo f a] -> [Allocation a]
findAllocations names xs = map toAlloc locMaximums
  where
      -- Grouped by base name. Only include locations that have base names
      -- included in the 'names' argument
    locGroups :: [[Loc a]]
    locGroups = groupBy ((==) `on` locBase) . filter ((`elem` names) . locBase) $ map pointsToLhs xs

    locMaximums :: [Loc a]
    locMaximums = map (foldr1 go) locGroups

    go :: Loc a -> Loc a -> Loc a
    go (name :+ i) (_ :+ j) = name :+ max i j

    toAlloc :: Loc a -> Allocation a
    toAlloc (x :+ i) = Alloc x i

