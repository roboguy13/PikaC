{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module PikaC.Syntax.Heaplet
  where

import Data.Semigroup
import Data.List
import Data.Function
import PikaC.Ppr

-- data Heaplet a
--   deriving (Show, Functor)

data PointsTo a e = Loc a :-> e
  deriving (Show, Functor)

data Loc a = a :+ Int
  deriving (Show, Functor)

instance Ppr a => Ppr (Loc a) where
  ppr (x :+ 0) = ppr x
  ppr (x :+ i) = parens $ hsep [ppr x, text "+", ppr i]

instance (Ppr a, Ppr e) => Ppr (PointsTo a e) where
  ppr (lhs :-> rhs) = hsep [ppr lhs, text ":->", ppr rhs]

instance (Ppr a, Ppr e) => Ppr [PointsTo a e] where
  ppr xs = braces $ sep $ punctuate (text "**") (map ppr xs)

pointsToLhs :: PointsTo a e -> Loc a
pointsToLhs (lhs :-> _) = lhs

pointsToRhs :: PointsTo a e -> e
pointsToRhs (_ :-> rhs) = rhs

locBase :: Loc a -> a
locBase (x :+ _) = x

locIx :: Loc a -> Int
locIx (_ :+ i) = i

pointsToNames :: Ord a => [PointsTo a e] -> [a]
pointsToNames = nub . map (locBase . pointsToLhs)

data Allocation a = Alloc a Int
  deriving (Show)

findAllocations :: forall a b. Eq a => [a] -> [PointsTo a b] -> [Allocation a]
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

