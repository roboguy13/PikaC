{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
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

import Control.Lens.Plated
import Control.Lens

type LayoutArg a = [Name a]

instance Ppr a => Ppr (LayoutArg a) where
  ppr xs =  text "{" <+> hsep (punctuate (text ",") (map ppr xs)) <+> text "}"

data PointsTo a = Loc a :-> a
  deriving (Show, Generic, Eq, Ord)

pointsToRhsLens :: Lens' (PointsTo a) a
pointsToRhsLens =
  lens (\(_ :-> y) -> y)
       (\(x :-> y) z -> x :-> z)

-- data Loc = LocName :+ Int
data Loc a = Name a :+ Int
  deriving (Show, Eq, Ord, Generic)

instance Plated (PointsTo a) where
  plate _ = pure

instance Plated (Loc a) where
  plate _ = pure
  -- plate f (x :-> y) = plate f x :-> undefined

instance (Typeable a, Alpha a) => Alpha (Loc a)

-- newtype LocVar = LocVar { unLocVar :: LocName } deriving (Show, Generic)
-- type LocName = Name LocVar

-- instance Alpha LocVar
--
-- instance Subst LocVar Loc
-- instance Subst LocVar a => Subst LocVar (PointsTo a)
--
-- instance Subst LocVar LocVar where
--   isvar (LocVar n) = Just $ SubstName n
--
-- instance Subst LocVar AdtName
-- instance Subst LocVar TypeVar
--
-- instance Ppr LocVar where ppr (LocVar v) = text $ show v

-- instance Ppr LocVar where ppr (LocVar v) = text v
--
-- instance Alpha LocVar

class HasPointsTo a where
  getPointsTo :: a -> [PointsTo a]

instance (Typeable a, Alpha a, Show a) => Alpha (PointsTo a)

instance Ppr a => Ppr (Loc a) where
  ppr (x :+ 0) = ppr x
  ppr (x :+ i) = parens $ hsep [ppr x, text "+", ppr i]

instance (Ppr a) => Ppr (PointsTo a) where
  ppr (lhs :-> rhs) = hsep [ppr lhs, text ":->", ppr rhs]

instance (Ppr a) => Ppr [PointsTo a] where
  ppr xs = braces $ sep $ punctuate (text " **") (map ppr xs)

pointsToLhs :: PointsTo a -> Loc a
pointsToLhs (lhs :-> _) = lhs

pointsToRhs :: PointsTo a -> a
pointsToRhs (_ :-> rhs) = rhs

locBase :: Loc a -> Name a
locBase (x :+ _) = x

locIx :: Loc a -> Int
locIx (_ :+ i) = i

pointsToNames :: [PointsTo a] -> [Name a]
pointsToNames = nub . map (locBase . pointsToLhs)

data Allocation a = Alloc (Name a) Int
  deriving (Show)

locsPossiblyWrittenTo :: [PointsTo a] -> [Loc a]
locsPossiblyWrittenTo = map pointsToLhs

findAllocations :: forall a. [Name a] -> [PointsTo a] -> [Allocation a]
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
    toAlloc (x :+ i) = Alloc x (i + 1)

