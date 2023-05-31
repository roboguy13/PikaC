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
import PikaC.Utils
import PikaC.Syntax.Type (AdtName)

import PikaC.Syntax.Pika.Pattern

import Data.Maybe
import Control.Monad.Identity

import Unbound.Generics.LocallyNameless

import GHC.Generics
import Data.Data

import Data.Ord

import Control.Lens.Plated
import Control.Lens hiding (elements)

import Control.Monad

import Test.QuickCheck

import GHC.Stack

type LayoutArg a = [Name a]

instance Ppr a => Ppr (LayoutArg a) where
  ppr xs =  text "{" <+> hsep (punctuate (text ",") (map ppr xs)) <+> text "}"

data PointsTo a = Loc a :-> a
  deriving (Show, Generic, Eq, Ord)

instance Subst (Name a) a => Subst (Name a) (PointsTo a)
instance Subst (Name a) a => Subst (Name a) (Loc a)
instance Subst (Pattern a) a => Subst (Pattern a) (PointsTo a)
instance Subst (Pattern a) a => Subst (Pattern a) (Loc a)

pointsToRhsLens :: Lens' (PointsTo a) a
pointsToRhsLens =
  lens (\(_ :-> y) -> y)
       (\(x :-> y) z -> x :-> z)

-- data Loc = LocName :+ Int
data Loc a = a :+ Int
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

locBase :: Loc a -> a
locBase (x :+ _) = x

locIx :: Loc a -> Int
locIx (_ :+ i) = i

-- pointsToNames :: Eq a => [PointsTo a] -> [a]
-- pointsToNames = nub . map (locBase . pointsToLhs)

data Allocation a = Alloc { allocName :: Name a, allocSize :: Int }
  deriving (Show, Generic)

instance Ppr a => Ppr (Allocation a) where
  ppr (Alloc n sz) = text "[[" <> ppr n <> text "," <> text (show sz) <> text "]]"

locsPossiblyWrittenTo :: [PointsTo a] -> [Loc a]
locsPossiblyWrittenTo = map pointsToLhs

lookupAllocation :: HasCallStack => [Allocation a] -> Name a -> Int
lookupAllocation allocs n =
  case find ((== n) . allocName) allocs of
    Just r -> allocSize r
    Nothing -> error $ "Cannot find allocation for " ++ ppr' n

toMaxAllocs :: [Allocation a] -> [Allocation a]
toMaxAllocs allocs0 = map go $ fastNub $ map allocName allocs0
  where
    go n = maximumBy (comparing allocSize) $ filter ((== n) . allocName) allocs0

findAllocations :: forall a. (Alpha a, IsName a a) => [Name a] -> [PointsTo a] -> [Allocation a]
findAllocations names xs = map toAlloc locMaximums
  where
      -- Grouped by base name. Only include locations that have base names
      -- included in the 'names' argument
    locGroups :: [[Loc a]]
    locGroups = groupBy (aeq `on` locBase) . filter ((`elem` names) . getName . locBase) $ map pointsToLhs xs

    locMaximums :: [Loc a]
    locMaximums = map (foldr1 go) locGroups

    go :: Loc a -> Loc a -> Loc a
    go (name :+ i) (_ :+ j) = name :+ max i j

    toAlloc :: Loc a -> Allocation a
    toAlloc (x :+ i) = Alloc (getName x) (i + 1)

instance Subst a a => Subst a (Allocation a)
instance (Alpha a, Typeable a) => Alpha (Allocation a)

-- Property testing --

instance Arbitrary a => Arbitrary (Allocation a) where
  arbitrary = Alloc <$> arbitrary <*> choose (0, 10)
  shrink = genericShrink

instance (IsBase a, Arbitrary a) => Arbitrary (Loc a) where
  arbitrary = liftA2 (:+) (arbitrary `suchThat` isVar) arbitrary
  shrink (x :+ i) = liftA2 (:+) (filter isVar (shrink x)) (shrink i)

instance (IsBase a, Arbitrary a) => Arbitrary (PointsTo a) where
  arbitrary = liftA2 (:->) arbitrary arbitrary
  shrink (x :-> y) = liftA2 (:->) (shrink x) (shrink y)

-- shrinkAssertion :: forall a. (a -> [a]) -> [PointsTo a] -> [[PointsTo a]]
-- shrinkAssertion shrinkA = shrinkList go
--   where
--     go :: PointsTo a -> [PointsTo a]
--     go (x :-> y) = fmap (x :->) (shrinkA y)

genValidAssertion :: HasVar a => [Name a] -> (Int -> Gen a) -> Int -> Gen [PointsTo a]
genValidAssertion names genA size = do
  i <- choose (1, 4)
  replicateM i (genValidPointsTo names genA (size `div` i))

genValidPointsTo :: HasVar a => [Name a] -> (Int -> Gen a) -> Int -> Gen (PointsTo a)
genValidPointsTo names genA size = do
    loc <- genValidLoc names halvedGenA
    rhs <- halvedGenA
    pure (loc :-> rhs)
  where
    halvedGenA =
      genA (size `div` 2)

genValidLoc :: HasVar a => [Name a] -> Gen a -> Gen (Loc a)
genValidLoc names genA = do
  i <- choose (0, 5)
  v <- mkVar <$> elements names
  pure (v :+ i)

genValidAllocations :: IsName a a => [PointsTo a] -> Gen [Allocation a]
genValidAllocations = mapM genValidAllocation

genValidAllocation :: IsName a a => PointsTo a -> Gen (Allocation a)
genValidAllocation (_ :-> rhs) = do
  genValidAllocationForName rhs

genValidAllocationForName :: IsName a a => a -> Gen (Allocation a)
genValidAllocationForName n = do
  sz <- choose (0, 5)
  pure (Alloc (getName n) sz)

