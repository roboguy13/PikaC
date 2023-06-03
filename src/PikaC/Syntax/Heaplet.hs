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
import Data.Validity

import GHC.Stack

import Control.DeepSeq

type LayoutArg a = [Name a]

instance Ppr a => Ppr (LayoutArg a) where
  ppr xs =  text "{" <+> hsep (punctuate (text ",") (map ppr xs)) <+> text "}"

data PointsTo a = Loc a :-> a
  deriving (Show, Generic, Eq, Ord)

instance Subst (Name a) a => Subst (Name a) (PointsTo a)
instance Subst (Name a) a => Subst (Name a) (Loc a)
instance Subst (Pattern a) a => Subst (Pattern a) (PointsTo a)
instance Subst (Pattern a) a => Subst (Pattern a) (Loc a)

instance (Typeable a, Alpha a, WellScoped (Name a) a) => WellScoped (Name a) (PointsTo a)
instance (Typeable a, Alpha a, WellScoped (Name a) a) => WellScoped (Name a) (Loc a)

pointsToRhsLens :: Lens' (PointsTo a) a
pointsToRhsLens =
  lens (\(_ :-> y) -> y)
       (\(x :-> y) z -> x :-> z)

-- data Loc = LocName :+ Int
data Loc a = a :+ Int
  deriving (Show, Functor, Eq, Ord, Generic)

instance NFData a => NFData (PointsTo a)
instance NFData a => NFData (Loc a)

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

instance (Typeable a, Alpha a) => WellScoped (Name a) (Allocation a)

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

instance (IsBase a, Validity a) => Validity (PointsTo a) where
  validate (lhs :-> rhs) =
    validate lhs <>
    validate rhs

instance IsBase a => Validity (Loc a) where
  validate (x :+ _) =
    check (isVar x) "Loc base name is variable"

validateWithRhs :: (IsBase a, Validity a) => (a -> Validation) -> PointsTo a -> Validation
validateWithRhs f p@(_ :-> y) = validate p <> f y

genValidPointsTo :: HasVar a => [Name a] -> (Int -> Gen a) -> Int -> Gen (PointsTo a)
genValidPointsTo names genA size = do
    loc <- genValidLoc names
    rhs <- genA (size `div` 2)
    pure (loc :-> rhs)

-- | Restrict so that it avoids generating LHS locations
-- in the given list
genValidPointsToRestricted :: HasVar a =>
  [Loc (Name a)] ->
  [Name a] ->
  (Int -> Gen a) ->
  Int -> Gen (PointsTo a)
genValidPointsToRestricted usedLocs names genA size = do
  loc <- genValidLocNotIn usedLocs names
  rhs <- genA (size-1)
  pure (loc :-> rhs)

genValidLoc :: HasVar a => [Name a] -> Gen (Loc a)
genValidLoc names = do
  i <- choose (0, 20)
  v <- mkVar <$> elements names
  pure (v :+ i)

genValidLocNotIn :: HasVar a => [Loc (Name a)] -> [Name a] -> Gen (Loc a)
genValidLocNotIn usedLocs names = do
  let choices =
        filter (`notElem` usedLocs) $
        liftA2 (:+) names [0..20]
  loc <- elements choices
  pure $ fmap mkVar loc

genValidAllocations :: IsName a a => [PointsTo a] -> Gen [Allocation a]
genValidAllocations = mapM genValidAllocation

genValidAllocation :: IsName a a => PointsTo a -> Gen (Allocation a)
genValidAllocation (_ :-> rhs) = do
  genValidAllocationForName rhs

genValidAllocationForName :: IsName a a => a -> Gen (Allocation a)
genValidAllocationForName n = do
  sz <- choose (0, 5)
  pure (Alloc (getName n) sz)

