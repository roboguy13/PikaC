{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}

module PikaC.Utils
  where

import qualified Data.Set as Set

import Data.Set (Set)

import Data.Bifunctor
import Data.List

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Bind
import Unbound.Generics.LocallyNameless.Name
import Unbound.Generics.PermM

import Data.Typeable
import Data.Data

import Control.Lens

-- rename :: forall a b. (Typeable a, Subst a b) => [(Name a, Name a)] -> b -> b
-- rename = substs

-- | Actually swaps the names in each pair
rename :: forall a b. (Typeable a, Alpha b) => [(Name a, Name a)] -> b -> b
rename pairs = swaps (go pairs)
  where
    go :: [(Name a, Name a)] -> Perm AnyName
    go [] = mempty
    go ((x, y) : rest) =
      single (AnyName x) (AnyName y) <> go rest

renameMaybe :: forall a b. (Typeable a, Alpha b) => Maybe [(Name a, Name a)] -> b -> b
renameMaybe Nothing = id
renameMaybe (Just rho) = rename rho

fastNub :: Ord a => [a] -> [a]
fastNub = Set.toList . Set.fromList

removeOne :: [a] -> [(a, [a])]
removeOne = removeOneSuchThat (const True)

removeOneSuchThat :: (a -> Bool) -> [a] -> [(a, [a])]
removeOneSuchThat p [] = []
removeOneSuchThat p (x:xs)
  | p x = (x, xs) : map (second (x:)) (removeOneSuchThat p xs)
  | otherwise = map (second (x:)) (removeOneSuchThat p xs)

remove :: (a -> Maybe b) -> [a] -> Maybe (b, [a])
remove f [] = Nothing
remove f (x:xs) =
  case f x of
    Just y -> Just (y, xs)
    Nothing -> fmap (second (x:)) (remove f xs)

lookupRemove :: Eq a => a -> [(a, b)] -> Maybe (b, [(a, b)])
lookupRemove x = remove $ \(lhs, rhs) ->
  if lhs == x
    then Just rhs
    else Nothing

countOccurrences :: Ord a => [a] -> Set (a, Int)
countOccurrences = Set.fromList . go []
  where
    go acc [] = acc
    go acc (x:xs) =
      case lookupRemove x acc of
        Nothing -> go ((x, 1) : acc) xs
        Just (n, acc') -> go ((x, n+1) : acc') xs

-- TODO: Deal with these orphan instances
-- deriving instance (Data a, Data b) => Data (Bind a b)
-- deriving instance Data a => Data (Name a)
-- instance (Data a, Data b) => Plated (Bind a b)
-- deriving instance Traversable (Bind a)
-- deriving instance Functor (Bind a)
-- deriving instance Foldable (Bind a)

