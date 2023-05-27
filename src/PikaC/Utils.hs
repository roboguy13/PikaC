{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

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

-- openBind :: (IsName a b, HasVar b, Alpha a, Alpha b, Subst b b) => Bind [a] b -> b
openBind :: (Alpha a1, Alpha b, Alpha a2, Subst a1 b, HasVar a1, HasNames a2 a1) =>
     Bind [a2] b -> b
openBind bnd@(B vs _) =
  instantiate bnd (concatMap (map mkVar . getNames) vs)

freshOpen :: (Fresh m, Alpha a1, Alpha b, Alpha a2, Subst a1 b, HasVar a1, HasNames a2 a1) =>
     Bind [a2] b -> m b
freshOpen bnd@(B vs _) = do
  vs' <- mapM fresh (concatMap getNames vs)
  pure (instantiate bnd (map mkVar vs'))

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

class HasVar a where
  mkVar :: Name a -> a

class IsName a b | a -> b where
  getName :: a -> Name b

instance IsName (Name a) a where
  getName = id

class HasNames a b | a -> b where
  getNames :: a -> [Name b]

instance HasNames (Name a) a where
  getNames x = [x]

-- TODO: Deal with these orphan instances
-- deriving instance (Data a, Data b) => Data (Bind a b)
-- deriving instance Data a => Data (Name a)
-- instance (Data a, Data b) => Plated (Bind a b)
-- deriving instance Traversable (Bind a)
-- deriving instance Functor (Bind a)
-- deriving instance Foldable (Bind a)

