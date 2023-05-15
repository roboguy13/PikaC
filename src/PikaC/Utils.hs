module PikaC.Utils
  where

import qualified Data.Set as Set

import Data.Set (Set)

import Data.Bifunctor
import Data.List

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

