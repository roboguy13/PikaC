{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module PikaC.Preorder.Preorder
  (Le (..)
  ,Preorder
  ,preorder
  ,CompareResult (..)
  ,compareP
  ,opposite
  ,toOrdering
  ,preorderElements
  ,pMax

  ,topologicalSort

  ,Contains (..)
  ,getContainsLabel
  ,getContainsChildren
  ,immediatelyLe
  ,immediatelyGe
  ,leastElems
  ,greatestElems
  ,validContains
  ,validContainsList

  ,preorderToContains
  ,containsToPreorder
  ,getSymmetricElements
  )
  where

import Data.List
import Data.Foldable

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog.Gen (element)

import qualified Data.Set as Set

import Control.Applicative

import PikaC.Utils

import GHC.Generics

import Data.Ord
import Data.Maybe

topologicalSort :: Ord a => [(a, a)] -> [(a, a)]
topologicalSort orig =
  filter (`elem` orig) .
  map go .
  getPreorder .
  containsToPreorder .
  preorderToContains .
  preorder $
  map (uncurry (:<=)) orig
  where
    go (x :<= y) = (x, y)

data Le a = a :<= a
  deriving (Show, Eq, Ord, Foldable, Generic)

data Preorder a = Preorder { rawPreorderElements :: [a], getPreorder :: [Le a] }
  deriving (Show, Eq)

opposite :: Preorder a -> Preorder a
opposite (Preorder elems p) = Preorder elems $ map flipR p
  where
    flipR (x :<= y) = y :<= x

-- | Input: Generators for the preorder (the preorder is the reflexive transitive closure of these)
--   Output: The transitive closure of the generators
preorder :: Ord a => [Le a] -> Preorder a
preorder xs = Preorder elems . fastNub $ removeRefls $ preorder' xs
  where
    elems = fastNub $ concatMap toList xs

preorder' :: Ord a => [Le a] -> [Le a]
preorder' = concatMap (uncurry extension) . removeOne

-- includeRefls :: Ord a => [Le a] -> [Le a]
-- includeRefls xs = map go (preorderElements (Preorder xs)) ++ xs
--   where
--     go x = x :<= x

-- | Removes elements of the form (a :<= a).
removeRefls :: Eq a => [Le a] -> [Le a]
removeRefls [] = []
removeRefls (r@(x :<= y) : rest)
  | x == y = removeRefls rest
  | otherwise = r : removeRefls rest


-- | Extend a list of Le pairs by one pair and, importantly, generate the
-- transitive closure
extension :: forall a. Ord a => Le a -> [Le a] -> [Le a]
extension (x :<= y) pairs0 =
  (x :<= y) :
    (extendBy (x :<= y) =<< removeOneSuchThat (\(w :<= _) -> w == y) pairs0)
  where
    extendBy :: Le a -> (Le a, [Le a]) -> [Le a]
    extendBy (x :<= y) (w :<= z, pairs) =
      let candidate = x :<= z
      in
      if candidate `notElem` pairs
        then extension candidate pairs
        else []

data CompareResult = Eq | Le | Ge | Incomparable
  deriving (Show, Eq)

compareP :: Eq a => Preorder a -> a -> a -> CompareResult
compareP p x y
  | x == y = Eq
  | (x :<= y) `elem` getPreorder p =
      if (y :<= x) `elem` getPreorder p
        then Eq
        else Le
  | (y :<= x) `elem` getPreorder p = Ge
  | otherwise = Incomparable

toOrdering :: CompareResult -> Maybe Ordering
toOrdering Eq = Just EQ
toOrdering Le = Just LT
toOrdering Ge = Just GT
toOrdering Incomparable = Nothing

le :: Eq a => Preorder a -> a -> a -> Bool
le p x y = compareP p x y == Le || compareP p x y == Eq

-- | Get elements <= the given value
ltElems :: Ord a => Preorder a -> a -> [a]
ltElems p x = filter (\y -> le p y x && compareP p x y /= Eq) (preorderElements p)

preorderElements :: Ord a => Preorder a -> [a]
preorderElements = fastNub . rawPreorderElements

-- | Group together elements of the preorder that are "equal" under the
-- preorder. That is, elements x and y get grouped if both x <= y and y <= x hold
preorderGroups ::  Ord a => Preorder a -> [[a]]
preorderGroups p@(Preorder _ pairs) = fastNub $ map (fastNub . go) elems
  where
    elems = preorderElements p

    go x = filter (\y -> compareP p x y == Eq) elems

data Contains a = Contains a [Contains a]
  deriving (Show, Eq, Ord, Foldable)

getContainsLabel :: Contains a -> a
getContainsLabel (Contains x _) = x

getContainsChildren :: Contains a -> [Contains a]
getContainsChildren (Contains _ ys) = ys

containedLabels :: Contains a -> [a]
containedLabels = concatMap go . getContainsChildren
  where
    go (Contains _ ys) = map getContainsLabel ys ++ concatMap go ys

pMax :: Eq a => Preorder a -> a -> a -> Maybe a
pMax p x y =
  case compareP p x y of
    Le -> Just y
    Ge -> Just x
    Eq -> Just x
    Incomparable -> Nothing

leastElems :: Ord a => Preorder a -> [a]
leastElems p = go elems
  where
    elems = preorderElements p

    go [] = []
    go (x : xs)
      | all (\y -> compareP p y x /= Le) elems = x : go xs
      | otherwise                              = go xs

greatestElems :: Ord a => Preorder a -> [a]
greatestElems = leastElems . opposite

compareReprs :: Eq a => Preorder a -> a -> a -> Ordering
compareReprs p x y =
  fromMaybe LT $ toOrdering (compareP p x y)

immediatelyLe :: Ord a => Preorder a -> a -> a -> Bool
immediatelyLe p x y =
  compareP p x y == Le
    &&
  all cond (preorderElements p)
  where
    pairs = getPreorder p
    -- TODO: How should we handle symmetry?
    cond z =
      not (z /= x && z /= y && (x :<= z) `elem` pairs && (z :<= y) `elem` pairs)

immediatelyGe :: Ord a => Preorder a -> a -> a -> Bool
immediatelyGe p = immediatelyLe (opposite p)

preorderToContains :: forall a. Ord a => Preorder a -> [Contains a]
preorderToContains p =
    let groups = sortBy (flip (compareReprs p)) (greatestElems p)
        r = go groups
        refls = map (`Contains` []) (filter (`notElem` concatMap toList r) (preorderElements p))
    in
    fastNub $ refls ++ go groups
  where

    go [] = []
    go (repr0:rest) =
      go1 repr0 : go (filter (\y -> compareP p repr0 y == Incomparable) rest)
      where
        containedIn x = filter (\y -> immediatelyLe p y x) $ preorderElements p

        go1 repr =
          Contains repr (go (containedIn repr))

containsToPreorder :: Ord a => [Contains a] -> Preorder a
containsToPreorder z =
    let Preorder _ r = preorder $ concatMap go z
    in
    Preorder (fastNub (concatMap toList z)) r
  where
    go (Contains x ys) =
      map ((:<= x) . getContainsLabel) ys
        ++
      concatMap go ys

-- | There is a pair of elements a, b such that a :<= b and b :<= a
getSymmetricElements :: Eq a => Preorder a -> [(a, a)]
getSymmetricElements = getSymmetricElements' . getPreorder

getSymmetricElements' :: Eq a => [Le a] -> [(a, a)]
getSymmetricElements' xs0 =
  map to $ filter cond xs0
  where
    to (x :<= y) = (x, y)

    cond (x :<= y) = (y :<= x) `elem` xs0

-- Properties --

prop_preorder_trans_closure :: Property
prop_preorder_trans_closure = property $ do
  list <- forAll (genLeList Gen.alpha)

  isTransitive (getPreorder (preorder list)) === True

isTransitive :: Eq a => [Le a] -> Bool
isTransitive orig_xs = go orig_xs
  where
    go [] = True
    go ((x :<= y):rest) =
      let candidateZs = map (\(_ :<= z) -> z) $ filter (cond y) rest
      in
      all (inOrig x) candidateZs

    inOrig x z = (x :<= z) `elem` orig_xs

    cond y (w :<= _) = w == y

prop_containsToPreorder_preorderToContains = property $ do
  p <- forAll $ genPreorder Gen.alpha 
  containsToPreorder (preorderToContains p) === p

prop_preorderToContains_containsToPreorder = property $ do
  cs <- forAll $ genContainsList (Range.linear 0 3) (Range.linear 0 3) Gen.alpha
  preorderToContains (containsToPreorder cs) === cs

prop_containsListDecreasing = property $ do
  p <- forAll $ genPreorder Gen.alpha
  containsListDecreasing p (preorderToContains p) === True

prop_containsEverythingLess = property $ do
  p <- forAll $ genPreorder Gen.alpha
  mapM_ (containsEverythingLess p) (preorderToContains p)

prop_containsEverythingLess' p =
  Group "test" [("prop_containsEverythingLess", property $ mapM_ (containsEverythingLess p) (preorderToContains p))]

-- example :: Preorder Char
-- example = preorder ['A' :<= 'B','A' :<= 'C','D' :<= 'C','E' :<= 'C']

preorderProps :: Group
preorderProps =
  Group "Preorder"
    [("prop_preorder_trans_closure", prop_preorder_trans_closure)
    ,("prop_containsToPreorder_preorderToContains", prop_containsToPreorder_preorderToContains)
    ,("prop_preorderToContains_containsToPreorder", prop_preorderToContains_containsToPreorder)
    ,("prop_containsListDecreasing", prop_containsListDecreasing)
    ,("prop_containsEverythingLess", prop_containsEverythingLess)
    ]

containsListDecreasing :: Eq a => Preorder a -> [Contains a] -> Bool
containsListDecreasing p = all (containsDecreasing p)

containsDecreasing :: Eq a => Preorder a -> Contains a -> Bool
containsDecreasing p (Contains x ys) =
  all (le p x) (concatMap containedLabels ys)
    && all (containsDecreasing p) ys

containsEverythingLess :: (Show a, Ord a, MonadTest m) => Preorder a -> Contains a -> m ()
containsEverythingLess p (Contains x ys) = do
  let labels = concatMap toList ys
      assertLabel a = annotate (show a ++ " not in contained labels; looking at node " ++ show x ++ "; labels are " ++ show labels) *>
                      assert (a `elem` labels)
  mapM_ assertLabel (ltElems p x)
  mapM_ (containsEverythingLess p) ys
  -- mapM_ (assert . 
  -- all (`elem` labels) (ltElems p x)
  --   && all (containsEverythingLess p) ys

genLeList :: Eq a => Gen a -> Gen [Le a]
genLeList g = 
  Gen.filter (null . getSymmetricElements') $
  liftA2 (liftA2 (:<=)) (nonEmpty' (Range.linear 0 10) g) (nonEmpty' (Range.linear 0 10) g)

genPreorder :: Ord a => Gen a -> Gen (Preorder a)
genPreorder g = fmap preorder (genLeList g)

genContains :: Ord a => Range Int -> Gen a -> Gen (Contains a)
genContains r g =
  Gen.filter validContains $
  Gen.recursive Gen.choice
    [ Contains <$> g <*> pure []
    ]
    [ Contains <$> g <*> Gen.list r (genContains r g)
    ]

genContainsList :: Ord a => Range Int -> Range Int -> Gen a -> Gen [Contains a]
genContainsList listR containsR g =
  Gen.filter validContainsList $ Gen.list listR (genContains containsR g)

-- uniqueLabels :: Ord a => Contains a -> Bool
-- uniqueLabels p = containedLabels p == fastNub (containedLabels p)

validContainsList :: Ord a => [Contains a] -> Bool
validContainsList cs =
  levelLabels == fastNub levelLabels
    &&
  all (validContainsList . getContainsChildren) cs
    &&
  go cs
  where
    levelLabels = map getContainsLabel cs

    go [] = True
    go (x:xs) =
      null (toList x `intersect` concatMap toList xs)
        &&
      go xs

validContains :: Eq a => Contains a -> Bool
validContains = containsNoSym

-- | Nothing corresponding to symmetric elements
containsNoSym :: Eq a => Contains a -> Bool
containsNoSym (Contains x xs) =
  x `notElem` concatMap toList xs
    &&
  all containsNoSym xs

nonEmpty' :: Range Int -> Gen a -> Gen [a]
nonEmpty' r g = toList <$> Gen.nonEmpty r g

