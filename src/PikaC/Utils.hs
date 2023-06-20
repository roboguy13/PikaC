{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DefaultSignatures #-}

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

import Unbound.Generics.LocallyNameless.Unsafe

import Data.Typeable
import Data.Data

import Control.Lens hiding (elements, from)

import Control.Monad

import GHC.Stack

import Data.Kind

import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import GHC.Generics

import Data.Validity

import Data.Char

strength :: Functor f => (a, f b) -> f (a, b)
strength (a, fb) =
  fmap (\x -> (a, x)) fb

-- -- TODO: Write property tests for this
-- joinBind :: (Typeable a, Fresh m, Alpha a, Alpha b, HasVar a) => Bind [Name a] (Bind [Name a] b) -> m (Bind [Name a] b)
-- joinBind bnd1 = do
--   (vars1, bnd2) <- unbind bnd1
--   (vars2, body) <- unbind bnd2
--   vars2' <- mapM fresh vars2
--   let body' = rename (zip vars2 vars2') body
--   pure $ bind (vars1 ++ vars2') body'

-- openBind :: (IsName a b, HasVar b, Alpha a, Alpha b, Subst b b) => Bind [a] b -> b
openBind :: (Alpha a1, Alpha b, Alpha a2, Subst a1 b, HasVar a1, HasNames a2 a1) =>
     Bind [a2] b -> b
openBind bnd@(B vs _) =
  instantiate bnd (concatMap (map mkVar . getNames) vs)


openBind' :: forall a1 a2 b. (HasVars a1 [a2], Subst a2 b, Alpha a1, Alpha b, Alpha a2, Subst a1 b,
      HasNames a2 a1) =>
     Bind a2 b -> b
openBind' bnd@(B v _) =
  instantiate bnd (mkVars @a1 @[a2] (getNames v))

-- openBind1 :: (Typeable b1, Alpha b2, Alpha a, HasVar b1, Subst (Name b1) b2, HasNames a b1) =>
  -- Bind a b2 -> b2
openBind1
  :: (Alpha a1, Alpha b, Alpha a2, Subst a1 b, HasVar a1,
      HasNames a2 a1) =>
     Bind a2 b -> b
openBind1 bnd@(B v _) =
  instantiate bnd (map mkVar (getNames v))

freshOpen :: forall m f a1 a2 b. (Fresh m, Alpha a1, Alpha b, Alpha a2, Subst a1 b, HasVar a1, HasNames a2 a1, FromName f) =>
     Bind [a2] b -> m ([f a1], b)
freshOpen bnd@(B vs _) = do
  vs' <- mapM fresh (concatMap getNames vs)
  pure (map fromName vs', instantiate bnd (map mkVar vs'))

-- rename :: forall a b. (Typeable a, Subst a b) => [(Name a, Name a)] -> b -> b
-- rename = substs

-- | Actually swaps the names in each pair
-- TODO: Implement this with 'substs' to make this faster
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

class HasVars a b where
  mkVars :: [Name a] -> b

class IsName a b | a -> b where
  getName :: HasCallStack => a -> Name b

instance IsName (Name a) a where
  getName = id

class HasNames a b | a -> b where
  getNames :: a -> [Name b]

class ConvertibleNames a b where
  getNamesAs :: a -> [b]

instance a ~ b => ConvertibleNames (Name a) (Name b) where
  getNamesAs x = [x]

instance a ~ b => ConvertibleNames [Name a] (Name b) where
  getNamesAs = id

class IsBase a where
  isVar :: a -> Bool
  isLit :: a -> Bool
  intLit :: Int -> a
  boolLit :: Bool -> a
  mkNot :: a -> a
  mkEqual :: a -> a -> a
  mkAnd :: a -> a -> a

isBase :: IsBase a => a -> Bool
isBase x = isVar x || isLit x

instance HasNames (Name a) a where
  getNames x = [x]

instance HasNames a b => HasNames [a] b where
  getNames = concatMap getNames

class FromName f where
  fromName :: Name a -> f a

instance FromName Name where
  fromName = id

getBv :: Bind a b -> a
getBv (B v _) = v

isClosed :: forall a (b :: Type). (Alpha a, Typeable a, Typeable b) => a -> Bool
isClosed = null . toListOf @(Name b) (fv @a)

qcSubseqs :: [a] -> [[a]]
qcSubseqs = drop 1 . init . subsequences

newtype IsClosed a b = IsClosed (Bind a b)
  deriving (Show, Generic)

-- -- | Closed "relative" to the list of Names
-- data RelativelyClosed a b =
--   RelativelyClosed
--     [Name a]
--     (

instance (Typeable a, Typeable b, Alpha a, Alpha b) => Validity (IsClosed a b) where
  validate (IsClosed bnd) =
    check (null (toListOf (fv @(Bind a b) @_ @a) bnd))
      "is closed"

-- class Binder n a where
--   getVarsBoundBy :: a -> [Name n]
--
-- instance HasNames a n => Binder n (Bind a b) where
--   getVarsBoundBy (B v _) = getNames v

-- class (Typeable n, Alpha n, Alpha a) => WellScoped n a | a -> n where
class (Typeable a, Alpha a) => WellScoped n a where
  wellScoped :: [n] -> a -> Validation

  default wellScoped :: (Generic a, GWellScoped n (Rep a)) =>
    [n] -> a -> Validation
  wellScoped inScopes = gWellScoped inScopes . from

instance (Typeable a, Alpha a, WellScoped (Name a) b) => WellScoped (Name a) (Bind [Name a] b) where
  wellScoped inScopeVars bnd =
    let (v, body) = unsafeUnbind bnd
        bodyFvs = toListOf (fv @b @_ @a) body
        everyInScope = inScopeVars ++ v
    in
    check (all @[] (`elem` everyInScope) bodyFvs)
      "Well scoped (WellScoped instance)"
      <> wellScoped everyInScope body

instance (Alpha a, Typeable a) => WellScoped (Name a) (Name a) where
  wellScoped inScopeVars v =
    decorate ("searching for " ++ show v ++ " in " ++ show inScopeVars) $
    check (isFreeName v)
      "Only free variables when using WellScoped"
    <>
    check (v `elem` inScopeVars)
      "Well scoped (WellScoped instance for Name)"

instance WellScoped a b => WellScoped a [b]

instance WellScoped a Int where wellScoped _ _ = mempty
instance WellScoped a Integer where wellScoped _ _ = mempty
instance WellScoped a Char where wellScoped _ _ = mempty
instance WellScoped a Bool where wellScoped _ _ = mempty

class GWellScoped n f where
  gWellScoped :: [n] -> f a -> Validation

instance GWellScoped n U1 where
  gWellScoped _ _ = mempty

instance GWellScoped n V1 where
  gWellScoped _ _ = mempty

instance (GWellScoped n a, GWellScoped n b) =>
    GWellScoped n (a :*: b) where
  gWellScoped inScopes (x :*: y) =
    gWellScoped inScopes x <> gWellScoped inScopes y

instance (GWellScoped n a, GWellScoped n b) =>
    GWellScoped n (a :+: b) where
  gWellScoped inScopes (L1 x) = gWellScoped inScopes x
  gWellScoped inScopes (R1 y) = gWellScoped inScopes y


instance (GWellScoped n a, Datatype c) => GWellScoped n (M1 D c a) where
  gWellScoped inScopes m1 = gWellScoped inScopes (unM1 m1)

instance (GWellScoped n a, Constructor c) => GWellScoped n (M1 C c a) where
  gWellScoped inScopes m1 = gWellScoped inScopes (unM1 m1) `annotateValidation` conName m1

instance (GWellScoped n a, Selector c) => GWellScoped n (M1 S c a) where
  gWellScoped inScopes m1 = gWellScoped inScopes (unM1 m1) `annotateValidation` selName m1

instance (WellScoped n a) => GWellScoped n (K1 R a) where
  gWellScoped inScopes (K1 x) = wellScoped inScopes x

annotateValidation = flip decorate

instance (Alpha a, Alpha b, Arbitrary b) => Arbitrary (Bind a b) where
  arbitrary = error "Arbitrary (Bind a b)"
  shrink bnd =
    let (vars, body) = unsafeUnbind bnd
        body' = shrink body
    in
    bind vars <$> body'

instance Arbitrary (Name a) where
  arbitrary = error "Arbitrary (Name a)"
  shrink _ = []

arbitraryAlpha :: Gen Char
arbitraryAlpha = arbitrary `suchThat` (`elem` ['a'..'z'])

arbitraryUppercase :: Gen Char
arbitraryUppercase = arbitrary `suchThat` (`elem` ['A'..'Z'])

arbitraryAnyCase :: Gen Char
arbitraryAnyCase =
  oneof
  [ arbitraryAlpha
  , arbitraryUppercase
  ]

genNameString :: Gen String
genNameString = do
  n <- chooseInt (1, 4)
  replicateM n arbitraryAlpha

noDups :: Ord a => [a] -> Bool
noDups xs = sort (fastNub xs) == sort xs

disjoint :: Eq a => [a] -> [a] -> Bool
disjoint xs ys = null (xs `intersect` ys)

elements' :: HasCallStack => [a] -> Gen a
elements' [] = error "elements' []"
elements' xs = elements xs

-- Get N items from the given list, also return the rest
useNItems :: Int -> [a] -> Gen ([a], [a])
useNItems n [] = pure ([], [])
useNItems n xs = do
  xs' <- shuffle xs
  pure $ splitAt n xs'

discardM :: Gen a
discardM = discard --pure undefined `suchThat` const False

isConstructor :: String -> Bool
isConstructor = isUpper . head

-- deriving instance (Data a, Data b) => Data (Bind a b)
-- deriving instance Data a => Data (Name a)
-- instance (Data a, Data b) => Plated (Bind a b)
-- deriving instance Traversable (Bind a)
-- deriving instance Functor (Bind a)
-- deriving instance Foldable (Bind a)

