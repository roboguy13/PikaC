{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module PikaC.Syntax.Pika.Pattern
  where

import PikaC.Ppr
import PikaC.Utils
import PikaC.Syntax.Type

import GHC.Stack

import Data.Functor.Compose
import Control.Monad

import Control.Lens
import Control.Lens.TH

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Bind
import Unbound.Generics.LocallyNameless.Unsafe

import GHC.Generics

import Data.Typeable

import Test.QuickCheck

import Control.DeepSeq

data Pattern a
  = PatternVar (Name a)
  | Pattern String [Name a]
  deriving (Show, Generic)

instance Size (Pattern a) where
  size (PatternVar _) = 1
  size (Pattern _ xs) = 1 + size xs

instance NFData a => NFData (Pattern a)

makeLenses ''Pattern

instance IsNested (Pattern a) where
  isNested (PatternVar {}) = False
  isNested (Pattern {}) = True

instance (Typeable a, Alpha a, Typeable b, Alpha b, WellScoped (Name a) b) =>
    WellScoped (Name a) (Bind (Pattern a) b) where
  wellScoped inScopeVars bnd =
    let (pat, body) = unsafeUnbind bnd
    in
    wellScoped (inScopeVars ++ getNames pat) body

instance HasNames (Pattern a) a where
  getNames (PatternVar v) = [v]
  getNames (Pattern _ vs) = vs

newtype PatternMatch a b =
  PatternMatch { getPatternMatch :: Bind (Pattern a) b }
  deriving (Show, Generic)

newtype PatternMatches a b =
  PatternMatches { getPatternMatches :: Bind [Pattern a] b }
  deriving (Show, Generic)

instance Size b => Size (PatternMatch a b) where
  size (PatternMatch m) = size m

instance Size b => Size (PatternMatches a b) where
  size (PatternMatches xs) = size xs

instance (NFData a, NFData b) => NFData (PatternMatch a b)
instance (NFData a, NFData b) => NFData (PatternMatches a b)

instance (Typeable a, Alpha a, Typeable b, Alpha b, WellScoped (Name a) b) => WellScoped (Name a) (PatternMatch a b) where
  wellScoped inScopeVars (PatternMatch bnd) =
    wellScoped inScopeVars bnd

instance (Alpha a, Typeable a, WellScoped (Name a) a) => WellScoped (Pattern a) a where
  wellScoped inScopeVars x =
    wellScoped (concatMap getNames inScopeVars) x

instance (Typeable a, Alpha b) => Alpha (PatternMatch a b)
instance (Typeable a, Alpha b) => Alpha (PatternMatches a b)

instance (Typeable a, Typeable b, Alpha b, Arbitrary a, Arbitrary b) => Arbitrary (PatternMatches a b) where
  arbitrary = PatternMatches <$> arbitrary
  shrink = genericShrink

openPatternMatch :: (Fresh m, HasVar a, Subst a b, Alpha b, Alpha a, Typeable a) =>
  PatternMatch a b -> m ([Name a], b)
openPatternMatch (PatternMatch bnd) = do
  (pat, b) <- unbind bnd
  pure (getNames pat, b)
  -- freshOpen (B (getNames pat) body)

openPatternMatches :: (Fresh m, HasVar a, Subst a b, Alpha b, Alpha a, Typeable a) =>
  PatternMatches a b -> m ([[Name a]], b)
openPatternMatches (PatternMatches matches) = do
  (pats, b) <- unbind matches
  pure (map getNames pats, b)

patternMatchPat :: PatternMatch a b -> Pattern a
patternMatchPat (PatternMatch (B pat _)) = pat

patternMatchesPats :: PatternMatches a b -> [Pattern a]
patternMatchesPats (PatternMatches (B pats _)) = pats

lookupPatternMatch :: [PatternMatch a b] -> String -> Maybe (PatternMatch a b)
lookupPatternMatch [] pat = Nothing
lookupPatternMatch (x@(PatternMatch (B (PatternVar v) _)) : _) constructor = Just x
lookupPatternMatch (x@(PatternMatch (B (Pattern constructor' _) _)) : xs) constructor
  | constructor' == constructor = Just x
  | otherwise = lookupPatternMatch xs constructor

lookupPatternMatch' :: (HasCallStack, Show b) => [PatternMatch a b] -> String -> PatternMatch a b
lookupPatternMatch' matches constructor =
  case lookupPatternMatch matches constructor of
    Nothing -> error $ "Cannot find match for constructor " ++ constructor ++ " in " ++ show matches
    Just r -> r


onPattern :: Applicative f => (Name a -> f (Name b)) -> Pattern a -> f (Pattern b)
onPattern f (PatternVar v) = PatternVar <$> f v
onPattern f (Pattern c vs) = Pattern c <$> traverse f vs

onPatternMatch :: (Fresh m, Typeable a, Typeable a', Alpha b, Alpha b') =>
  (Name a -> m (Name a')) -> (b -> m b') -> PatternMatch a b -> m (PatternMatch a' b')
onPatternMatch varFn bodyFn (PatternMatch m) = do
  (pat, body) <- unbind m
  PatternMatch <$> (bind <$> onPattern varFn pat <*> bodyFn body)

onPatternMatches :: (Fresh m, Typeable a, Typeable a', Alpha b, Alpha b') =>
  (Name a -> m (Name a')) -> (b -> m b') -> PatternMatches a b -> m (PatternMatches a' b')
onPatternMatches varFn bodyFn (PatternMatches m) = do
  (pats, body) <- unbind m
  PatternMatches <$> (bind <$> mapM (onPattern varFn) pats <*> bodyFn body)

applyPatternMatch :: (Typeable a, Alpha a, HasApp a, Subst a b, Alpha b) => PatternMatch a b -> String -> [a] -> Either String b
applyPatternMatch match@(PatternMatch (B pat body)) constructor xs =
  case pat of
    PatternVar v -> Right $ substBind (B v body) (mkApp constructor xs)
    Pattern constructor2 vs
      | length vs /= length xs -> Left $ "applyPatternMatch: Wrong number of constructor arguments. Expected " ++ show (length vs) ++ ", got " ++ show (length xs) ++ "\n" ++ show match
      | constructor2 == constructor -> Right $ instantiate (B vs body) xs
      | otherwise -> Left $ "applyPatternMatch: Pattern constructors do not match: Expected: " ++ constructor ++ ", found: " ++ constructor2

applyPatternMatch' :: (HasCallStack, Typeable a, Alpha a, Alpha b, HasApp a, Subst a b) => PatternMatch a b -> String -> [a] -> b
applyPatternMatch' match constructor xs =
  case applyPatternMatch match constructor xs of
    Left e -> error e
    Right r -> r

applyPatternMatches :: (Show a, Typeable a, Alpha a, Alpha b, HasApp a, Subst a b) => PatternMatches a b -> [(String, [a])] -> Either String b
applyPatternMatches matches@(PatternMatches (B pats body)) args
  | length pats /= length args = Left $ "applyPatternMatches: Expected " ++ show (length pats) ++ " patterns, got " ++ show (length args) ++ " arguments: " ++ show (matches, args)
  | otherwise = do
    (names, xs) <- unzip . concat <$> zipWithM (uncurry . patternMatchSubst) pats args
    pure (instantiate (B names body) xs)

applyPatternMatches' :: (Show a, Typeable a, Alpha a, HasApp a, Subst a b, Alpha b) => PatternMatches a b -> [(String, [a])] -> b
applyPatternMatches' matches args =
  case applyPatternMatches matches args of
    Left e -> error e
    Right r -> r

instance (Typeable a) => Alpha (Pattern a)
-- instance Subst (Name a) (Pattern a)
-- instance Subst (Pattern a) AdtName

-- instance Subst a a => Subst a (Pattern a)
instance Subst a b => Subst a (Pattern b)
instance (Subst a a, Subst a b, Typeable a, Typeable b, Alpha a, Alpha b) => Subst a (PatternMatch a b)
instance (Subst a a, Subst a b, Typeable a, Typeable b, Alpha a, Alpha b) => Subst a (PatternMatches a b)




instance (Typeable a, Alpha b, Arbitrary b) => Arbitrary (PatternMatch a b) where
  arbitrary = error "Arbitrary PatternMatch"
  shrink = genericShrink

class HasApp a where
  mkApp :: String -> [a] -> a

getPatternNames :: Pattern a -> [Name a]
getPatternNames (PatternVar n) = [n]
getPatternNames (Pattern _ ns) = ns

instance Ppr a => Ppr (Pattern a) where
  ppr (PatternVar x) = ppr x
  ppr (Pattern constructor []) = text constructor
  ppr (Pattern constructor vars) =
    text "(" <> hsep (text constructor : map ppr vars) <> text ")"

patternMatchSubst :: (Show b, HasApp b) =>
     Pattern a -> String -> [b] -> Either String [(Name a, b)]
patternMatchSubst (PatternVar v) constructor xs =
  Right [(v, mkApp constructor xs)]
patternMatchSubst pat@(Pattern constructorP vars) constructor xs
  | length vars /= length xs = Left $ "patternMatch: Wrong number of constructor arguments. Expected " ++ show (length vars) ++ ", got " ++ show (length xs) ++ ":\nPattern: " ++ show pat ++ "\nConstructor application" ++ show (constructor, xs)
  | constructorP /= constructor =
      Left $ "patternMatch: Pattern constructors do not match: Expected: "++ constructorP ++ ", found: " ++ constructor
  | otherwise = Right (zip vars xs)

patternMatchSubst' :: (Show b, HasApp b) =>
     Pattern a -> String -> [b] -> [(Name a, b)]
patternMatchSubst' pat constructor xs =
  case patternMatchSubst pat constructor xs of
    Left err -> error err
    Right r -> r

patternMatch
  :: (Show b, Subst b a, HasApp b) =>
     Pattern b -> String -> [b] -> Either String (a -> a)
patternMatch pat constructor xs = substs <$> patternMatchSubst pat constructor xs
--   Right (subst v (mkApp constructor xs))
-- patternMatch (Pattern constructorP vars) constructor xs
--   | constructorP /= constructor =
--       Left $ "patternMatch: Pattern constructors do not match: Expected: "++ constructorP ++ ", found: " ++ constructor
--   | otherwise = Right $ substs (zip vars xs)

patternMatch'
  :: (Show b, Subst b a, HasApp b) => Pattern b -> String -> [b] -> a -> a
patternMatch' pat constructor xs =
  case patternMatch pat constructor xs of
    Left err -> error err
    Right r -> r

