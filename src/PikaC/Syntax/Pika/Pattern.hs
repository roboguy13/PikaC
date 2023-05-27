{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PikaC.Syntax.Pika.Pattern
  where

import PikaC.Ppr
import PikaC.Utils

import GHC.Stack

import Data.Functor.Compose
import Control.Monad

import Control.Lens
import Control.Lens.TH

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Bind

import GHC.Generics

import Data.Typeable

data Pattern a
  = PatternVar (Name a)
  | Pattern String [Name a]
  deriving (Show, Generic)

makeLenses ''Pattern

instance HasNames (Pattern a) a where
  getNames (PatternVar v) = [v]
  getNames (Pattern _ vs) = vs

newtype PatternMatch a =
  PatternMatch (Bind (Pattern a) a)
  deriving (Show)

newtype PatternMatches a =
  PatternMatches (Bind [Pattern a] a)
  deriving (Show)

patternMatchesPats :: PatternMatches a -> [Pattern a]
patternMatchesPats (PatternMatches (B pats _)) = pats

applyPatternMatch :: (Typeable a, Alpha a, HasApp a, Subst a a) => PatternMatch a -> String -> [a] -> Either String a
applyPatternMatch match@(PatternMatch (B pat body)) constructor xs =
  case pat of
    PatternVar v -> Right $ substBind (B v body) (mkApp constructor xs)
    Pattern constructor2 vs
      | length vs /= length xs -> Left $ "applyPatternMatch: Wrong number of constructor arguments. Expected " ++ show (length vs) ++ ", got " ++ show (length xs) ++ "\n" ++ show match
      | constructor2 == constructor -> Right $ instantiate (B vs body) xs
      | otherwise -> Left $ "applyPatternMatch: Pattern constructors do not match: Expected: " ++ constructor ++ ", found: " ++ constructor2

applyPatternMatch' :: (HasCallStack, Typeable a, Alpha a, HasApp a, Subst a a) => PatternMatch a -> String -> [a] -> a
applyPatternMatch' match constructor xs =
  case applyPatternMatch match constructor xs of
    Left e -> error e
    Right r -> r

applyPatternMatches :: (Show a, Typeable a, Alpha a, HasApp a, Subst a a) => PatternMatches a -> [(String, [a])] -> Either String a
applyPatternMatches matches@(PatternMatches (B pats body)) args
  | length pats /= length args = Left $ "applyPatternMatches: Expected " ++ show (length pats) ++ " patterns, got " ++ show (length args) ++ " arguments: " ++ show (matches, args)
  | otherwise = do
    (names, xs) <- unzip . concat <$> zipWithM (uncurry . patternMatchSubst) pats args
    pure (instantiate (B names body) xs)

applyPatternMatches' :: (Show a, Typeable a, Alpha a, HasApp a, Subst a a) => PatternMatches a -> [(String, [a])] -> a
applyPatternMatches' matches args =
  case applyPatternMatches matches args of
    Left e -> error e
    Right r -> r

instance (Typeable a) => Alpha (Pattern a)

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

