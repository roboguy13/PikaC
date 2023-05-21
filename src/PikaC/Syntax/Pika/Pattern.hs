{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module PikaC.Syntax.Pika.Pattern
  where

import PikaC.Ppr

import GHC.Stack

import Bound

import Data.Functor.Compose
import Control.Monad

import Control.Lens
import Control.Lens.TH

import Unbound.Generics.LocallyNameless

import GHC.Generics

import Data.Typeable

type family PType a

data Pattern a
  = PatternVar (Name (PType a))
  | Pattern String [Name (PType a)]
      -- { _patConstructor :: String
      -- , _patVars :: [Name (PType a)]
      -- }
  deriving (Show, Generic)

makeLenses ''Pattern

instance (Typeable (PType a)) => Alpha (Pattern a)

class HasApp a where
  mkApp :: String -> [a] -> a

getPatternNames :: Pattern a -> [Name (PType a)]
getPatternNames (PatternVar n) = [n]
getPatternNames (Pattern _ ns) = ns

instance Ppr a => Ppr (Pattern a) where
  ppr (PatternVar x) = ppr x
  ppr (Pattern constructor []) = text constructor
  ppr (Pattern constructor vars) =
    text "(" <> hsep (text constructor : map ppr vars) <> text ")"

-- patternMatchSubst :: (HasApp (PType a), Subst (PType a) b) => Pattern a -> String -> [PType a] -> Either String [(Name (PType a), PType a)]
patternMatchSubst (PatternVar v) constructor xs =
  Right [(v, mkApp constructor xs)]
patternMatchSubst (Pattern constructorP vars) constructor xs
  | constructorP /= constructor =
      Left $ "patternMatch: Pattern constructors do not match: Expected: "++ constructorP ++ ", found: " ++ constructor
  | otherwise = Right (zip vars xs)

patternMatchSubst' pat constructor xs =
  case patternMatchSubst pat constructor xs of
    Left err -> error err
    Right r -> r

-- patternMatch :: (HasApp (PType a), Subst (PType a) b) => Pattern a -> String -> [PType a] -> Either String (b -> b)
patternMatch pat constructor xs = substs <$> patternMatchSubst pat constructor xs
--   Right (subst v (mkApp constructor xs))
-- patternMatch (Pattern constructorP vars) constructor xs
--   | constructorP /= constructor =
--       Left $ "patternMatch: Pattern constructors do not match: Expected: "++ constructorP ++ ", found: " ++ constructor
--   | otherwise = Right $ substs (zip vars xs)

-- patternMatch' :: (HasApp (PType a), Subst (PType a) b) => Pattern a -> String -> [PType a] -> b -> b
patternMatch' pat constructor xs =
  case patternMatch pat constructor xs of
    Left err -> error err
    Right r -> r

