{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

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

type family PType a

data Pattern a
  = PatternVar (Name (PType a))
  | Pattern
      { _patConstructor :: String
      , _patVars :: [Name (PType a)]
      }
  deriving (Show)

makeLenses ''Pattern

patternMatch :: Subst (PType a) b => Pattern a -> String -> [PType a] -> Maybe (b -> b)
patternMatch pat constructor xs
  | _patConstructor pat /= constructor = Nothing
  | otherwise = Just $ substs (zip (_patVars pat) xs)

patternMatch' :: Subst (PType a) b => Pattern a -> String -> [PType a] -> b -> b
patternMatch' pat constructor xs =
  case patternMatch pat constructor xs of
    Nothing -> error $ "patternMatch': Pattern constructors do not match: Expected: " ++ _patConstructor pat ++ ", found: " ++ constructor
    Just r -> r

