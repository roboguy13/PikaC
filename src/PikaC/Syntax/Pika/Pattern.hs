{-# LANGUAGE ScopedTypeVariables #-}

module PikaC.Syntax.Pika.Pattern
  where

import PikaC.Syntax.Pika.Expr
import PikaC.Ppr

import GHC.Stack

import Bound

import Data.Functor.Compose
import Control.Monad

import Unbound.Generics.LocallyNameless

data Pattern =
  Pattern
    { patConstructor :: String
    , patVars :: [ExprName]
    }

patternMatch :: Pattern -> String -> [Expr] -> Maybe (Expr -> Expr)
patternMatch pat constructor xs
  | patConstructor pat == constructor = Nothing
  | otherwise = Just $ substs (zip (patVars pat) xs)

patternMatch' :: Pattern -> String -> [Expr] -> Expr -> Expr
patternMatch' pat constructor xs =
  case patternMatch pat constructor xs of
    Nothing -> error $ "patternMatch': Pattern constructors do not match: Expected: " ++ patConstructor pat ++ ", found: " ++ constructor
    Just r -> r

