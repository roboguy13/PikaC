{-# LANGUAGE ScopedTypeVariables #-}

module PikaC.Syntax.Pika.Pattern
  where

import PikaC.Syntax.Pika.Expr
import PikaC.Ppr

import GHC.Stack

import Bound

import Data.Functor.Compose
import Control.Monad

data PatternMatch f a b =
  PatternMatch
    { patConstructor :: String
    , patRhs :: Scope a f b
    , patVars :: [a]
    }

instance Functor f => Functor (PatternMatch f a) where
  fmap f (PatternMatch c rhs vars) =
    PatternMatch c (fmap f rhs) vars

type PatternMatch' f a = PatternMatch f a a

patternMatch :: (Eq a, Show a, Monad f) => PatternMatch f a b -> String -> [f b] -> Maybe (f b)
patternMatch pat constructor xs
  | patConstructor pat /= constructor = Nothing
  | otherwise = Just $ instantiate go (patRhs pat)
    where
      bnds = zip (patVars pat) xs
      go x =
        case lookup x bnds of
          Nothing -> error $ "Cannot find pattern variable " ++ show x ++ " in pattern for " ++ constructor
          Just r -> r

data PatternMatches f a b
  = NoPatterns (f b)
  | Pat (PatternMatch (PatternMatches f a) a b)

instance Functor f => Functor (PatternMatches f a) where
  fmap f (NoPatterns x) = NoPatterns (fmap f x)
  fmap f (Pat m) = Pat $ fmap f m

instance Monad f => Applicative (PatternMatches f a) where
  pure = NoPatterns . pure
  (<*>) = ap

instance Monad f => Monad (PatternMatches f a) where
  return = pure
  NoPatterns x >>= f = NoPatterns (x >>>= f)

-- patternMatch' :: forall f a b. (Eq a, Ppr (f b), Show a, Monad f) => PatternMatches f a b -> [(String, [f b])] -> f b
-- patternMatch' (NoPatterns fa) [] = fa
-- patternMatch' (NoPatterns fa) ((constructor, _):_) =
--   error $ "patternMatch': Expected no pattern match for " ++ ppr' fa ++ ", constructor " ++ constructor
-- patternMatch' (Pat match) ((constructor, xs) : xss) =
--   case patternMatch match constructor (map NoPatterns xs) of
--     Nothing -> error $ "patternMatch': Pattern match constructor does not match: " ++ constructor
--   where
--     -- go :: f a -> Compose (PatternMatches f) f a
--     -- go x = Compose (_ x)


-- type PatSubst a b = [(a, b)]
--
-- patternMatch :: HasCallStack => Pattern a -> (String, [b]) -> Maybe (PatSubst a b)
-- patternMatch pat (fn, xs)
--   | patConstructor pat == fn =
--       if length xs /= length (patVars pat)
--         then error "Pattern argument lengths do not match"
--         else Just $ zip (patVars pat) xs
--   | otherwise = Nothing
--
