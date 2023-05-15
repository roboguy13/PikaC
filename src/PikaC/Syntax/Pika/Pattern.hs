module PikaC.Syntax.Pika.Pattern
  where

import PikaC.Syntax.Pika.Expr

import GHC.Stack

data Pattern a =
  Pattern
    { patConstructor :: String
    , patVars :: [a]
    }
  deriving (Show)

type PatSubst a b = [(a, b)]

patternMatch :: HasCallStack => Pattern a -> (String, [b]) -> Maybe (PatSubst a b)
patternMatch pat (fn, xs)
  | patConstructor pat == fn =
      if length xs /= length (patVars pat)
        then error "Pattern argument lengths do not match"
        else Just $ zip (patVars pat) xs
  | otherwise = Nothing

