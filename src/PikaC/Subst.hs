module PikaC.Subst
  where

import Bound

type Env a b = [(a, b)]

class Subst f where
  naiveSubstitute :: Env a (f a) -> f a -> f a
  rename :: Env a a -> f a -> f a
  getFreeVars :: f a -> [a]

substitute :: (Variable a, Subst f) =>
  Env a (f a) -> f a -> f a
substitute = undefined

class Eq a => Variable a where
  varIncrement :: a -> a

fresh :: (Variable a, Subst f) => a -> f a -> a
fresh x fa =
  if x `elem` getFreeVars fa
    then fresh (varIncrement x) fa
    else x

