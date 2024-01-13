--
-- Use union-find to query a collection of equations between names
--

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PikaC.Equality
  where

import Unbound.Generics.LocallyNameless

import Data.Equivalence.Monad
import Control.Arrow (second)

import Debug.Trace

class Equality a b | a -> b where
  getLHS :: a -> Name b
  getRHS :: a -> Name b

instance Equality (Name a, Name a) a where
  getLHS = fst
  getRHS = snd

mkEquiv :: Equality a b => [a] -> Equiv s b ()
mkEquiv eqs = do
  mapM_ go eqs
  where
    go eq = do
      lhsC <- getClass (getLHS eq)
      rhsC <- getClass (getRHS eq)
      combine lhsC rhsC

isSame :: Equality a b => Name b -> Name b -> Equiv s b Bool
isSame x y = do
  xD <- desc =<< getClass x
  yD <- desc =<< getClass y
  pure $ xD == yD
  where
    go eq = do
      lhsC <- getClass (getLHS eq)
      rhsC <- getClass (getRHS eq)
      combine lhsC rhsC

runEquiv :: (forall s. Equiv s b t) -> t
runEquiv = runEquivM (:[]) (++)

type Equiv s a = EquivM s [Name a] (Name a)

simplifyEqualities :: forall a. (Show a, Subst a a) => [(Name a, a)] -> [(Name a, a)]
simplifyEqualities = go 1
  where
    go i xs =
      case splitOff i xs of
        Nothing -> xs
        Just (ys, eqn, zs) ->
          go (i+1) (map (second (substEqn eqn)) ys ++ [eqn] ++ map (second (substEqn eqn)) zs)

    substEqn :: (Name a, a) -> a -> a
    substEqn (lhs, rhs) x =
      subst lhs rhs x

splitOff :: Int -> [a] -> Maybe ([a], a, [a])
splitOff i xs =
  let (ys, zs) = splitAt i xs
      reversedYs = reverse ys
  in
  case reversedYs of
    (ry:rys) ->
      if i > length xs -- TODO: Find a better way
      then Nothing
      else Just (reverse rys, ry, zs)
    [] -> Nothing

