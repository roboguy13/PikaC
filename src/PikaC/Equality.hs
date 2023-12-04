--
-- Use union-find to query a collection of equations between names
--

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module PikaC.Equality
  where

import Unbound.Generics.LocallyNameless

import Data.Equivalence.Monad

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

