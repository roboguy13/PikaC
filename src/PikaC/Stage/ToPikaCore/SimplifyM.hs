{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module PikaC.Stage.ToPikaCore.SimplifyM
  (SimplifyFuel (..)
  ,SimplifyM
  ,runSimplifyM
  ,runQuiet
  ,Quiet
  ,LogIO
  ,runLogIO
  ,fixedPoint
  ,Logger (..)
  ,step
  )
  where

import Text.PrettyPrint.HughesPJ hiding ((<>), Mode, first)

import PikaC.Ppr

import Unbound.Generics.LocallyNameless

import Control.Monad.State
import Control.Monad.Identity

import Data.Proxy

import Control.Monad

class Monad m => Logger m where
  logM :: String -> m ()

data SimplifyFuel = Unlimited | Fuel Int
  deriving (Show)

newtype Quiet a = Quiet (Identity a)
  deriving (Functor, Applicative, Monad)

newtype LogIO a = LogIO (IO a)
  deriving (Functor, Applicative, Monad)

instance Logger Quiet where
  logM _ = Quiet (Identity ())

instance Logger LogIO where
  logM = LogIO . putStrLn

newtype SimplifyM m a = SimplifyM (FreshMT (StateT SimplifyFuel m) a)
  deriving (Functor, Applicative, Monad, Fresh)

instance Logger m => Logger (SimplifyM m) where
  logM = SimplifyM . lift . lift . logM

runSimplifyM :: (Logger m, Ppr a) => SimplifyFuel -> (a -> SimplifyM m a) -> a -> m a
runSimplifyM fuel fn initial =
  runSimplifyM' fuel $ do
    logM $ "With fuel " ++ show fuel ++ ", running simplifier on:\n" ++ ppr' initial
    fn initial

runQuiet :: (Ppr a) => SimplifyFuel -> (a -> SimplifyM Quiet a) -> a -> a
runQuiet fuel fn initial =
  let Quiet (Identity r) = runSimplifyM fuel fn initial
  in r

runLogIO :: (Ppr a) => SimplifyFuel -> (a -> SimplifyM LogIO a) -> a -> IO a
runLogIO fuel fn initial =
  let LogIO m = runSimplifyM fuel fn initial
  in m

runSimplifyM' :: Monad m => SimplifyFuel -> SimplifyM m a -> m a
runSimplifyM' fuel (SimplifyM act) = evalStateT (runFreshMT act) fuel

liftState :: Monad m => StateT SimplifyFuel m a -> SimplifyM m a
liftState m = SimplifyM (lift m)

liftM :: Monad m => m a -> SimplifyM m a
liftM = SimplifyM . lift . lift

data FuelState = Done | TakeStep
  deriving (Show)

tryUseFuel :: Monad m => SimplifyM m FuelState
tryUseFuel =
  liftState get >>= \case
    Fuel n | n <= 0 -> pure Done
    Fuel n -> liftState (put (Fuel (n-1))) *> pure TakeStep
    Unlimited -> pure TakeStep

fixedPoint :: (Logger m, Alpha a) => (a -> SimplifyM m a) -> a -> SimplifyM m a
fixedPoint f x =
  tryUseFuel >>= \case
    TakeStep -> do
      y <- f x
      if aeq y x
        then pure y
        else fixedPoint f y
    Done -> pure x

step :: (Logger m, Alpha a, Ppr a) => String -> (a -> SimplifyM m a) -> a -> SimplifyM m a
step stageName f x = do
  tryUseFuel >>= \case
    Done -> pure x
    TakeStep -> go
  where
    go = do
      r <- f x
      when (not (aeq r x)) $ logM $ "\n    <<" ++ stageName ++ ">>:\n" ++ ppr' r
      pure r

