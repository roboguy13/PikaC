{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module PikaC.Stage.ToPikaCore.SimplifyM
  (SimplifyFuel (..)
  ,SimplifyM
  ,runSimplifyFn
  ,runSimplifyFn'
  -- ,runSimplifyM''

  ,Quiet
  ,LogIO
  ,ErrorLog
  ,runQuiet
  ,runLogIO
  ,runErrorLog
  ,runSimplifyQuiet
  ,runSimplifyLogIO


  ,fixedPoint
  ,Logger (..)
  ,step
  )
  where

import Text.PrettyPrint.HughesPJ hiding ((<>), Mode, first)

import PikaC.Ppr

import Unbound.Generics.LocallyNameless

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Monad.Morph

import Data.Proxy

import Control.Monad

import GHC.Stack

class Monad m => Logger m where
  logM :: String -> m ()
  logError :: HasCallStack => String -> m a

data SimplifyFuel = Unlimited | Fuel Int
  deriving (Show)

newtype Quiet a = Quiet (Identity a)
  deriving (Functor, Applicative, Monad)

newtype LogIO a = LogIO { runLogIO :: IO a }
  deriving (Functor, Applicative, Monad)

newtype ErrorLog a = ErrorLog (State String a)
  deriving (Functor, Applicative, Monad)

instance Logger ErrorLog where
  logM str = ErrorLog $ modify (<> ("\n" <> str))
  logError str = do
    msg <- ErrorLog get
    error (str <> "\n" <> msg)

runQuiet :: Quiet a -> a
runQuiet (Quiet (Identity x)) = x

runErrorLog :: ErrorLog a -> a
runErrorLog (ErrorLog m) = evalState m mempty

instance Logger Quiet where
  logM _ = Quiet (Identity ())
  logError = error

instance Logger LogIO where
  logM = LogIO . putStrLn
  logError = error

newtype SimplifyM m a = SimplifyM (FreshMT (StateT SimplifyFuel m) a)
  deriving (Functor, Applicative, Monad, Fresh)

instance Logger m => Logger (SimplifyM m) where
  logM = SimplifyM . lift . lift . logM
  logError = SimplifyM . lift . lift . logError

runSimplifyFn :: (Logger m, Ppr a) => SimplifyFuel -> (a -> SimplifyM m a) -> a -> m a
runSimplifyFn fuel fn = runFreshMT . runSimplifyFn' fuel fn

runSimplifyFn' :: (Logger m, Ppr a) => SimplifyFuel -> (a -> SimplifyM m a) -> a -> FreshMT m a
runSimplifyFn' fuel fn initial =
  runSimplifyM'' fuel $ do
    logM $ "With fuel " ++ show fuel ++ ", running simplifier on:\n" ++ ppr' initial
    r <- fn initial
    logM "Simplifer done.\n"
    pure r

runSimplifyQuiet :: (Ppr a) => SimplifyFuel -> (a -> SimplifyM Quiet a) -> a -> a
runSimplifyQuiet fuel fn initial =
  let Quiet (Identity r) = runSimplifyFn fuel fn initial
  in r

runSimplifyLogIO :: (Ppr a) => SimplifyFuel -> (a -> SimplifyM LogIO a) -> a -> IO a
runSimplifyLogIO fuel fn initial =
  let LogIO m = runSimplifyFn fuel fn initial
  in m

runSimplifyM' :: Monad m => SimplifyFuel -> SimplifyM m a -> m a
runSimplifyM' fuel (SimplifyM act) = evalStateT (runFreshMT act) fuel

runSimplifyM'' :: Monad m => SimplifyFuel -> SimplifyM m a -> FreshMT m a
runSimplifyM'' fuel (SimplifyM act) = FreshMT $ do
  ix <- get
  let z = contFreshMT act ix
  lift $ evalStateT z fuel

-- liftFreshMT :: Monad m => FreshMT m a -> SimplifyM m a
-- liftFreshMT act = SimplifyM (lift (_ act))

-- runSimplifyM'' :: Monad m => SimplifyFuel -> SimplifyM m a -> FreshMT m a
-- runSimplifyM'' fuel (SimplifyM act) =
--   evalStateT _ fuel

-- runSimplifyM :: SimplifyFuel -> (a -> SimplifyM m a) -> a -> Fresh (m a)
-- runSimplifyM fuel f initial = undefined


-- runSimplifyFreshM :: SimplifyFuel -> (a -> SimplifyM m a) -> a -> FreshMT m a
-- runSimplifyFreshM fuel fn initial = undefined

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

