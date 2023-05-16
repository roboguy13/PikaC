{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PikaC.FreshGen
  where

import           Control.Monad.State

newtype FreshGen a = FreshGen (State Int a)
  deriving (Functor, Applicative, Monad, MonadState Int)

runFreshGen :: FreshGen a -> a
runFreshGen (FreshGen m) = evalState m 0

fresh :: String -> FreshGen String
fresh postfix = do
  i <- get
  modify succ
  pure ("_" ++ show i ++ "_" ++ postfix )

