module Common where

import Control.Monad
import Data.Int
import Control.DeepSeq
import GHC.Generics

#include "Defaults.hs"

testIterations :: Int
testIterations = TEST_ITERATIONS

-- testLoop :: IO () -> IO ()
-- testLoop = replicateM_ testIterations

data Tree = Leaf | Node Int Tree Tree
  deriving (Show, Generic)

data List = Nil | Cons Int List
  deriving (Generic)

instance Show List where
  show Nil = "[]"
  show (Cons x0 xs0) = '[' : go x0 xs0
    where
      go x Nil = show x ++ "]"
      go x (Cons y ys) = show x ++ "," ++ go y ys

theList :: List
theList = force $ go 0
  where
    go n
      | n < LIST_MAX = Cons n (go (n+1))
      | otherwise    = Nil

-- theList = force [0 .. LIST_MAX-1]

mkCompleteTree :: Int -> Tree
mkCompleteTree 0 = Leaf
mkCompleteTree depth =
  let subtree = mkCompleteTree (depth - 1)
  in
  Node depth subtree subtree

tree :: Tree
tree = force $ mkCompleteTree BINARY_TREE_SIZE


data Nat = Z | S Nat
  deriving (Show, Generic)

fromInt :: Int -> Nat
fromInt 0 = Z
fromInt n = S (fromInt (n - 1))

instance NFData Tree
instance NFData List
instance NFData Nat

theNat :: Nat
theNat = force $ fromInt theInt

theInt :: Int
theInt = DEFAULT_NAT

-- -- NOTE: Taken from 'criterion' module 'Criterion.Measurement.Types':
-- --
-- -- | Generate a function which applies an argument to a function a given
-- -- number of times, running its action and reducing the result to normal form.
-- nfAppIO' :: (b -> ()) -> (a -> IO b) -> a -> (Int64 -> IO ())
-- nfAppIO' reduce f v = go
--   where go n
--           | n <= 0    = return ()
--           | otherwise = do
--               x <- f v
--               reduce x `seq` go (n-1)
-- {-# NOINLINE nfAppIO' #-}
--
-- nfAppIO :: NFData b => (a -> IO b) -> a -> IO ()
-- nfAppIO f x = nfAppIO' rnf f x 1

