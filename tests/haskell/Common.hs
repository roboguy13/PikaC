module Common where

import Control.Monad

#include "Defaults.hs"

testIterations :: Int
testIterations = TEST_ITERATIONS

testLoop :: IO () -> IO ()
testLoop = replicateM_ testIterations

data Tree = Leaf | Node Int Tree Tree
  deriving (Show)

theList :: [Int]
theList = [0 .. LIST_MAX-1]

mkCompleteTree :: Int -> Tree
mkCompleteTree 0 = Leaf
mkCompleteTree depth =
  let subtree = mkCompleteTree (depth - 1)
  in
  Node depth subtree subtree

tree :: Tree
tree = mkCompleteTree BINARY_TREE_SIZE

data Nat = Z | S Nat
  deriving (Show)

fromInt :: Int -> Nat
fromInt 0 = Z
fromInt n = S (fromInt (n - 1))

theNat :: Nat
theNat = fromInt DEFAULT_NAT
