
import Common

leftList :: Tree -> [Int]
leftList Leaf = []
leftList (Node x l r) = x : leftList l

go :: Int -> IO ()
go 0 = pure ()
go n = do
  print $ leftList tree
  go (n - 1)

main :: IO ()
main = go testIterations
