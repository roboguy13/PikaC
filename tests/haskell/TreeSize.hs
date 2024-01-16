import Common

treeSize :: Tree -> Int
treeSize Leaf = 1
treeSize (Node _ l r) = 1 + treeSize l + treeSize r

go :: Int -> IO ()
go 0 = pure ()
go n = do
  print $ treeSize tree
  go (n - 1)

main :: IO ()
main = go testIterations

