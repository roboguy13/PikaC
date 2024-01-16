
data Tree = Leaf | Node Int Tree Tree
  deriving (Show)

treeSize :: Tree -> Int
treeSize Leaf = 1
treeSize (Node _ l r) = 1 + treeSize l + treeSize r

mkCompleteTree :: Int -> Tree
mkCompleteTree 0 = Leaf
mkCompleteTree depth =
  let subtree = mkCompleteTree (depth - 1)
  in
  Node depth subtree subtree

tree :: Tree
tree = mkCompleteTree 15

main :: IO ()
main = print $ treeSize tree

