
data Tree = Leaf | Node Int Tree Tree
  deriving (Show)

leftList :: Tree -> [Int]
leftList Leaf = []
leftList (Node x l r) = x : leftList l

mkCompleteTree :: Int -> Tree
mkCompleteTree 0 = Leaf
mkCompleteTree depth =
  let subtree = mkCompleteTree (depth - 1)
  in
  Node depth subtree subtree

tree :: Tree
tree = mkCompleteTree 10

main :: IO ()
main = print $ leftList tree
