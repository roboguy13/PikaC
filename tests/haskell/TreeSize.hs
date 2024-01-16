import Common

treeSize :: Tree -> Int
treeSize Leaf = 1
treeSize (Node _ l r) = 1 + treeSize l + treeSize r

main :: IO ()
main = print $ treeSize tree

