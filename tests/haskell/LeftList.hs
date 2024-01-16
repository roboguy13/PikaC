
import Common

leftList :: Tree -> [Int]
leftList Leaf = []
leftList (Node x l r) = x : leftList l

main :: IO ()
main = testLoop $ print $ leftList tree
