import Common

myTake :: Nat -> [Int] -> [Int]
myTake Z _ = []
myTake (S n) (x:xs) = x : myTake n xs
mkTake (S _) xs = xs

main :: IO ()
main = print $ myTake (fromInt 5000) theList

