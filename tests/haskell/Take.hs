import Common

myTake :: Nat -> [Int] -> [Int]
myTake Z _ = []
myTake (S n) (x:xs) = x : myTake n xs

theList :: [Int]
theList = [1..100000]

main :: IO ()
main = print $ myTake (fromInt 5000) theList

