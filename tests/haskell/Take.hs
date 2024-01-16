
data Nat = Z | S Nat
  deriving (Show)

myTake :: Nat -> [Int] -> [Int]
myTake Z _ = []
myTake (S n) (x:xs) = x : myTake n xs

theList :: [Int]
theList = [1..10000]


main :: IO ()
main = print $ myTake 7 theList

