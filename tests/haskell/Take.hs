
data Nat = Z | S Nat
  deriving (Show)

myTake :: Nat -> [Int] -> [Int]
myTake Z _ = []
myTake (S n) (x:xs) = x : myTake n xs

theList :: [Int]
theList = [1..100000]

fromInt :: Int -> Nat
fromInt 0 = Z
fromInt n = S (fromInt (n - 1))

main :: IO ()
main = print $ myTake (fromInt 5000) theList

