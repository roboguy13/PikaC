import Common

myTake :: Nat -> [Int] -> [Int]
myTake Z _ = []
myTake (S n) (x:xs) = x : myTake n xs
myTake (S _) xs = xs

go :: Int -> IO ()
go 0 = pure ()
go n = do
  print $ myTake (fromInt 5000) theList
  go (n - 1)

main :: IO ()
main = go testIterations

