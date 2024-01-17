import Common

myTake :: Nat -> List -> List
myTake Z _ = Nil
myTake (S n) (Cons x xs) = Cons x (myTake n xs)
myTake (S _) xs = xs

go :: Int -> IO ()
go 0 = pure ()
go n = do
  print $ myTake theNat theList
  go (n - 1)

main :: IO ()
main = go testIterations

