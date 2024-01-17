import Common

mapAdd :: Int -> List -> List
mapAdd _ Nil = Nil
mapAdd n (Cons x xs) = Cons (x + n) (mapAdd n xs)

go :: Int -> IO ()
go 0 = pure ()
go n = do
  print $ mapAdd theInt theList
  go (n - 1)

main :: IO ()
main = go testIterations

