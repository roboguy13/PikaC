import Common

mySum :: List -> Int
mySum Nil = 0
mySum (Cons x xs) = x + mySum xs

go :: Int -> IO ()
go 0 = pure ()
go n = do
  print $ mySum theList
  go (n - 1)

main :: IO ()
main = go testIterations

