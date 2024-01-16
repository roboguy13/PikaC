import Common

mySum :: [Int] -> Int
mySum [] = 0
mySum (x:xs) = x + mySum xs

go :: Int -> IO ()
go 0 = pure ()
go n = do
  print $ mySum theList
  go (n - 1)

main :: IO ()
main = go testIterations

