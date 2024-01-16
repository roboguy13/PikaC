import Common

mapAdd :: Int -> [Int] -> [Int]
mapAdd _ [] = []
mapAdd n (x:xs) = (x + n) : mapAdd n xs

go :: Int -> IO ()
go 0 = pure ()
go n = do
  print $ mapAdd theInt theList
  go (n - 1)

main :: IO ()
main = go testIterations

