import Common

filterLt :: Int -> [Int] -> [Int]
filterLt n [] = []
filterLt n (x:xs)
  | x < n     = filterLt n xs
  | otherwise = x : filterLt n xs

go :: Int -> IO ()
go 0 = pure ()
go n = do
  print $ filterLt theInt theList
  go (n - 1)

main :: IO ()
main = go testIterations

