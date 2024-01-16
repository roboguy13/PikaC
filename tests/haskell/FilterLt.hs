theList :: [Int]
theList = [1..10000]

filterLt :: Int -> [Int] -> [Int]
filterLt n [] = []
filterLt n (x:xs)
  | x < n = filterLt n xs
  | otherwise = x : filterLt n xs

main :: IO ()
main = print $ filterLt 7 theList

