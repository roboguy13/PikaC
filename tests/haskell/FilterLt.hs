import Common

filterLt :: Int -> List -> List
filterLt n Nil = Nil
filterLt n (Cons x xs)
  | x < n     = filterLt n xs
  | otherwise = Cons x (filterLt n xs)

go :: Int -> IO ()
go 0 = pure ()
go n = do
  print $ filterLt theInt theList
  go (n - 1)

main :: IO ()
main = go testIterations

