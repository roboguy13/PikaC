import Common
import Control.Monad

filterLt :: Int -> [Int] -> [Int]
filterLt n [] = []
filterLt n (x:xs)
  | x < n     = filterLt n xs
  | otherwise = x : filterLt n xs

main :: IO ()
main = testLoop $ print $ filterLt 7 theList

