{-# LANGUAGE FlexibleInstances #-}

module PikaC.Ppr
  where

data Brackets = NoBrackets | Brackets

class Ppr a where
  pprS :: a -> Brackets -> String

ppr :: Ppr a => a -> String
ppr = flip pprS NoBrackets

pprGrouped :: Ppr a => a -> String
pprGrouped x = pprS x Brackets

instance Ppr String where pprS = const
instance Ppr Int where pprS = (show .) . const

grouped :: String -> Brackets -> String
grouped = groupedWith ("(", ")")

groupedWith :: (String, String) -> String -> Brackets -> String
groupedWith (start, end) x NoBrackets = x
groupedWith (start, end) x Brackets = start <> x <> end

standalone :: String -> Brackets -> String
standalone = const


