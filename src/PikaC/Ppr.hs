{-# LANGUAGE FlexibleInstances #-}

module PikaC.Ppr
  (module Text.PrettyPrint.HughesPJ
  ,Ppr
  ,ppr
  ,ppr'
  ,pprP
  ,IsNested
  ,isNested
  )
  where

import Text.PrettyPrint.HughesPJ hiding ((<>))

class Ppr a where
  ppr :: a -> Doc

ppr' :: Ppr a => a -> String
ppr' = render . ppr

pprP :: (IsNested a, Ppr a) => a -> Doc
pprP x = maybeParens (isNested x) (ppr x)

instance Ppr String where ppr = text
instance Ppr Int where ppr = text . show
instance Ppr Bool where ppr = text . show

class IsNested a where
  isNested :: a -> Bool

--   pprS :: a -> Brackets -> String
-- data Brackets = NoBrackets | Brackets
--
-- ppr :: Ppr a => a -> String
-- ppr = flip pprS NoBrackets
--
-- pprGrouped :: Ppr a => a -> String
-- pprGrouped x = pprS x Brackets
--
--
-- grouped :: String -> Brackets -> String
-- grouped = groupedWith ("(", ")")
--
-- groupedWith :: (String, String) -> String -> Brackets -> String
-- groupedWith (start, end) x NoBrackets = x
-- groupedWith (start, end) x Brackets = start <> x <> end
--
-- standalone :: String -> Brackets -> String
-- standalone = const
--
--
