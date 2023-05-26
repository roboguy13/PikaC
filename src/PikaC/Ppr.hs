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

import Text.PrettyPrint.HughesPJ hiding ((<>), Mode)

import Unbound.Generics.LocallyNameless

class Ppr a where
  ppr :: a -> Doc

ppr' :: Ppr a => a -> String
ppr' = render . ppr

pprP :: (IsNested a, Ppr a) => a -> Doc
pprP x = maybeParens (isNested x) (ppr x)

instance Ppr String where ppr = text
instance Ppr Int where ppr = text . show
instance Ppr Bool where ppr = text . show

instance Ppr (Name a) where ppr = text . show

instance Ppr AnyName where ppr (AnyName n) = ppr n

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
