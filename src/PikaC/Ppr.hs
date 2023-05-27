{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module PikaC.Ppr
  (module Text.PrettyPrint.HughesPJ
  ,Ppr
  ,ppr
  ,ppr'
  ,pprP
  ,IsNested
  ,isNested
  ,PprShow (..)
  ,PprBind (..)
  )
  where

import PikaC.Utils

import Text.PrettyPrint.HughesPJ hiding ((<>), Mode)

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Bind

import Data.Typeable

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

newtype PprShow a = PprShow a

instance Ppr a => Show (PprShow a) where
  show (PprShow x) = ppr' x

newtype PprBind a b = PprBind (Bind [a] b)

instance (Ppr b, HasNames a b, HasVar b, Alpha a, Alpha b, Subst b b) => Ppr (PprBind a b) where
  ppr (PprBind bnd) = ppr $ openBind bnd

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
