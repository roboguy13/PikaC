{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module PikaC.Syntax.Type
  where

import PikaC.Ppr
import Text.Show.Deriving

-- import Bound
import Unbound.Generics.LocallyNameless

import Control.Monad.Trans
import GHC.Generics

data Type
  = IntType
  | BoolType
  | FnType Type Type
  | TyVar TypeName
  | TyLayoutName String
  deriving (Show, Generic)

instance Alpha Type

splitFnType :: Type -> ([Type], Type)
splitFnType (FnType src tgt) =
  let (args, r) = splitFnType tgt
  in
  (src:args, r)
splitFnType x = ([], x)

-- | Example:
--     (a :~ layout(Adt2), b :~ layout(Adt2)) => a -> b
data TypeSig =
  TypeSig
  { typeSigLayoutConstraints :: [LayoutConstraint]
  , typeSigTy :: Type
  }
  deriving (Show)

newtype AdtName = AdtName String
  deriving (Show, Eq, Ord, Generic)

newtype TypeVar = TypeVar String
type TypeName = Name TypeVar

instance Ppr TypeVar where ppr (TypeVar v) = text v

instance Alpha AdtName
instance Subst a AdtName where isvar _ = Nothing -- We never replace a concrete layout name with anything else

data LayoutConstraint = TypeName :~ AdtName
  deriving (Show)

-- instance Bound LayoutTypeArg where

instance Ppr Type where
  ppr IntType = text "Int"
  ppr BoolType = text "Bool"
  ppr (FnType src tgt) = hsep [pprP src, text " -> ", ppr tgt]
  ppr (TyVar x) = ppr x

instance Ppr AdtName where ppr (AdtName n) = text n

instance Ppr LayoutConstraint where
  ppr (x :~ adt) = hsep [ppr x, text ":~ layout(" <> ppr adt <> text ")"]

instance IsNested Type where
  isNested (FnType {}) = True
  isNested IntType = False
  isNested BoolType = False
  isNested (TyVar x) = False

