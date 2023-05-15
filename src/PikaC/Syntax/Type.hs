{-# LANGUAGE DeriveFunctor #-}

module PikaC.Syntax.Type
  where

import           PikaC.Ppr

data Type a
  = IntType
  | BoolType
  | FnType (Type a) (Type a)
  | TyVar a
  deriving (Show, Functor)

-- | Example:
--     (a :~ layout(Adt2), b :~ layout(Adt2)) => a -> b
data TypeSig a =
  TypeSig
  { typeSigLayoutConstraints :: [LayoutConstraint a]
  , typeSigTy :: Type a
  }
  deriving (Show, Functor)

newtype AdtName = AdtName String
  deriving (Show, Eq, Ord)

data LayoutConstraint a = a :~ AdtName
  deriving (Show, Functor)

data LayoutArg a = ConcreteLayout String | LayoutVar a
  deriving (Show, Functor)

instance Ppr a => Ppr (Type a) where
  ppr IntType = text "Int"
  ppr BoolType = text "Bool"
  ppr (FnType src tgt) = hsep [pprP src, text " -> ", ppr tgt]
  ppr (TyVar x) = ppr x

instance Ppr AdtName where ppr (AdtName n) = text n

instance Ppr a => Ppr (LayoutConstraint a) where
  ppr (x :~ adt) = hsep [ppr x, text ":~ layout(" <> ppr adt <> text ")"]

instance IsNested (Type a) where
  isNested (FnType {}) = True
  isNested IntType = False
  isNested BoolType = False
  isNested (TyVar x) = False
