{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module PikaC.Syntax.Type
  where

import PikaC.Ppr
import Text.Show.Deriving

import Bound

import Control.Monad.Trans

data Type a
  = IntType
  | BoolType
  | FnType (Type a) (Type a)
  | TyVar a
  deriving (Show, Functor)

splitFnType :: Type a -> ([Type a], Type a)
splitFnType (FnType src tgt) =
  let (args, r) = splitFnType tgt
  in
  (src:args, r)
splitFnType x = ([], x)

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

data LayoutTypeArg f a = ConcreteLayout String | LayoutVar (f a)
  deriving (Show, Functor)

deriveShow1 ''LayoutTypeArg

layoutTypeArgSubst :: Monad f => LayoutTypeArg f a -> (a -> f c) -> LayoutTypeArg f c
layoutTypeArgSubst (ConcreteLayout s) _ = ConcreteLayout s
layoutTypeArgSubst (LayoutVar x) f = LayoutVar $ x >>= f

-- instance Bound LayoutTypeArg where

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
