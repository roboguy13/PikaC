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

instance Ppr a => Ppr (Type a) where
  pprS IntType = standalone "Int"
  pprS BoolType = standalone "Bool"
  pprS (FnType src tgt) = grouped $ pprGrouped src <> " -> " <> ppr tgt
  pprS (TyVar x) = pprS x

instance Ppr AdtName where pprS (AdtName n) = standalone n

instance Ppr a => Ppr (LayoutConstraint a) where
  pprS (x :~ adt) = standalone $ ppr x <> " :~ layout(" <> ppr adt <> ")"

