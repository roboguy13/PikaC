{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module PikaC.Syntax.Type
  where

import PikaC.Ppr
import PikaC.Utils
import Text.Show.Deriving

-- import Bound
import Unbound.Generics.LocallyNameless

import Control.Monad.Trans
import GHC.Generics
import Data.Data

import Control.Lens.TH

import Test.QuickCheck
import Control.Monad

import Control.DeepSeq

import GHC.Stack

data Type
  = IntType
  | BoolType
  | FnType Type Type
  | TyVar TypeName
  | GhostApp Type [String]
  deriving (Show, Generic)

getTyVar :: HasCallStack => Type -> TypeName
getTyVar (TyVar t) = t
getTyVar (GhostApp (TyVar t) _) = t

isBaseType :: Type -> Bool
isBaseType IntType = True
isBaseType BoolType = True
isBaseType _ = False

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
  { _typeSigLayoutConstraints :: [LayoutConstraint]
  , _typeSigTy :: Type
  }
  deriving (Show, Generic)

instance Arbitrary Type where
  arbitrary = error "Arbitrary Type"
  shrink = genericShrink
    -- oneof
    --   [ IntType
    --   , BoolType
    --   , FnType
    --   ]

instance Arbitrary TypeSig where
  arbitrary = error "Arbitrary TypeSig"
  shrink = genericShrink

instance Arbitrary LayoutConstraint where
  arbitrary = error "Arbitrary LayoutConstraint"
  shrink = genericShrink

instance Ppr TypeSig where
  ppr (TypeSig [] ty) = ppr ty
  ppr (TypeSig xs ty) =
    hsep
      [text "(" <>
         hsep (punctuate (text ",") (map ppr xs)) <>
         text ")"
      ,text "=>"
      ,ppr ty
      ]

newtype AdtName = AdtName { unAdtName :: String }
  deriving (Show, Eq, Ord, Generic, Data)

instance Arbitrary AdtName where
  arbitrary = error "Arbitrary AdtName"

-- newtype TypeVar = TypeVar { unTypeVar :: TypeName } deriving (Show, Generic)
type TypeName = Name Type

-- instance Alpha TypeVar

-- instance Subst TypeVar TypeVar where
--   isvar (TypeVar v) = Just $ SubstName v

-- instance Subst TypeVar AdtName

-- instance Ppr TypeVar where ppr (TypeVar v) = text $ show v

instance Alpha AdtName
-- instance Subst a AdtName where isvar _ = Nothing -- We never replace a concrete layout name with anything else

data LayoutConstraint = TypeName :~ AdtName
  deriving (Show, Generic)

-- instance Bound LayoutTypeArg where

instance Ppr Type where
  ppr IntType = text "Int"
  ppr BoolType = text "Bool"
  ppr (FnType src tgt) = hsep [pprP src, text "->", ppr tgt]
  ppr (TyVar x) = ppr x
  ppr (GhostApp ty xs) = pprP ty <+> hsep (map (\x -> text "@" <> text x) xs)

instance Ppr AdtName where ppr (AdtName n) = text n

instance Ppr LayoutConstraint where
  ppr (x :~ adt) = hsep [ppr x, text ":~ layout(" <> ppr adt <> text ")"]

instance IsNested Type where
  isNested (FnType {}) = True
  isNested IntType = False
  isNested BoolType = False
  isNested (TyVar x) = False
  isNested (GhostApp {}) = False

makeLenses ''TypeSig

instance NFData AdtName

instance NFData Type
instance NFData TypeSig
instance NFData LayoutConstraint

--
-- Property tests --
--

-- instance WellScoped (Name a) Type
-- instance WellScoped (Name a) (Name Type)

data AdtArg = BaseArg | RecArg
  deriving (Show, Eq, Generic)

-- | Has at least on non-nullary constructor
nonUnitSig :: (AdtName, [(String, [AdtArg])]) -> Bool
nonUnitSig (_, xs) = any (not . null . snd) xs

genAdtSig :: Gen (AdtName, [(String, [AdtArg])])
genAdtSig = do
  n <- choose (1, 3)
  liftA2 (,) genAdtName (replicateM n genConstructor)
    `suchThat` nonUnitSig -- Don't generate unit types

-- | Constructor name and arity
genConstructor :: Gen (String, [AdtArg])
genConstructor = do
  n <- choose (0, 3) :: Gen Int
  args <- replicateM n $ oneof [pure BaseArg, pure RecArg]
  liftA2 (,) genConstructorName (pure args)

genAdtName :: Gen AdtName
genAdtName = AdtName <$> fmap (:[]) (elements ['A'..'Z'])

genConstructorName :: Gen String
genConstructorName = do
  n <- choose (1, 3)
  replicateM n (elements ['A'..'Z'])

