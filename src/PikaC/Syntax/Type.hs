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
import Unbound.Generics.LocallyNameless.Bind
import Unbound.Generics.LocallyNameless.Unsafe

import Control.Monad.Trans
import GHC.Generics
import Data.Data

import Control.Lens.TH

import Test.QuickCheck
import Control.Monad
import Control.Applicative

import Control.DeepSeq

import Data.Validity

import GHC.Stack

data Type
  = IntType
  | BoolType
  | FnType Type Type
  | TyVar TypeName
  | LayoutId String
  | ForAll (Bind (TypeName, Embed AdtName) Type)
  | GhostApp Type [String]
  | PlaceholderVar TypeName -- These should be replaced by actual TyVars or LayoutIds
  deriving (Show, Generic)

instance Subst Type Type where
  isvar (TyVar v) = Just (SubstName v)
  isvar _ = Nothing

instance Subst (f a) (f a) => Subst (f a) Type

getLayoutId :: HasCallStack => Type -> String
getLayoutId (LayoutId t) = t
getLayoutId (GhostApp (LayoutId t) _) = t

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
  { _typeSigConstrainedType :: Bind [TypeName] ConstrainedType
  }
  deriving (Show, Generic)

data ConstrainedType =
  ConstrainedType
  { _ctypeLayoutConstraints :: [LayoutConstraint]
  , _ctypeTy :: Type
  }
  deriving (Show, Generic)

toTypeSig :: Type -> TypeSig
toTypeSig = TypeSig . bind [] . ConstrainedType []

fromTypeSig_unsafe :: HasCallStack => TypeSig -> Type
fromTypeSig_unsafe (TypeSig (B [] (ConstrainedType [] ty))) = ty

fromTypeSig :: TypeSig -> Type
fromTypeSig (TypeSig bnd) =
  let (vs, ctType) = unsafeUnbind bnd
  in
  mkForAlls [ctType]

mkForAlls :: [ConstrainedType] -> Type
mkForAlls [ConstrainedType cts ty] = mkForAllCts cts ty
mkForAlls (ConstrainedType cts ty : rest) =
  mkForAllCts cts (mkForAlls rest)

mkForAllCts :: [LayoutConstraint] -> Type -> Type
mkForAllCts = flip $ foldr go
  where
    go (t :~ adt) = ForAll . bind (t, Embed adt)

findConstraint :: TypeName -> [LayoutConstraint] -> Maybe AdtName
findConstraint _ [] = Nothing
findConstraint v ((v' :~ adt) : cts)
  | v' == v = Just adt
  | otherwise = findConstraint v cts

findAdtConstraint :: AdtName -> [LayoutConstraint] -> Maybe TypeName
findAdtConstraint _ [] = Nothing
findAdtConstraint adt ((v :~ adt') : cts)
  | adt' == adt = Just v
  | otherwise = findAdtConstraint adt cts

instance Alpha LayoutConstraint
instance Alpha ConstrainedType

instance Arbitrary Type where
  arbitrary = error "Arbitrary Type"
  shrink = genericShrink
    -- oneof
    --   [ IntType
    --   , BoolType
    --   , FnType
    --   ]

  -- TODO: Implement
instance Validity TypeSig where validate _ = mempty
instance Validity Type where validate _ = mempty

instance Arbitrary ConstrainedType where
  arbitrary = error "Arbitrary ConstrainedType"
  shrink = genericShrink

instance Arbitrary TypeSig where
  arbitrary = error "Arbitrary TypeSig"
  shrink = genericShrink

instance Arbitrary LayoutConstraint where
  arbitrary = error "Arbitrary LayoutConstraint"
  shrink = genericShrink

instance Ppr TypeSig where
  ppr (TypeSig bnd) = runFreshM $ do
    (_, ctype) <- unbind bnd
    pure $ ppr ctype

instance Ppr ConstrainedType where
  ppr (ConstrainedType [] ty) = ppr ty
  ppr (ConstrainedType xs ty) =
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
  ppr (LayoutId x) = text x
  ppr (PlaceholderVar x) = text "?" <> ppr x
  ppr (GhostApp ty xs) = pprP ty <+> hsep (map (\x -> text "@" <> text x) xs)
  ppr (ForAll bnd) =
    let ((v, Embed adt), body) = unsafeUnbind bnd
    in
    ((text "forall" <+> (parens (ppr v <+> text ":~" <+> ppr adt))) <> text ".") <+> ppr body

-- instance Subst Type AdtName
instance Subst a a => Subst a AdtName

instance Ppr AdtName where ppr (AdtName n) = text n

instance Ppr LayoutConstraint where
  ppr (x :~ adt) = hsep [ppr x, text ":~ layout(" <> ppr adt <> text ")"]

instance IsNested Type where
  isNested (FnType {}) = True
  isNested IntType = False
  isNested BoolType = False
  isNested (TyVar x) = False
  isNested (LayoutId x) = False
  isNested (PlaceholderVar x) = False
  isNested (GhostApp {}) = False
  isNested (ForAll {}) = True

makeLenses ''TypeSig

instance NFData AdtName

instance NFData Type
instance NFData TypeSig
instance NFData LayoutConstraint
instance NFData ConstrainedType


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

