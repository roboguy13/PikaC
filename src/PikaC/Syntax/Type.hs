{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

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
import GHC.Generics hiding (Constructor)
import Data.Data

import Control.Lens.TH

import Test.QuickCheck
import Control.Monad
import Control.Applicative

import Control.DeepSeq

import Control.Lens hiding (elements)

import Data.Validity

import GHC.Stack

import Debug.Trace

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

instance Size Type where
  size IntType = 1
  size BoolType = 1
  size (FnType x y) = visibleNode $ size x + size y
  size (TyVar _) = 1
  size (LayoutId _) = 1
  size (ForAll bnd) = visibleNode $ size bnd
  size (GhostApp t xs) = visibleNode $ size t + length xs
  size (PlaceholderVar _) = 1

instance Size AdtName where
  size _ = 1

type TypeName = Name Type

newtype AdtName = AdtName { unAdtName :: String }
  deriving (Show, Eq, Ord, Generic, Data)

makePrisms ''Type

data Typed a = Typed Type a
  deriving (Show, Generic)

class TypePair f where
  type TypePairType f
  typePairType :: Alpha a => f a -> TypePairType f
  typePairData :: Alpha a => f a -> a
  mkTypePair :: Alpha a => TypePairType f -> a -> f a

instance TypePair Typed where
  type TypePairType Typed = Type
  typePairType (Typed ty _) = ty
  typePairData (Typed _ x) = x
  mkTypePair = Typed

instance Subst Type Type where
  isvar (TyVar v) = Just (SubstName v)
  isvar _ = Nothing

instance Subst (f a) (f a) => Subst (f a) Type

instance Plated Type where
  plate _ IntType = pure IntType
  plate _ BoolType = pure BoolType
  plate _ (TyVar v) = pure $ TyVar v
  plate _ (LayoutId x) = pure $ LayoutId x
  plate f (FnType x y) = FnType <$> f x <*> f y
  plate f (ForAll bnd) =
    let (v, body) = unsafeUnbind bnd
    in
    ForAll <$> bind v <$> f body
  plate f (GhostApp t args) = GhostApp <$> f t <*> pure args
  plate _ (PlaceholderVar v) = pure $ PlaceholderVar v

getLayoutId :: HasCallStack => Type -> String
getLayoutId (LayoutId t) = t
getLayoutId (GhostApp (LayoutId t) _) = t

isBaseType :: Type -> Bool
isBaseType IntType = True
isBaseType BoolType = True
isBaseType _ = False

isFnType :: Type -> Bool
isFnType (FnType {}) = True
isFnType _ = False

instance Alpha Type

splitFnType :: Type -> ([Type], Type)
splitFnType (FnType src tgt) =
  let (args, r) = splitFnType tgt
  in
  (src:args, r)
splitFnType x = ([], x)

-- | Example:
--     (a :~ layout(Adt2), b :~ layout(Adt2)) => a -> b
data TypeSig' a =
  TypeSig
  { _typeSigConstrainedType :: Bind [TypeName] (ConstrainedType, a)
  }
  deriving (Show, Generic)

instance Size a => Size (TypeSig' a) where
  size (TypeSig x) = visibleNode $ size x

instance Alpha a => Alpha (TypeSig' a)

type TypeSig = TypeSig' ()

data ConstrainedType =
  ConstrainedType
  { _ctypeLayoutConstraints :: [LayoutConstraint]
  , _ctypeTy :: Type
  }
  deriving (Show, Generic)

instance Size ConstrainedType where
  size (ConstrainedType x y) = visibleNode $ size x + size y

instance TypePair TypeSig' where
  type TypePairType TypeSig' = TypeSig
  typePairType (TypeSig bnd) =
    let (vs, (ty, _)) = unsafeUnbind bnd
    in
    TypeSig $ bind vs (ty, ())
  typePairData (TypeSig bnd) =
    let (vs, (_, body)) = unsafeUnbind bnd
    in
    body
  mkTypePair (TypeSig bnd) x =
    let (vs, (ty, ())) = unsafeUnbind bnd
    in
    TypeSig $ bind vs (ty, x)
toTypeSig :: Alpha a => Typed a -> TypeSig' a
toTypeSig (Typed ty x) = TypeSig $ bind [] (ConstrainedType [] ty, x)

fromTypeSig_unsafe :: HasCallStack => TypeSig' a -> Typed a
fromTypeSig_unsafe (TypeSig (B [] (ConstrainedType [] ty, x))) = Typed ty x

fromTypeSig :: Alpha a => TypeSig' a -> Typed a
fromTypeSig (TypeSig bnd) =
  let (vs, (ctType, x)) = unsafeUnbind bnd
  in
  Typed (mkForAlls [ctType]) x

mkForAlls :: [ConstrainedType] -> Type
mkForAlls [ConstrainedType cts ty] = mkForAllCts cts ty
mkForAlls (ConstrainedType cts ty : rest) =
  mkForAllCts cts (mkForAlls rest)

mkForAllCts :: [LayoutConstraint] -> Type -> Type
mkForAllCts = flip $ foldr go
  where
    go (t :~ adt) = ForAll . bind (t, Embed adt)

forAllsToCts :: Fresh m => Type -> m [LayoutConstraint]
forAllsToCts (ForAll bnd) = do
  ((v, Embed adt), body) <- unbind bnd
  fmap ((v :~ adt) :) (forAllsToCts body)
forAllsToCts _ = pure []

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
    (_, (ctype, ())) <- unbind bnd
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

data Adt =
  Adt
  { _adtName :: AdtName
  , _adtConstructors :: [Constructor]
  }
  deriving (Show, Generic)

instance Size Adt where
  size (Adt _ xs) = visibleNode $ 1 + sizeList xs

data Constructor =
  Constructor
  { _constructorName :: String
  , _constructorType :: Type
  }
  deriving (Show, Generic)

instance Size Constructor where
  size (Constructor _ t) = visibleNode $ 1 + size t

-- | Build the appropriate layout polymorphic type, assuming
-- that one layout is used for each type.
-- Assumes that the constructor type has no quantifiers.
mkConstructor :: String -> Type -> Constructor
mkConstructor cName ty =
  let ty' = mkForAllCts layoutCts $ mkFnType $ map go (argTys ++ [resultTy])
  in
  Constructor cName ty'
  where
    -- | Quantifier variables for each ADT in the constructor's type
    adtVars :: [(AdtName, TypeName)]
    adtVars = zip adts $ map (string2Name . ('a':) . show) [0..]

    layoutCts :: [LayoutConstraint]
    layoutCts = map (\(adt, ty) -> ty :~ adt) adtVars

    (argTys, resultTy) = splitFnType ty

    adts :: [AdtName]
    adts = fastNub . map AdtName $ toListOf (traversed._LayoutId) $ universe $ ty

    go :: Type -> Type
    go (LayoutId adt) =
      let Just tyVar = lookup (AdtName adt) adtVars
      in
      TyVar tyVar
    go ty = ty

mkFnType :: HasCallStack => [Type] -> Type
mkFnType [] = error "mkFunType []"
mkFnType [x] = x
mkFnType (x:xs) = FnType x (mkFnType xs)

instance Arbitrary AdtName where
  arbitrary = error "Arbitrary AdtName"

-- newtype TypeVar = TypeVar { unTypeVar :: TypeName } deriving (Show, Generic)

-- instance Alpha TypeVar

-- instance Subst TypeVar TypeVar where
--   isvar (TypeVar v) = Just $ SubstName v

-- instance Subst TypeVar AdtName

-- instance Ppr TypeVar where ppr (TypeVar v) = text $ show v

instance Alpha AdtName
-- instance Subst a AdtName where isvar _ = Nothing -- We never replace a concrete layout name with anything else

data LayoutConstraint = TypeName :~ AdtName
  deriving (Show, Generic)

instance Size LayoutConstraint where
  size (x :~ y) = visibleNode $ size x + size y

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

makeLenses ''TypeSig'

instance NFData AdtName

instance NFData Type
instance NFData f => NFData (TypeSig' f)
instance NFData LayoutConstraint
instance NFData ConstrainedType

instance NFData Adt
instance NFData Constructor


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

