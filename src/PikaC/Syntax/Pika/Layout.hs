{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module PikaC.Syntax.Pika.Layout
  where

import PikaC.Syntax.Heaplet
import PikaC.Syntax.Pika.Pattern
import PikaC.Syntax.Type

import PikaC.Ppr
import PikaC.Utils

import Control.Monad.Identity

import GHC.Stack

-- import Bound
-- import Bound.Var

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Bind

import Data.Void
import Data.Typeable

import Control.Lens
import Control.Lens.TH

import GHC.Generics

-- newtype Operand a b = Operand (Var a b)
--   deriving (Functor, Applicative, Monad, Show)

-- pattern Actual :: a -> Operand a b
-- pattern Actual x = Operand (B x)
-- pattern OperandV y = Operand (F y)

-- instance (Show a, Show b) => Ppr (Operand a b) where ppr = text . show

-- newtype Operand a = Operand (Var (Expr a) a)
--
-- evalOperand :: Operand a -> Expr a
-- evalOperand (Operand (B x)) = x
-- evalOperand (Operand (F y)) = V y

data Layout a =
  Layout
    { _layoutName :: String
    , _layoutAdt :: AdtName
    -- , _layoutSig :: LayoutSig a
    , _layoutBranches :: Bind [ModedName a] [LayoutBranch a]
    }
    deriving (Show, Generic)

-- data LayoutSig a =
--   LayoutSig
--     , _layoutSigParams :: [ModedName a]
--     }
--     deriving (Show, Generic)

data Moded a = Moded Mode a
  deriving (Show, Generic)

type ModedName a = Moded (Name a)

data Mode = In | Out
  deriving (Show, Generic, Eq)

type ModeEnv a = [ModedName a]

lookupMode :: ModeEnv a -> Name a -> Maybe Mode
lookupMode env n =
  lookup n (map (\(Moded x y) -> (y, x)) env)

lookupMode' :: HasCallStack => ModeEnv a -> Name a -> Mode
lookupMode' env n =
  case lookupMode env n of
    Nothing -> error $ "lookupMode': Cannot find mode for " ++ show n
    Just r -> r

instance Ppr a => Ppr (ModedName a) where
  ppr (Moded mode n) = ppr mode <> ppr n

instance Ppr Mode where
  ppr In = text "+"
  ppr Out = text "-"

instance (Show a, Alpha a, Typeable a) => Alpha (Moded a)

instance Alpha Mode

instance Subst (Moded a) a => Subst (Moded a) (LayoutBranch a)
instance Subst (Moded a) a => Subst (Moded a) (Pattern a)
instance Subst (Moded a) a => Subst (Moded a) (LayoutBody a)
instance Subst (Moded a) a => Subst (Moded a) (LayoutHeaplet a)
instance Subst (Moded a) a => Subst (Moded a) (PointsTo a)
instance Subst (Moded a) a => Subst (Moded a) (Loc a)

-- type Layout = Layout' Identity

data LayoutBranch a =
  LayoutBranch
    { _layoutPattern :: Pattern a
    , _layoutBody :: LayoutBody a
    }
    deriving (Show, Generic)

-- instance Bound f => Functor (LayoutBranch f)
--
-- instance Bound f => Monad (LayoutBranch f)

-- mkLayoutBranch :: Pattern a -> LayoutBody Identity a -> LayoutBranch LayoutBody a
-- mkLayoutBranch pat body =
--   LayoutBranch
--     { layoutPattern = pat
--     , layoutBody = abstract Just (hoist generalize body)
--     }

-- type LayoutBranch = LayoutBranch' Identity
--
-- newtype LayoutBody operand a = LayoutBody [LayoutHeaplet operand a]
newtype LayoutBody a = LayoutBody { _unLayoutBody :: [LayoutHeaplet a] }
  deriving (Show, Generic, Semigroup, Monoid)

data LayoutHeaplet a
  = LPointsTo (PointsTo a)
  | LApply
      String -- Layout name
      a    -- Pattern variable
      [Name a]    -- Layout variables
      -- [LocVar]    -- Layout variables
  deriving (Show, Generic)

-- type PatternVar = Name Expr

makeLenses ''Layout
makeLenses ''LayoutBranch
makeLenses ''LayoutBody
makePrisms ''LayoutHeaplet

instance (IsNested a, Ppr a) => Ppr (LayoutHeaplet a) where
  ppr (LPointsTo p) = ppr p
  ppr (LApply f patVar layoutVars) =
    text f
      <+> pprP patVar
      <+> (text "["
          <> hsep (punctuate (text ",") (map ppr layoutVars))
          <> text "]")

instance (Alpha a, Typeable a) => Alpha (Layout a)
-- instance (a ~ PType a, Alpha a, Typeable a) => Alpha (LayoutSig a)

-- updateLayoutParams :: Fresh m =>
--   (Name a -> m (Name b)) -> [LayoutBranches
-- updateLayoutParams = undefined

-- instance Subst LayoutVar (f a) => Subst LayoutVar (LayoutHeaplet f a)

-- instance Bound LayoutHeaplet where
--   LPointsTo (x :-> y) >>>= f = LPointsTo $ fmap (>>= f) x :-> fmap (>>= f) y
--   LApply name patVar layoutVars >>>= f =
--     LApply name (patVar >>= f) (map (>>= f) layoutVars)

type LayoutEnv a = [Layout a]

lookupLayout :: LayoutEnv a -> String -> Layout a
lookupLayout [] name = error $ "lookupLayout: Cannot find layout " ++ name
lookupLayout (x:xs) name
  | _layoutName x == name = x
  | otherwise = lookupLayout xs name

-- class ModedC f where
--   getMode :: ModeEnv a -> f (Name a) -> Maybe Mode
--
-- instance ModedC Name where
--   getMode = lookupMode
--
-- -- TODO: Make sure this is consistent with environment?
-- instance ModedC Moded where
--   getMode _ (Moded m _) = Just m

instance IsName (Moded (Name a)) a where
  getName = modedNameName

instance HasNames (Moded (Name a)) a where
  getNames x = [getName x]

getMode :: Moded a -> Mode
getMode (Moded m _) = m

modedNameName :: ModedName a -> Name a
modedNameName (Moded _ n) = n

lookupLayoutBranch :: forall a. HasApp a => Layout a -> String -> Maybe (Bind [ModedName a] (LayoutBranch a))
lookupLayoutBranch layout constructor = go $ _layoutBranches layout
  where
    go :: Bind [ModedName a] [LayoutBranch a] -> Maybe (Bind [ModedName a] (LayoutBranch a))
    go (B _ []) = Nothing
    go (B vs (x:xs)) =
      case _layoutPattern x of
        PatternVar _ -> Just (B vs x)
        Pattern c _
          | c == constructor -> Just (B vs x)
          | otherwise -> go (B vs xs)

lookupLayoutBranch' :: (HasCallStack, HasApp a) => Layout a -> String -> Bind [ModedName a] (LayoutBranch a)
lookupLayoutBranch' layout c =
  case lookupLayoutBranch layout c of
    Nothing -> error $ "lookupLayoutBranch: Cannot find branch for constructor " ++ c
    Just r -> r

-- instance Subst a a => Subst a (LayoutBody a)
-- instance Subst a a => Subst a (LayoutHeaplet a)
-- instance Subst Expr a => Subst Expr (PointsTo a)

-- | Apply layout to a constructor value, given layout parameter names
applyLayout :: (Subst (Moded a) a, Subst a (ModedName a), Alpha a, Typeable a, HasApp a, HasApp a, Subst a (LayoutBody a)) => Layout a -> String -> [a] -> Maybe (Bind [ModedName a] (LayoutBody a))
applyLayout layout constructor args = do
  B params branch <- lookupLayoutBranch layout constructor

  let body = _layoutBody branch

  pure $ patternMatch' (_layoutPattern branch) constructor args (B params body)

applyLayout' :: (Subst (Moded a) a, Subst a (ModedName a), Alpha a, Typeable a, Show a, HasApp a, HasApp a, Subst a (LayoutBody a)) => Layout a -> String -> [a] -> Bind [ModedName a] (LayoutBody a)
applyLayout' layout c args =
  case applyLayout layout c args of
    Nothing -> error $ "applyLayout': Cannot find branch for constructor " ++ c ++ " in " ++ show layout
    Just r -> r

instance (Typeable a, Show a, Alpha a) => Alpha (LayoutHeaplet a)

instance (Show a, Typeable a, Alpha a) => Alpha (LayoutBody a)
instance (Show a, Typeable a, Alpha a) => Alpha (LayoutBranch a)

-- | A name mapping gotten by applying a layout. This can be used
-- for recursively applying layouts
type LayoutMapping a = [(Name a, Name a)]

-- | Follow the LApply's one time to get a LayoutMapping for
-- those layout applications
mappingFromLApplies :: [Layout a] -> LayoutBody a -> LayoutMapping a
mappingFromLApplies = undefined

-- | Includes parameters, excludes pattern variables
layoutBranchFVs :: (Show a, Typeable a, Alpha a) => LayoutBranch a -> [Name a]
layoutBranchFVs branch =
  filter (`notElem` getPatternNames (_layoutPattern branch))
    $ toListOf fv branch

--
-- -- applyLayout' :: (Ppr (Expr a), Show a, Ppr a, Eq a) => Layout' (Operand (LayoutBody a)) a -> String -> [Operand (LayoutBody a) a] -> Operand (LayoutBody a) (Expr a)
-- -- applyLayout' = applyLayout
--
-- -- | Reduce layout lambda applications
-- reduceLayoutApps :: forall a. Expr a -> Expr a
-- reduceLayoutApps = error "TODO: Implement reduceLayoutApps" --go
--   where
--     go :: Expr a -> Expr a
--     go (ApplyLayout (LayoutLambda adt e) layout) = instantiate1 (LayoutTypeArg layout) e
--     -- go e = e
--
