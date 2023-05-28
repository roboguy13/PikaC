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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}

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
    , _layoutBranches ::
        Bind [ModedName a] -- Layout parameters
          [LayoutBranch a]
    }
    deriving (Show, Generic)

newtype LayoutBranch a =
  LayoutBranch
    { _layoutMatch ::
        PatternMatch a 
          (Bind [Exists a] -- Existential variables
            (LayoutBody a))
    }
    deriving (Show, Generic)

-- | A layout where the layout parameters have been substituted
type OpenedLayout a = [LayoutBranch a]

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

-- type Layout = Layout' Identity

newtype Exists a = Exists (ModedName a)
  deriving (Show, Generic)

instance (Typeable a) => Alpha (Exists a)
instance Subst a (ModedName a) => Subst a (Exists a)

layoutBranchPattern :: LayoutBranch a -> Pattern a
layoutBranchPattern = patternMatchPat . _layoutMatch

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

-- getLayoutParams :: Layout a -> [ModedName a]
-- getLayoutParams = view (_1 . layoutBranches)

unbindLayout :: (Fresh m, Typeable a, Alpha a) => Layout a -> m ([ModedName a], [LayoutBranch a])
unbindLayout = unbind . _layoutBranches


instance (Subst (Moded a) (Exists a), Alpha a, Typeable a, Subst (Moded a) a) => Subst (Moded a) (LayoutBranch a)
instance Subst (Moded a) a => Subst (Moded a) (Pattern a)
instance Subst (Moded a) a => Subst (Moded a) (LayoutBody a)
instance Subst (Moded a) a => Subst (Moded a) (LayoutHeaplet a)
instance Subst (Moded a) a => Subst (Moded a) (PointsTo a)
instance Subst (Moded a) a => Subst (Moded a) (Loc a)
instance (Alpha a, Typeable a, Subst (Moded a) a) => Subst (Moded a) (PatternMatch a (LayoutBody a))
instance (Alpha a, Typeable a, Subst (Moded a) (Exists a), Subst (Moded a) a) => Subst (Moded a) (PatternMatch a (Bind [Exists a] (LayoutBody a)))

instance (IsNested a, Ppr a) => Ppr (LayoutHeaplet a) where
  ppr (LPointsTo p) = ppr p
  ppr (LApply f patVar layoutVars) =
    text f
      <+> pprP patVar
      <+> (text "["
          <> hsep (punctuate (text ",") (map ppr layoutVars))
          <> text "]")

instance (Alpha a, Typeable a) => Alpha (Layout a)

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

openLayout :: (Fresh m, Alpha a, Typeable a, Subst a (LayoutBranch a), HasVar a) => Layout a -> m ([ModedName a], OpenedLayout a)
openLayout layout = do
  let branches@(B vs _) = _layoutBranches layout
      modes = map getMode vs
  (vs', opened) <- freshOpen branches
  pure (zipWith Moded modes vs', opened)

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
      case layoutBranchPattern x of
        PatternVar _ -> Just (B vs x)
        Pattern c _
          | c == constructor -> Just (B vs x)
          | otherwise -> go (B vs xs)

lookupLayoutBranch' :: (HasCallStack, HasApp a) => Layout a -> String -> Bind [ModedName a] (LayoutBranch a)
lookupLayoutBranch' layout c =
  case lookupLayoutBranch layout c of
    Nothing -> error $ "lookupLayoutBranch: Cannot find branch for constructor " ++ c
    Just r -> r

lookupOpenedLayoutBranch :: forall a. HasApp a => OpenedLayout a -> String -> Maybe (LayoutBranch a)
lookupOpenedLayoutBranch branches constructor = go branches
  where
    go [] = Nothing
    go (x:xs) =
      case layoutBranchPattern x of
        PatternVar _ -> Just x
        Pattern c _
          | c == constructor -> Just x
          | otherwise -> go xs

lookupOpenedLayoutBranch' :: forall a. (HasCallStack, HasApp a) => OpenedLayout a -> String -> LayoutBranch a
lookupOpenedLayoutBranch' branches c =
  case lookupOpenedLayoutBranch branches c of
    Nothing -> error $ "lookupOpenedLayoutBranch': Cannot find branch for constructor " ++ c
    Just r -> r

openLayoutBranch :: (HasCallStack, Fresh m, HasApp a, Alpha a, Typeable a, Subst a (LayoutBranch a), HasVar a) =>
  Bind [ModedName a] (LayoutBranch a) -> m ([ModedName a], LayoutBranch a)
openLayoutBranch bnd@(B vs _) = do
  let modes = map getMode vs
  (vs', branch) <- freshOpen bnd
  pure (zipWith Moded modes vs', branch)

-- | Apply layout to a constructor value
applyLayout :: (Fresh m, Subst (Moded a) a, Subst a (ModedName a), Alpha a, Typeable a, HasApp a, HasApp a, Subst a (LayoutBody a)) =>
  Layout a -> String -> [a] -> m (Maybe (Bind [ModedName a] (Bind [Exists a] (LayoutBody a))))
applyLayout layout constructor args = do
  sequenceA (unbind <$> lookupLayoutBranch layout constructor) >>= \case
    Nothing -> pure Nothing
    Just (params, branch) ->
      let matched = applyPatternMatch' (_layoutMatch branch) constructor args
      in
      pure . Just $ bind params matched

applyLayout' :: (Fresh m, Subst (Moded a) a, Subst a (ModedName a), Alpha a, Typeable a, HasApp a, HasApp a, Subst a (LayoutBody a)) =>
  Layout a -> String -> [a] -> m (Bind [ModedName a] (Bind [Exists a] (LayoutBody a)))
applyLayout' layout c args =
  applyLayout layout c args >>= \case
    Nothing -> error $ "applyLayout': Cannot find branch for constructor " ++ c ++ " in " ++ show layout
    Just r -> pure r

getPointsTos :: LayoutBody a -> [PointsTo a]
getPointsTos b@(LayoutBody xs0) = go xs0
  where
    go [] = []
    go (LApply {} : xs) = go xs
    go (LPointsTo p : xs) = p : go xs

instance (Typeable a, Show a, Alpha a) => Alpha (LayoutHeaplet a)

instance (Show a, Typeable a, Alpha a) => Alpha (LayoutBody a)
instance (Show a, Typeable a, Alpha a) => Alpha (LayoutBranch a)

-- -- | A name mapping gotten by applying a layout. This can be used
-- -- for recursively applying layouts
-- type LayoutMapping a = [(Name a, Name a)]

-- -- | Follow the LApply's one time to get a LayoutMapping for
-- -- those layout applications
-- mappingFromLApplies :: [Layout a] -> LayoutBody a -> LayoutMapping a
-- mappingFromLApplies = undefined

-- -- | Includes parameters, excludes pattern variables
-- layoutBranchFVs :: (Show a, Typeable a, Alpha a) => LayoutBranch a -> [Name a]
-- layoutBranchFVs branch =
--   filter (`notElem` getPatternNames (_layoutPattern branch))
--     $ toListOf fv branch

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
