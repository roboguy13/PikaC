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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module PikaC.Syntax.Pika.Layout
  where

import PikaC.Syntax.Heaplet
import PikaC.Syntax.Pika.Pattern
import PikaC.Syntax.Type

import PikaC.Stage

import PikaC.Ppr
import PikaC.Utils

import Control.Monad.Identity

import GHC.Stack

-- import Bound
-- import Bound.Var

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Bind
import Unbound.Generics.LocallyNameless.Unsafe

import Data.Void
import Data.Typeable

import Data.Bifunctor

import Control.Lens hiding (elements)
import Control.Lens.TH

import GHC.Generics

import Data.List

import Control.Applicative
import Control.Monad

import Test.QuickCheck

import Debug.Trace

import Data.Validity

import Control.DeepSeq

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

instance NFData a => NFData (Layout a)
instance NFData a => NFData (LayoutBranch a)
instance NFData a => NFData (Moded a)
instance NFData a => NFData (Exists a)
instance NFData a => NFData (LayoutBody a)
instance NFData a => NFData (LayoutHeaplet a)
instance NFData Mode


-- | A layout where the layout parameters have been substituted
type OpenedLayout a = [LayoutBranch a]

data Moded' (s :: Stage) a = Moded' (XModed s) Mode a
  deriving (Generic)

deriving instance (Show (XModed s), Show a) => Show (Moded' s a)

type family XModed (s :: Stage)
type instance XModed PC = ()

type Moded = Moded' PC

pattern Moded x y = Moded' () x y

type ModedName' s a = Moded' s (Name a)
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

instance (Ppr a) => Ppr (ModedName' s a) where
  ppr (Moded' _ mode n) = ppr mode <> ppr n

instance Ppr Mode where
  ppr In = text "+"
  ppr Out = text "-"

instance (Alpha (XModed s), Show a, Alpha a, Typeable a) => Alpha (Moded' s a)

instance Alpha Mode

-- type Layout = Layout' Identity

newtype Exists a = Exists { getExists :: ModedName a }
  deriving (Show, Generic)

instance (Typeable a) => Alpha (Exists a)
instance Subst a (ModedName a) => Subst a (Exists a)

instance HasNames (Exists a) a where
  getNames (Exists x) = getNames x

instance HasNames a b => HasNames (Moded a) b where
  getNames (Moded _ x) = getNames x

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
      [a]    -- Layout variables
      -- [LocVar]    -- Layout variables
  deriving (Show, Generic)

-- | Split into points-to and applications
splitLayoutHeaplets ::
  [LayoutHeaplet a] ->
  ([PointsTo a], [(String, a, [a])])
splitLayoutHeaplets [] = ([], [])
splitLayoutHeaplets (LPointsTo p : xs) =
  first (p:) (splitLayoutHeaplets xs)
splitLayoutHeaplets (LApply layoutName exprArg layoutVars : xs) =
  second ((layoutName, exprArg, layoutVars) :) (splitLayoutHeaplets xs)

-- type PatternVar = Name Expr

makeLenses ''Layout
makeLenses ''LayoutBranch
makeLenses ''LayoutBody
makePrisms ''LayoutHeaplet

-- -- NOTE: This is just for testing
-- getFirstBranch :: (MonadFail m, Fresh m, Typeable a, Alpha a) =>
--   Layout a ->
--   m (Bind [ModedName a]
--       (Bind [Exists a]
--         (LayoutBody a)))
-- getFirstBranch layout = do
--   (vars, (LayoutBranch branch:_)) <- unbind (_layoutBranches layout)
--   pure $ bind vars branch

instance Ppr a => Ppr (Exists a) where ppr (Exists x) = ppr x

instance (IsNested a, Ppr a) => Ppr (LayoutBody a) where
  ppr (LayoutBody []) = text "emp"
  ppr (LayoutBody xs) = hsep (intersperse (text "**") (map ppr xs))

instance (Subst (Name a) a, Alpha a, Typeable a) => Subst (Name a) (LayoutBranch a)
instance (Subst (Name a) a, Alpha a, Typeable a) => Subst (Name a) (PatternMatch a (Bind [Exists a] (LayoutBody a)))
instance Subst (Name a) (Exists a)
instance Subst (Name a) (Moded (Name a))
instance Subst (Name a) Mode
instance Subst (Name a) a => Subst (Name a) (LayoutBody a)
instance Subst (Name a) a => Subst (Name a) (LayoutHeaplet a)
instance Subst (Pattern a) a => Subst (Pattern a) (LayoutBody a)
instance Subst (Pattern a) a => Subst (Pattern a) (LayoutHeaplet a)
instance Subst (Pattern a) (Exists a)
instance Subst (Pattern a) (Moded (Name a))
instance Subst (Pattern a) Mode
instance Subst (Exists a) a => Subst (Exists a) (LayoutBody a)
instance Subst (Exists a) a => Subst (Exists a) (LayoutHeaplet a)

instance Subst (Exists a) a => Subst (Exists a) (PointsTo a)
instance Subst (Exists a) a => Subst (Exists a) (Loc a)

instance forall a. (Subst (Exists a) a, Subst (Pattern a) a, Subst (Name a) a, IsNested a, HasVar a, Subst a (LayoutBody a), Subst a (LayoutBranch a), Subst a (ModedName a), Typeable a, Alpha a, Ppr a) =>
    Ppr (Layout a) where
  ppr layout =
    let bnd@(B params branches) = _layoutBranches layout
        name = _layoutName layout

        -- branchLines = map (go name) $ instantiate bnd (map (mkVar . modedNameName) params)
        branchLines = map (go name) $ openBind bnd
    in

    ((text name <+> text ":" <+> text "layout[") <> hsep (punctuate (text ",") (map ppr params)) <> text "](" <> ppr (_layoutAdt layout) <> text ")"
           $$
           vcat branchLines)
    where
      go :: String -> LayoutBranch a -> Doc
      go name (LayoutBranch (PatternMatch branch)) =
        -- let bnd1@(B pat opened) = branch
        --     bodyBnd@(B existVars _) = instantiate bnd1 [pat]
        --     body = instantiate bodyBnd existVars
        let (B pat _) = branch

            bnd :: Bind [Exists a] (LayoutBody a)
            bnd = openBind1 branch

            (B existVars _) = bnd
            body = openBind @a bnd

            exists =
              if null existVars
                then mempty
                else (text "exists" <+> hsep (punctuate (text ",") (map ppr existVars))) <> text "."
        in

        (text name <+> ppr pat <+> text ":="
                $$ nest 2 (exists $$ ppr body))

getLayoutParams :: Layout a -> [ModedName a]
getLayoutParams layout =
  let B vs _ = _layoutBranches layout
  in
  vs

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

instance Subst (Exists a) AdtName
instance Subst (Exists a) (Moded (Name a))
instance Subst (Exists a) Mode

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

lookupLayout :: HasCallStack => LayoutEnv a -> String -> Layout a
lookupLayout [] name = error $ "lookupLayout: Cannot find layout " ++ name
lookupLayout (x:xs) name
  | _layoutName x == name = x
  | otherwise = lookupLayout xs name

-- class ModedC f where
--   findMode :: ModeEnv a -> f (Name a) -> Maybe Mode
--
-- -- TODO: Make sure this is consistent with environment?
-- instance ModedC Moded where
--   findMode _ (Moded m _) = Just m

openLayout :: (Fresh m, Alpha a, Typeable a, Subst a (LayoutBranch a), HasVar a) => Layout a -> m ([ModedName a], OpenedLayout a)
openLayout layout = do
  let branches = _layoutBranches layout
      -- modes = map getMode vs
  -- (vs', opened) <- freshOpen branches
  unbind branches

instance IsName (Moded (Name a)) a where
  getName = modedNameName

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
--
-- applyLayoutBranch ::
--   (Fresh m, Subst (Moded a) a, Subst a (ModedName a), Alpha a, Typeable a, HasApp a, HasApp a, Subst a (LayoutBody a)) =>
--   LayoutBranch a -> String -> [a] -> m (Bind [ModedName a] (Bind [Exists a] (LayoutBody a)))
applyLayoutBranch
  :: (Typeable a, Alpha a, HasApp a, Subst a (LayoutBody a),
      Subst a (ModedName a)) =>
     LayoutBranch a
     -> String -> [a] -> Either String (Bind [Exists a] (LayoutBody a))
applyLayoutBranch branch constructor args =
    applyPatternMatch (_layoutMatch branch) constructor args

applyLayoutBranchPattern
  :: (Typeable a, HasApp (Name a),
      Subst (Name a) (LayoutBody (Name a)),
      Subst (Name a) (ModedName (Name a))) =>
     LayoutBranch (Name a)
     -> Pattern a
     -> Either String (Bind [Exists (Name a)] (LayoutBody (Name a)))
applyLayoutBranchPattern branch (PatternVar {}) = Left "applyLayoutBranchPattern: PatternVar"
applyLayoutBranchPattern branch (Pattern c args) = applyLayoutBranch branch c args

applyLayoutBranchPattern'
  :: (Typeable a, HasApp (Name a),
      Subst (Name a) (LayoutBody (Name a)),
      Subst (Name a) (ModedName (Name a))) =>
     LayoutBranch (Name a)
     -> Pattern a -> Bind [Exists (Name a)] (LayoutBody (Name a))
applyLayoutBranchPattern' branch pat =
  case applyLayoutBranchPattern branch pat of
    Left e -> error e
    Right r -> r

-- applyLayoutBranchPatternM
--   :: (Fresh m, Typeable a, HasApp (Name a),
--       Subst (Name a) (LayoutBody (Name a)),
--       Subst (Name a) (ModedName (Name a))) =>
--      LayoutBranch (Name a)
--      -> Pattern a -> m (LayoutBody (Name a))
-- applyLayoutBranchPatternM branch pat = do
--   let bnd@(B vs _) = applyLayoutBranchPattern' branch pat
--   vs' <- mapM fresh (concatMap getNames vs)
--   pure $ instantiate bnd (map mkVars vs')

-- | Apply layout to a constructor value
applyLayout :: (Fresh m, Subst (Moded a) a, Subst a (ModedName a), Alpha a, Typeable a, HasApp a, HasApp a, Subst a (LayoutBody a)) =>
  Layout a -> String -> [a] -> m (Maybe (Bind [ModedName a] (Bind [Exists a] (LayoutBody a))))
applyLayout layout constructor args =
  sequenceA (unbind <$> lookupLayoutBranch layout constructor) >>= \case
    Nothing -> pure Nothing
    Just (params, branch) ->
      case applyLayoutBranch branch constructor args of
        Left {} -> pure Nothing
        Right b -> pure . Just $ bind params b

applyLayout' :: (Fresh m, Subst (Moded a) a, Subst a (ModedName a), Alpha a, Typeable a, HasApp a, HasApp a, Subst a (LayoutBody a)) =>
  Layout a -> String -> [a] -> m (Bind [ModedName a] (Bind [Exists a] (LayoutBody a)))
applyLayout' layout c args =
  applyLayout layout c args >>= \case
    Nothing -> error $ "applyLayout': Cannot find branch for constructor " ++ c ++ " in " ++ show layout
    Just r -> pure r

freshExists :: (Fresh m, Alpha a, Subst a (LayoutBody a), HasVar a, Typeable a) =>
  Bind [ModedName a] (Bind [Exists a] (LayoutBody a)) ->
  m (Bind [ModedName a] (LayoutBody a))
freshExists bnd0 = do
  (vars, bnd1) <- unbind bnd0
  (_, bnd2) <- freshOpenExists bnd1
  pure (bind vars bnd2)

freshOpenExists ::  (Fresh m, Alpha b, Subst c b, HasVar c, Alpha c, Typeable c) =>
     Bind [Exists c] b -> m ([Exists c], b)
freshOpenExists bnd@(B vs _) = do
  vs' <- mapM fresh (concatMap getNames vs)
  let moded = zipWith Moded (map (getMode . getExists) vs) vs'
  pure (map Exists moded, instantiate bnd (map mkVar vs'))

applyLayoutPattern
  :: (Ppr a, Typeable a, Alpha a, HasApp a, Subst a (LayoutBody a),
      Subst a (ModedName a), HasVar a) =>
     [LayoutBranch a]
     -> Pattern a -> Either [Char] (Bind [Exists a] (LayoutBody a))
applyLayoutPattern layout (PatternVar {}) = Left "applyLayoutPattern: PatternVar"
applyLayoutPattern layout pat@(Pattern c args) =
  go layout
  where
    go [] = Left $ "applyLayoutPattern: Cannot find layout for pattern " ++ ppr' pat ++ " in " ++ show layout
    go (x:xs) = applyLayoutBranch x c (map mkVar args) <> go xs

applyLayoutPattern'
  :: (Ppr a, Typeable a, Alpha a, HasApp a, Subst a (LayoutBody a),
      Subst a (ModedName a), HasVar a) =>
     [LayoutBranch a] -> Pattern a -> Bind [Exists a] (LayoutBody a)
applyLayoutPattern' layout pat =
  case applyLayoutPattern layout pat of
    Left e -> error e
    Right r -> r

applyLayoutBranchPatternM 
  :: (Fresh m, Ppr a, Typeable a, Alpha a, HasApp a, Subst a (LayoutBody a),
      Subst a (ModedName a), HasVar a) =>
     LayoutBranch a -> Pattern a -> m (LayoutBody a)
applyLayoutBranchPatternM branch pat = do
  let bnd@(B vs _) = applyLayoutPattern' [branch] pat
  vs' <- mapM fresh (concatMap getNames vs)
  pure $ instantiate bnd (map mkVar vs')

applyLayoutPatternM 
  :: (Fresh m, Ppr a, Typeable a, Alpha a, HasApp a, Subst a (LayoutBody a),
      Subst a (ModedName a), HasVar a) =>
     [LayoutBranch a] -> Pattern a -> m (LayoutBody a)
applyLayoutPatternM layout pat = do
  let bnd@(B vs _) = applyLayoutPattern' layout pat
  vs' <- mapM fresh (concatMap getNames vs)
  pure $ instantiate bnd (map mkVar vs')

applyLayoutPatternMaybe
  :: (Fresh m, Ppr a, Typeable a, Alpha a, HasApp a, Subst a (LayoutBody a),
      Subst a (ModedName a), HasVar a) =>
     Maybe (OpenedLayout a) -> Pattern a -> m (LayoutBody a)
applyLayoutPatternMaybe Nothing pat = pure mempty
applyLayoutPatternMaybe (Just layout) pat =
  applyLayoutPatternM layout pat

getPointsTos :: LayoutBody a -> [PointsTo a]
getPointsTos b@(LayoutBody xs0) = go xs0
  where
    go [] = []
    go (LApply {} : xs) = go xs
    go (LPointsTo p : xs) = p : go xs

getLApplies :: LayoutBody a -> [(String, a, [a])]
getLApplies b@(LayoutBody xs0) = go xs0
  where
    go [] = []
    go ((LApply n e vs) : xs) = (n, e, vs) : go xs
    go (LPointsTo {} : xs) = go xs

maxAllocsForLayout :: forall a. (HasVar a, IsName a a, Subst a (Layout a), Subst a (LayoutBranch a), Typeable a, Alpha a) => Layout a -> [Name a] -> [Allocation a]
maxAllocsForLayout layout params =
    let branches = instantiate (_layoutBranches layout) (map mkVar params)
    in
      concatMap go branches
  where
    go (LayoutBranch (PatternMatch m)) =
      let B _ bnd1 = m
          B _ body = bnd1
      in
      findAllocations params $ getPointsTos body

layoutLAppliesMaxAllocs :: forall a. (HasVar a, IsName a a, Subst a (Layout a), Subst a (LayoutBranch a), Typeable a, Alpha a) => [Layout a] -> [LayoutBody a] -> [Allocation a]
layoutLAppliesMaxAllocs layouts = toMaxAllocs . concatMap (go . _unLayoutBody)
  where
    go :: [LayoutHeaplet a] -> [Allocation a]
    go [] = []
    go (LApply n e vs : xs) =
      let layout = lookupLayout layouts n
      in
        maxAllocsForLayout layout (map getName vs) ++ go xs
    go (LPointsTo {} : xs) = go xs

    -- go' vs (LayoutBranch (PatternMatch m)) =
    --   let B _ bnd1 = m
    --       B _ body = bnd1
    --   in
    --   findAllocations (map getName vs) $ getPointsTos body

-- -- A e [x ...]
-- substLApplies

instance (Typeable a, Show a, Alpha a) => Alpha (LayoutHeaplet a)

instance (Show a, Typeable a, Alpha a) => Alpha (LayoutBody a)
instance (Show a, Typeable a, Alpha a) => Alpha (LayoutBranch a)

type LayoutName = TypeName

-- getLayoutSig ::
--   Layout a ->
--   (AdtName, [(String, Int)]) ->
--   (LayoutName, [(String, [Maybe LayoutName])])
-- getLayoutSig layout adtSig =
--   let B params branches = _layoutBranches layout
--   in
--   (string2Name $ _layoutName layout, map go branches)
--   where
--     go (LayoutBranch (PatternMatch (B (Pattern cName patVars) (B existVars body)))) = undefined

--
-- Property tests --
--

instance (Typeable a, Alpha a, WellScoped (Name a) a) => WellScoped (ModedName a) (Layout a)

instance WellScoped a AdtName where
  wellScoped _ _ = mempty

instance (Typeable a, Alpha a, WellScoped (Name a) b) =>
  WellScoped (ModedName a) (Bind [ModedName a] b) where
    wellScoped inScopeVars bnd =
      let (vs, body) = unsafeUnbind bnd
          inScopeVars' :: [Name a]
          inScopeVars' = map modedNameName inScopeVars
      in
      wellScoped @(Name a)
        (inScopeVars' ++ map modedNameName vs)
        body

instance (Typeable a, Alpha a, WellScoped (Name a) a) => WellScoped (Name a) (LayoutBranch a)

instance (Typeable a, Alpha a, Typeable b, Alpha b, WellScoped (Name a) b) => WellScoped (Name a) (Bind [Exists a] b) where
  wellScoped inScopeVars bnd =
    let (vars, body) = unsafeUnbind bnd
    in
    wellScoped (inScopeVars ++ map (modedNameName . getExists) vars) body

instance (Typeable a, Alpha a, Typeable b, Alpha b, WellScoped (Name a) b) => WellScoped (Name a) (Bind [ModedName a] b) where
  wellScoped inScopeVars bnd =
    let (vars, body) = unsafeUnbind bnd
    in
    wellScoped (inScopeVars ++ map modedNameName vars) body

instance (Typeable a, Alpha a, WellScoped (Name a) a) => WellScoped (Name a) (LayoutBody a)
instance (Typeable a, Alpha a, WellScoped (Name a) a) => WellScoped (Name a) (LayoutHeaplet a)

instance (IsName a a, Alpha a, Typeable a, Show a, WellScoped (Name a) a) => Validity (Layout a) where
  validate layout =
    let (_, branches) = unsafeUnbind $ _layoutBranches layout
    in
    wellScoped @(ModedName a) [] layout <>
    mconcat (map validate branches)

instance (IsName a a, Alpha a, Typeable a, Show a) => Validity (LayoutBranch a) where
  validate branch@(LayoutBranch (PatternMatch (B (PatternVar _) _))) =
    decorate (show branch) $
    invalid "Layout branch matches has pattern variable match rather than matching on a constructor"

  validate branch@(LayoutBranch (PatternMatch (B (Pattern cName params) body))) =
    allExistsAreUsed branch
    <> allPatVarsAreUsed branch

allExistsAreUsed :: (Alpha a, Typeable a) => LayoutBranch a -> Validation
allExistsAreUsed (LayoutBranch (PatternMatch (B (Pattern cName params) bnd))) =
  let (existVars, body) = unsafeUnbind bnd
      bodyFvs = toListOf fv body
  in
  check (all (`elem` bodyFvs) (map (modedNameName . getExists) existVars))
    "All existentials in a layout branch must be used"

allPatVarsAreUsed :: (IsName a a, Alpha a, Typeable a) => LayoutBranch a -> Validation
allPatVarsAreUsed (LayoutBranch (PatternMatch bnd)) =
  let (Pattern cName patVars, bnd1) = unsafeUnbind bnd
      (existVars, body) = unsafeUnbind bnd1
      rhs's = map (getName . pointsToRhs) $ getPointsTos body
  in
  check (all (`elem` rhs's) patVars)
    "All pattern variables are used in layout branch"

instance (IsName a a, Typeable a, Alpha a, IsBase a, Arbitrary a, WellScoped (Name a) a) => Arbitrary (Layout a) where
  arbitrary = error "Arbitrary Layout"
  shrink layout = filter isValid $ do
    let (vars, branches) = unsafeUnbind $ _layoutBranches layout
    branches' <- filter isValid . sequenceA $ map shrink branches
    pure $ layout
      { _layoutBranches = bind vars branches'
      }

instance (IsName a a, Alpha a, Typeable a, IsBase a, Arbitrary a) => Arbitrary (LayoutBranch a) where
  arbitrary = error "Arbitrary LayoutBranch"
  shrink =  filter isValid . genericShrink

instance (IsBase a, Arbitrary a) => Arbitrary (LayoutBody a) where
  arbitrary = error "Arbitrary LayoutBody"
  shrink = genericShrink

instance (IsBase a, Arbitrary a) => Arbitrary (LayoutHeaplet a) where
  arbitrary = error "Arbitrary LayoutHeaplet"
  shrink (LApply layoutName x ys) =
    LApply layoutName <$> shrink x <*> sequenceA (map shrink ys)
  shrink e = genericShrink e

-- NOTE: Only Out mode parameters for now
genLayout :: (IsNested a, Ppr a, Typeable a, Alpha a, HasVar a, IsName a a) =>
  String ->
  [(AdtName, [(String, [AdtArg])])] ->  -- ADTs and their constructor names and the constructor arities
  Int ->
  Gen (Layout a)
genLayout lName adts size = do
  (adtName, constructors) <- elements' adts

  -- n <- choose (1, 3)
  -- params <- map string2Name <$> genParamNames n

  -- TODO: Generate various numbers of parameters. For now, we always
  -- generate one parameter.
  params <- map string2Name <$> genParamNames 1

  let dividedSize = size `div` length constructors

  branches <- mapM (genLayoutBranch lName params dividedSize) constructors

  pure $
    Layout
    { _layoutName = lName
    , _layoutAdt = adtName
    , _layoutBranches =
        bind (map (Moded Out) params)
          branches
    }

-- TODO: Remove unused existentials and layout parameters
genLayoutBranch :: (IsNested a, Ppr a, Typeable a, Alpha a, HasVar a, IsName a a) =>
  String ->
  [Name a] ->
  Int ->
  (String, [AdtArg]) ->
  Gen (LayoutBranch a)
genLayoutBranch layoutName params size (constructor, arity) = do
  n <- chooseInt (1, 3)
  patVars <- fmap (map string2Name) (genParamNames (length arity)) `suchThat` disjoint params
  let namesSoFar = patVars ++ params
  existsNames <- fmap (map string2Name) (genParamNames n) `suchThat` disjoint namesSoFar
  -- k <- choose (2, 7)
  -- let shuffledLen = max (length patVars) k
  let dividedSize = size `div` length patVars
  -- body <- replicateM k (genLayoutHeaplet layoutName patVars params existsNames dividedSize) -- `suchThat` noDupPointsToLhs
  shuffledPatVars <- shuffle patVars
  shuffledParams <- fmap cycle $ shuffle params

  (usedLocs1, body1) <- genLayoutRequiredPointsTo layoutName [] shuffledPatVars shuffledParams dividedSize `suchThat` (\xs -> null patVars || not (null xs)) -- `suchThat` noDupPointsToLhs

  additionalHeaplets <- chooseInt (0, 8)
  (usedLocs2, body2) <- genLayoutHeaplets additionalHeaplets usedLocs1 layoutName patVars params size
      -- `suchThat` noDupPointsToLhs

  (existsVarsUsed, body3) <- genExistHeaplets layoutName patVars params existsNames size

  let body = body1 ++ body2 ++ body3

  let lhs's = map (fmap getName) $ map pointsToLhs (getPointsTos (LayoutBody body))

  if (not (noDups lhs's))
    then error "unreachable?"
    else pure $
      LayoutBranch
        $ PatternMatch
            $ bind (Pattern constructor patVars)
              $ bind (map (Exists . Moded In) existsVarsUsed)
                $ LayoutBody body

-- noDupPointsToLhs :: IsName a a => [LayoutHeaplet a] -> Bool
-- noDupPointsToLhs = noDups . map (getName . locBase . pointsToLhs) . getPointsTos . LayoutBody

noDupPointsToLhs :: IsName a a => [LayoutHeaplet a] -> Bool
noDupPointsToLhs = noDups . map (fmap getName) . map pointsToLhs . getPointsTos . LayoutBody

genLayoutHeaplets :: (IsName a a, HasVar a) =>
  Int ->
  [Loc (Name a)] ->
  String ->
  [Name a] ->
  [Name a] ->
  Int ->
  Gen ([Loc (Name a)], [LayoutHeaplet a])
genLayoutHeaplets 0 usedLocs layoutName patVars params size = pure (usedLocs, [])
genLayoutHeaplets _ usedLocs layoutName [] params size = pure (usedLocs, [])
genLayoutHeaplets count usedLocs layoutName patVars params size = do
  (usedLoc, h) <- genLayoutHeaplet usedLocs layoutName patVars params size
  (usedLocs', hs) <- genLayoutHeaplets (count-1) (usedLoc:usedLocs) layoutName patVars params size
  pure (usedLocs', h:hs)

genLayoutHeaplet :: (IsName a a, HasVar a) =>
  [Loc (Name a)] ->
  String ->
  [Name a] ->
  [Name a] ->
  Int ->
  Gen (Loc (Name a), LayoutHeaplet a)
genLayoutHeaplet usedLocs layoutName patVars params size = do
    p <- genValidPointsToRestricted usedLocs
            params (\_ -> mkVar <$> elements' (patVars)) (size - 1)
      -- `suchThat` ((`notElem` usedLocs) . fmap getName . pointsToLhs)
    pure (fmap getName (pointsToLhs p), LPointsTo p)
    -- ++
    -- if null patVars
    --   then []
    --   else [ LApply layoutName
    --     <$> fmap mkVar (elements' patVars)
    --     <*> fmap (map mkVar) (replicateM (length params) (elements' (existVars `union` params)))
    --     ]

genExistHeaplets :: (HasVar a) =>
  String ->
  [Name a] ->
  [Name a] ->
  [Name a] ->
  Int ->
  Gen ([Name a], [LayoutHeaplet a])
genExistHeaplets layoutName [] params existVars size | size <= 0 = discardM
genExistHeaplets layoutName [] params existVars size = pure ([], mempty)
genExistHeaplets layoutName (patVar:patVars) params existVars size = do

  -- ix <- chooseInt (1, length existVars)
  let arity = length params
  let (here, rest) = splitAt (arity-1) existVars -- We use arity-1 so that we always get at least one item from params when we make xs


  xs <- shuffle $ here ++ take (arity - length here) params

  (existVars', heaplets) <- genExistHeaplets layoutName patVars params rest (size-1)

  pure $ (here ++ existVars', LApply layoutName (mkVar patVar) (map mkVar xs) : heaplets)

genLayoutRequiredPointsTo :: (IsName a a, HasVar a) =>
  String ->
  [Loc (Name a)] ->
  [Name a] ->
  [Name a] ->
  Int ->
  Gen ([Loc (Name a)], [LayoutHeaplet a])
genLayoutRequiredPointsTo _ _          []      _               _ = pure ([], [])
genLayoutRequiredPointsTo _ _          _       []              _ = pure ([], [])
-- genLayoutRequiredPointsTo layoutName patVars remainingParams existVars size | size <= 0 = pure []
genLayoutRequiredPointsTo usedLocs layoutName (patVar:patVars) (param:remainingParams) size = do
  (usedLocs', rest) <- genLayoutRequiredPointsTo usedLocs layoutName patVars remainingParams (size-1)
  here <- genValidPointsToRestricted usedLocs' [param] (\_ -> pure $ mkVar patVar) size
  -- here <- (genValidPointsTo [param] (\_ -> pure $ mkVar patVar) size)
  --           `suchThat` ((`notElem` usedLocs) . fmap getName . pointsToLhs)
  pure (fmap getName (pointsToLhs here) : usedLocs'
       , LPointsTo here : rest)
    

genLayoutName :: Gen String
genLayoutName = do
  i <- chooseInt (0,4) :: Gen Int
  x <- arbitraryUppercase
  prefix <- replicateM i arbitraryAnyCase
  pure (x : prefix ++ "_layout")


genParamNames :: Int -> Gen [String]
genParamNames n =
  replicateM n genNameString `suchThat` noDups

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
