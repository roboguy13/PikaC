{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -Wincomplete-patterns #-}

module PikaC.Stage.ToPikaCore
  (toPikaCore)
  where

import qualified PikaC.Syntax.Pika.Expr as Pika
import qualified PikaC.Syntax.Pika.FnDef as Pika

import qualified PikaC.Syntax.PikaCore.Expr as PikaCore
import qualified PikaC.Syntax.PikaCore.FnDef as PikaCore

import PikaC.Syntax.Pika.Layout
import PikaC.Syntax.Type
import PikaC.Syntax.Heaplet
import PikaC.Syntax.Pika.Pattern
import PikaC.Ppr

import PikaC.Stage.ToPikaCore.Monad
import PikaC.Stage.ToPikaCore.Simplify

import PikaC.Stage.ToPikaCore.Utils
import PikaC.Stage.ToPikaCore.SimplifyM

import PikaC.Utils

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Bind

import GHC.Stack

import Data.Void
import Data.List
import Data.Maybe

import Control.Lens hiding (simple)
import Control.Lens.Extras

import Control.Applicative
import Control.Arrow (second)

import Control.Monad
import Control.Monad.Reader

import Debug.Trace

import Data.Char

-- | This does a basic conversion and then hands it off to the simplifier.
--
-- Preconditions:
--   1. The layouts and the function definition should be
--      well-moded.
--   2. There should be no LayoutLambda's
--
toPikaCore :: forall m. Logger m => SimplifyFuel -> [Layout Pika.Expr] -> [Pika.FnDef] -> Pika.FnDef -> m PikaCore.FnDef
toPikaCore simplifyFuel layouts0 globalFns fn = runFreshMT . runPikaIntern' $ do
  let (argTypes, resultType) = splitFnType (_typeSigTy (Pika.fnDefTypeSig fn))

  layouts <- mapM (runPikaConvert'' mempty globalFns . convertLayout) layouts0
  -- outParams <- generateParams layouts resultType

  let argLayouts = map (lookupTypeLayout layouts) argTypes
      resultLayout = lookupTypeLayout layouts resultType

  openedArgLayouts <- traverse (traverse openLayout) argLayouts
  openedResultLayout <- traverse openLayout resultLayout

  inParams <- mapM getParameters openedArgLayouts
  outParams <- getParameters openedResultLayout

  -- inAsns <- traverse (traverse (traverse freshOpen . snd)) openedArgLayouts
  -- inAsns <- mapM getAssertion 

  branches' <-
    runPikaConvert'' layouts globalFns $ mapM (convertBranch openedArgLayouts) $ Pika.fnDefBranches fn

  piLift . lift . runSimplifyFn @m simplifyFuel simplifyFnDef $
  -- pure $
    PikaCore.FnDef
      { PikaCore._fnDefName = PikaCore.FnName $ Pika.fnDefName fn
      , PikaCore._fnDefBranches =
          bind (concat inParams)
            (bind outParams branches')
      }

convertBranch :: forall m. Monad m => [Maybe ([ModedName PikaCore.Expr], OpenedLayout PikaCore.Expr)] -> Pika.FnDefBranch -> PikaConvert m PikaCore.FnDefBranch
convertBranch openedArgLayouts (Pika.FnDefBranch matches0) = do

  let argMatches = map (map _layoutMatch) . map snd $ catMaybes openedArgLayouts

  openedArgBranches <- mapM openPatternMatch $ concat argMatches
  openedLayoutBodies0 <- map snd <$>
    (mapM (freshOpen . snd) openedArgBranches
      :: PikaConvert m [([Name PikaCore.Expr], LayoutBody PikaCore.Expr)])

  matches1 <- mapM convertPattern $ patternMatchesPats matches0

  openedLayoutBodies <- zipWithM applyLayoutPatternMaybe (map (fmap snd) openedArgLayouts) matches1

  matches <- convertPatternMatches (Just openedLayoutBodies) matches0

  (_, bodyExpr) <- openPatternMatches matches0
  bodyExpr' <- convertExpr (Just openedLayoutBodies) bodyExpr

  -- inAsns <- zipWithM getAssertion openedArgLayouts $ patternMatchesPats matches
  layouts <- asks _layoutEnv
  let inAsns = map getPointsTos openedLayoutBodies
      allocs = layoutMaxAllocs layouts openedLayoutBodies

  pure $ PikaCore.FnDefBranch
    { PikaCore._fnDefBranchInputAssertions = inAsns
    , PikaCore._fnDefBranchInAllocs = allocs
    , PikaCore._fnDefBranchBody = bodyExpr'
    }

convertPatternMatches :: Monad m => Maybe [LayoutBody PikaCore.Expr] -> PatternMatches Pika.Expr Pika.Expr -> PikaConvert m (PatternMatches PikaCore.Expr PikaCore.Expr)
convertPatternMatches argLayoutBodies =
  onPatternMatches internExprName (convertExpr argLayoutBodies)

getParameters :: Fresh m => Maybe ([Moded PikaCore.ExprName], OpenedLayout PikaCore.Expr) -> m [ModedName PikaCore.Expr]
getParameters Nothing = ((:[]) . Moded In) <$> fresh (string2Name "ww")
getParameters (Just (vars, _)) = pure vars

convertAppHereUsing :: (Monad m, HasCallStack) =>
  Maybe [LayoutBody PikaCore.Expr] -> Pika.Expr ->
  Layout PikaCore.Expr ->
  PikaConvert m PikaCore.Expr
convertAppHereUsing opened e layout = do
  let params0 = getLayoutParams layout
  params <- mapM (fresh . modedNameName) params0
  let modedParams = zipWith Moded (map getMode params0) params
  convertApp opened e (modedParams, PikaCore.LayoutV (map PikaCore.V params))

convertAppHere :: (Monad m, HasCallStack) =>
  Maybe [LayoutBody PikaCore.Expr] -> Pika.Expr ->
  PikaConvert m PikaCore.Expr
convertAppHere opened e@(Pika.ApplyLayout _ layoutName) = do
  layout <- lookupLayoutM (name2String layoutName)
  convertAppHereUsing opened e layout
convertAppHere opened e@(Pika.App f args)
  | isConstructor f = error $ "convertAppHere: Found constructor that's not applied to a layout: " ++ ppr' e
  | otherwise = do
        -- TODO: Handle base types
      TyVar layoutName <- lookupFnResultType f
      layout <- lookupLayoutM (name2String layoutName)
      convertAppHereUsing opened e layout
convertAppHere opened e = convertExpr opened e

convertApps :: (Monad m, HasCallStack) =>
  Maybe [LayoutBody PikaCore.Expr] -> Pika.Expr ->
  [([ModedName PikaCore.Expr], PikaCore.Expr)] ->
  PikaConvert m PikaCore.Expr
convertApps opened e [] = convertExpr opened e
convertApps opened e ((vs, z) : rest) = undefined
  -- convertApp opened e vs (

convertApp :: (Monad m, HasCallStack) =>
  Maybe [LayoutBody PikaCore.Expr] -> Pika.Expr ->
  ([ModedName PikaCore.Expr], PikaCore.Expr) ->
  PikaConvert m PikaCore.Expr
convertApp openedArgLayouts (Pika.ApplyLayout app@(Pika.App f args) layoutName) (vs, z)
  -- | isConstructor f =
  --     lowerConstructor openedArgLayouts f args (name2String layoutName)
  -- | otherwise =
  | not (isConstructor f) =
      lowerApp openedArgLayouts (PikaCore.FnName f) args (name2String layoutName) (vs, z)
convertApp openedArgLayouts e0@(Pika.App f args) (vs, z)
  | isConstructor f = error $ "convertApp: Found constructor that's not applied to a layout: " ++ ppr' e0
  | otherwise = do
        -- TODO: Handle base types
      TyVar layoutName <- lookupFnResultType f
      lowerApp openedArgLayouts (PikaCore.FnName f) args (name2String layoutName) (vs, z)
convertApp openedArgLayouts e (vs, z) =
  PikaCore.WithIn
    <$> convertExpr openedArgLayouts e
    <*> pure (bind vs z)

lowerApp ::  Monad m =>
  Maybe [LayoutBody PikaCore.Expr] -> PikaCore.FnName -> [Pika.Expr] -> String ->
  ([ModedName PikaCore.Expr], PikaCore.Expr) ->
  PikaConvert m PikaCore.Expr
lowerApp openedArgLayouts f args layoutName (vs, z) = do
  args' <- mapM (convertExpr openedArgLayouts) args
  pure $
    PikaCore.WithIn
      (PikaCore.App f args')
      (bind vs z)

lowerConstructor :: Monad m =>
  Maybe [LayoutBody PikaCore.Expr] -> String -> [Pika.Expr] -> String ->
  -- ([ModedName PikaCore.Expr], PikaCore.Expr) ->
  PikaConvert m PikaCore.Expr
lowerConstructor openedArgLayouts f args layoutName
  | not (isConstructor f) = error $ "lowerConstructor: Requires constructor, got " ++ f
  | otherwise = do
      args' <- mapM (convertExpr openedArgLayouts) args

      layout <- lookupLayoutM layoutName
      applied <- applyLayout' layout f args'
      (params, existsBnd) <- unbind applied
      (_, LayoutBody body) <- freshOpenExists existsBnd
      convertLApplies params body

-- LApply --> WithIn
convertLApply :: Monad m => String -> PikaCore.Expr -> [PikaCore.Expr] -> PikaCore.Expr -> PikaConvert m PikaCore.Expr
convertLApply layoutName exprArg layoutVarExprs restExpr = do
  let layoutVars = map PikaCore.getV layoutVarExprs

  layout <- lookupLayoutM layoutName
  let params = getLayoutParams layout
      paramModes = map getMode params

  pure $
    PikaCore.WithIn
      exprArg
      (bind (zipWith Moded paramModes layoutVars)
        restExpr)

convertLApplies :: Monad m =>
  [ModedName PikaCore.Expr] ->
  [LayoutHeaplet PikaCore.Expr] ->
  PikaConvert m PikaCore.Expr
convertLApplies names heaplets = do
  let (pointsTos, applies) = splitLayoutHeaplets heaplets
  go applies (PikaCore.SslAssertion (bind names pointsTos))
  where
    go [] z = pure z
    go ((layoutName, exprArg, layoutVarExprs) : xs) z =
      convertLApply layoutName exprArg layoutVarExprs
        =<< go xs z

convertExpr :: forall m. (Monad m, HasCallStack) => Maybe [LayoutBody PikaCore.Expr] -> Pika.Expr -> PikaConvert m PikaCore.Expr
convertExpr openedArgLayouts = go
  where
    go :: Pika.Expr -> PikaConvert m PikaCore.Expr
    go (Pika.V x) = do
      x' <- internExprName x
      case lookupVar openedArgLayouts x' of
        [v] -> pure $ PikaCore.V v
        vs -> pure $ PikaCore.LayoutV (map PikaCore.V vs)
    go e0@(Pika.LayoutLambda {}) = error $ "convertExpr: " ++ ppr' e0
    go (Pika.ApplyLayout (Pika.V v) layoutName) = PikaCore.V <$> internExprName v -- TODO: Is this correct?

    go e@(Pika.ApplyLayout (Pika.App f args) layoutName)
      | isConstructor f = lowerConstructor openedArgLayouts f args (name2String layoutName)
    go e@(Pika.ApplyLayout {}) = convertAppHere openedArgLayouts e
    go e@(Pika.App {}) = convertAppHere openedArgLayouts e
    go (Pika.Add x y) = PikaCore.Add <$> go x <*> go y
    go (Pika.IntLit i) = pure $ PikaCore.IntLit i

lookupVar :: HasCallStack => Maybe [LayoutBody PikaCore.Expr] -> Name PikaCore.Expr -> [Name PikaCore.Expr]
lookupVar Nothing v = [v]
lookupVar (Just openedArgLayouts) v =
  case foldr (<|>) Nothing (map (`lookupVar1` v) (map _unLayoutBody openedArgLayouts)) of
    Nothing -> error $ "lookupVar: Cannot find " ++ show v ++ " in " ++ show openedArgLayouts
    Just r -> r

lookupVar1 :: [LayoutHeaplet PikaCore.Expr] -> Name PikaCore.Expr -> Maybe [Name PikaCore.Expr]
lookupVar1 [] v = Nothing
lookupVar1 (LPointsTo ((x :+ i) :-> PikaCore.V rhs) : rest) v
  | rhs == v = Just [v]
lookupVar1 (LApply layoutName (PikaCore.V patVar) layoutParams : rest) v
  | patVar == v = Just (map getName layoutParams)
lookupVar1 (_ : rest) v = lookupVar1 rest v

onBind
  :: (Applicative f, Alpha a1, Alpha a2, Alpha a3, Alpha t, Alpha a4,
      Subst a3 t, HasVar a3, HasNames a4 a3) =>
     ([a4] -> f a1) -> (t -> f a2) -> Bind [a4] t -> f (Bind a1 a2)
onBind varFn bodyFn bnd = do
  -- (vars, body) <- unbind bnd
  let vars = getBv bnd
      -- body = openBind' @a1 @a bnd
      body = openBind bnd
  bind <$> varFn vars <*> bodyFn body


-- TODO: Move these instances
instance Subst [ModedName Pika.Expr] (LayoutBranch Pika.Expr)
instance Subst [ModedName Pika.Expr] (PatternMatch Pika.Expr (Bind [Exists Pika.Expr] (LayoutBody Pika.Expr)))
instance Subst [ModedName Pika.Expr] (LayoutBody Pika.Expr)
instance Subst [ModedName Pika.Expr] (Exists Pika.Expr)
instance Subst [ModedName Pika.Expr] (Pattern Pika.Expr)
instance Subst [ModedName Pika.Expr] (LayoutHeaplet Pika.Expr)
instance Subst [ModedName Pika.Expr] (Moded (Name Pika.Expr))
instance Subst [ModedName Pika.Expr] (PointsTo Pika.Expr)
instance Subst [ModedName Pika.Expr] (Loc Pika.Expr)
instance Subst [ModedName Pika.Expr] Pika.Expr
instance Subst [ModedName Pika.Expr] Mode
instance Subst [ModedName Pika.Expr] AdtName

convertLayout :: (Monad m, HasCallStack) => Layout Pika.Expr -> PikaConvert m (Layout PikaCore.Expr)
convertLayout layout0 = do
  branches <- onBind (mapM internModedExprName) (mapM convertLayoutBranch) $ _layoutBranches layout0
  -- params <- mapM internModedExprName $ _layoutSigParams $ _layoutSig layout0

  pure $
    Layout
    { _layoutName = _layoutName layout0
    , _layoutBranches = branches
    , _layoutAdt = _layoutAdt layout0
    }

convertLayoutBranch :: forall m. (Monad m, HasCallStack) => LayoutBranch Pika.Expr -> PikaConvert m (LayoutBranch PikaCore.Expr)
convertLayoutBranch (LayoutBranch branch) = do
    LayoutBranch <$> onPatternMatch internExprName goExists branch
  where
    goExists ::
      Bind [Exists Pika.Expr] (LayoutBody Pika.Expr) ->
      PikaConvert m (Bind [Exists PikaCore.Expr] (LayoutBody PikaCore.Expr))
    goExists = onBind (mapM internExists) (fmap LayoutBody . go . _unLayoutBody)

    go :: [LayoutHeaplet Pika.Expr] -> PikaConvert m [LayoutHeaplet PikaCore.Expr]
    go = traverse go'

    go' :: LayoutHeaplet Pika.Expr -> PikaConvert m (LayoutHeaplet PikaCore.Expr)
    go' (LApply layoutName patVar vs) = do
      patVar' <- convertExpr mempty patVar
      vs' <- mapM internExprName (map getName vs)
      pure $ LApply layoutName patVar' (map PikaCore.V vs')

    go' (LPointsTo ((Pika.V a :+ i) :-> b)) = do
      a' <- internExprName a
      b' <- convertExpr mempty b
      pure (LPointsTo ((PikaCore.V a' :+ i) :-> b'))

convertPattern :: Monad m => Pattern Pika.Expr -> PikaConvert m (Pattern PikaCore.Expr)
convertPattern (PatternVar v) =
  PatternVar <$> internExprName v
convertPattern (Pattern constructor vars) =
  Pattern constructor <$> mapM internExprName vars

isConstructor :: String -> Bool
isConstructor = isUpper . head

lookupTypeLayout :: [Layout a] -> Type -> Maybe (Layout a)
lookupTypeLayout layouts (TyVar n) = Just $ lookupLayout layouts (name2String n)
lookupTypeLayout _ _ = Nothing

