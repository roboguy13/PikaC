{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}

{-# OPTIONS_GHC -Wincomplete-patterns -fprint-potential-instances #-}

module PikaC.Stage.ToPikaCore
  (toPikaCore
  ,convertLayout
  ,convertExprAndSimplify
  )
  where

import qualified PikaC.Syntax.Pika.Expr as Pika
import qualified PikaC.Syntax.Pika.FnDef as Pika

import qualified PikaC.Syntax.PikaCore.Expr as PikaCore
import qualified PikaC.Syntax.PikaCore.FnDef as PikaCore

import PikaC.Syntax.Pika.Layout
import PikaC.Syntax.Type hiding (AdtArg (..))
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

import Control.Monad
import Control.Monad.Reader

import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Functor

import Debug.Trace

import Data.Char

data OpenedArg'' a b
  = BaseArg'  -- An argument with base type
        b

  | LayoutArg a
  deriving (Show, Functor)

type OpenedArg' a b =
  OpenedArg''
    a
    (b,    -- The "local" name
    PikaCore.ExprName -- The "global" name
    )

pattern BaseArg :: b -> PikaCore.ExprName -> OpenedArg' a b
pattern BaseArg x y = BaseArg' (x, y)

instance Bifunctor OpenedArg'' where
  bimap f g (BaseArg' x) = BaseArg' (g x)
  bimap f g (LayoutArg x) = LayoutArg (f x)

instance Bifoldable OpenedArg'' where
  bifoldMap _ g (BaseArg' x) = g x
  bifoldMap f _ (LayoutArg x) = f x

instance Bitraversable OpenedArg'' where
  bitraverse _ g (BaseArg' x) = BaseArg' <$> g x
  bitraverse f _ (LayoutArg x) = LayoutArg <$> f x

type OpenedArgLayout = OpenedArg' (Layout PikaCore.Expr) ()
type OpenedArg = OpenedArg' (OpenedLayout PikaCore.Expr) ()
type OpenedArgBody = OpenedArg' (LayoutBody PikaCore.Expr) PikaCore.ExprName

getArgLayouts :: [OpenedArg' a b] -> [a]
getArgLayouts [] = []
getArgLayouts (LayoutArg x : xs) = x : getArgLayouts xs
getArgLayouts (BaseArg {} : xs) = getArgLayouts xs

overArgLayout :: Monad m => (a -> m a') -> OpenedArg' a b -> m (OpenedArg' a' b)
overArgLayout f = go
  where
    go (BaseArg x y) = pure $ BaseArg x y
    go (LayoutArg x) = LayoutArg <$> f x

mapArgLayouts :: Monad m => (a -> m a') -> [OpenedArg' a b] -> m [OpenedArg' a' b]
mapArgLayouts f = mapM (overArgLayout f)

-- toOpenedArgs :: Fresh m => [Maybe (OpenedLayout PikaCore.Expr)] -> m [OpenedArg]
-- toOpenedArgs [] = pure []
-- toOpenedArgs (Just x : xs) = do
--   xs' <- toOpenedArgs xs
--   pure (LayoutArg x : xs')
-- toOpenedArgs (Nothing : xs) = do
--   xs' <- toOpenedArgs xs
--   v <- fresh (string2Name "a")
--   pure (BaseArg () v : xs')
--   -- pure (BaseArg v : xs')

convertExprAndSimplify :: forall m. Logger m => [OpenedArgBody] -> Pika.Expr -> PikaConvert m PikaCore.Expr
convertExprAndSimplify openedLayouts e = do
  converted <- convertExpr openedLayouts e 
  (pcLift . lift . runSimplifyFn @m Unlimited simplifyExpr) converted


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

  layouts <- mapM (runPikaConvert'' layouts0 mempty globalFns . convertLayout) layouts0
  -- outParams <- generateParams layouts resultType

  argLayouts <- mapM (lookupTypeLayout layouts) argTypes
  resultLayout <- lookupTypeLayout layouts resultType

  openedArgLayouts <- mapArgLayouts openLayout argLayouts
  openedResultLayout <- overArgLayout openLayout resultLayout

  let inParams = map getParameters openedArgLayouts
      outParams = getParameters openedResultLayout

  -- openedArgs <- toOpenedArgs (map (fmap snd) openedArgLayouts)
  let openedArgs :: [OpenedArg' (OpenedLayout PikaCore.Expr) ()]
      openedArgs = map (first snd) openedArgLayouts

  -- inAsns <- traverse (traverse (traverse freshOpen . snd)) openedArgLayouts
  -- inAsns <- mapM getAssertion 

  branches' <- 
    runPikaConvert'' layouts0 layouts globalFns $ mapM (convertBranch openedArgs) $ Pika.fnDefBranches fn

  let outAllocs =
        case resultLayout of
          BaseArg' {} -> [Alloc (modedNameName (head outParams)) 0]
          LayoutArg layout -> maxAllocsForLayout layout (map modedNameName outParams)

  let layouts = zipWith getArgLayout argTypes $ map (map modedNameName) inParams

  piLift . lift . runSimplifyFn @m simplifyFuel simplifyFnDef $
  -- pure $
    PikaCore.FnDef
      { PikaCore._fnDefName = PikaCore.FnName $ Pika.fnDefName fn
      , PikaCore._fnDefOutputSizes =
          map (lookupAllocation outAllocs . modedNameName) outParams
      , PikaCore._fnDefType = _typeSigTy (Pika.fnDefTypeSig fn)
      , PikaCore._fnDefBranches =
          bind (concat inParams)
            (bind outParams (layouts, branches'))
      }

getArgLayout :: Type -> [PikaCore.ExprName] -> Maybe PikaCore.ArgLayout
getArgLayout IntType vs = Nothing
getArgLayout BoolType vs = Nothing
getArgLayout (TyVar n) vs = Just $ PikaCore.ArgLayout (name2String n) vs

mkInputs :: [OpenedArg] -> [PikaCore.ExprAssertion] -> [PikaCore.Input]
mkInputs [] [] = []
mkInputs (BaseArg () x : args) (asns) = PikaCore.InputVar x : mkInputs args asns
mkInputs (LayoutArg {} : args) (asn : asns) = PikaCore.InputAsn asn : mkInputs args asns

-- | Use the PatternVars from the pattern list to give names to the base
-- typed arguments in the OpenedArgs
branchOpenedArgs :: [Pattern PikaCore.Expr] -> [OpenedArg' (LayoutBody PikaCore.Expr) ()] -> [OpenedArgBody]
branchOpenedArgs = zipWith go
  where
    go (PatternVar v) (BaseArg () x) = BaseArg v x
    go _ (LayoutArg x) = LayoutArg x

convertBranch :: forall m. Monad m => [OpenedArg] -> Pika.FnDefBranch -> PikaConvert m PikaCore.FnDefBranch
convertBranch openedArgLayouts (Pika.FnDefBranch matches0) = do

  let argMatches = map (map _layoutMatch) $ getArgLayouts openedArgLayouts

  openedArgBranches <- mapM openPatternMatch $ concat argMatches
  openedLayoutBodies0 <- map (_ghostCondBody . snd) <$>
    (mapM (freshOpen . snd) openedArgBranches
      :: PikaConvert m [([Name PikaCore.Expr], GhostCondition PikaCore.Expr (LayoutBody PikaCore.Expr))])

  matches1 <- mapM convertPattern $ patternMatchesPats matches0

  openedLayoutBodies <- map (first _ghostCondBody) <$> zipWithM (overArgLayout . flip applyLayoutPatternM) matches1 openedArgLayouts

  let openedArgs = branchOpenedArgs matches1 openedLayoutBodies

  matches <- convertPatternMatches openedArgs (Pika.unguardMatches matches0)

  (_, Pika.GuardedExpr cond bodyExpr) <- openPatternMatches matches0
  condExpr <- convertExpr openedArgs cond
  bodyExpr' <- convertExpr openedArgs bodyExpr

  -- inAsns <- zipWithM getAssertion openedArgLayouts $ patternMatchesPats matches
  layouts <- asks _layoutEnv
  let inAsns = map getPointsTos $ getArgLayouts openedLayoutBodies
      allocs = layoutLAppliesMaxAllocs layouts $ getArgLayouts openedLayoutBodies

  pure $ PikaCore.FnDefBranch
    { PikaCore._fnDefBranchInputAssertions = mkInputs openedArgLayouts inAsns
    , PikaCore._fnDefBranchInAllocs = allocs
    , PikaCore._fnDefBranchCondition = condExpr
    , PikaCore._fnDefBranchBody = bodyExpr'
    }

convertPatternMatches :: Monad m => [OpenedArgBody] -> PatternMatches Pika.Expr Pika.Expr -> PikaConvert m (PatternMatches PikaCore.Expr PikaCore.Expr)
convertPatternMatches argLayoutBodies =
  onPatternMatches internExprName (convertExpr argLayoutBodies)

getParameters :: OpenedArg' ([Moded PikaCore.ExprName], OpenedLayout PikaCore.Expr) () -> [ModedName PikaCore.Expr]
getParameters (BaseArg () v) = [Moded In v]
getParameters (LayoutArg (vars, _)) = vars

convertAppHereUsing :: (Monad m, HasCallStack) =>
  [OpenedArgBody] -> Pika.Expr ->
  [Moded PikaCore.ExprName] ->
  -- Layout PikaCore.Expr ->
  PikaConvert m PikaCore.Expr
convertAppHereUsing opened e params = do
  convertApp opened e (params, PikaCore.LayoutV (map PikaCore.V (map modedNameName params)))

convertAppHere :: (Monad m, HasCallStack) =>
  [OpenedArgBody] -> Pika.Expr ->
  PikaConvert m PikaCore.Expr
convertAppHere opened e@(Pika.ApplyLayout _ layoutName) = do
  layout <- lookupLayoutM (name2String layoutName)
  let params0 = getLayoutParams layout
  params <- mapM (fresh . modedNameName) params0
  let modedParams = zipWith Moded (map getMode params0) params
  convertAppHereUsing opened e modedParams
convertAppHere opened e@(Pika.App f args)
  | isConstructor f = error $ "convertAppHere: Found constructor that's not applied to a layout: " ++ ppr' e
  | otherwise = do
        -- TODO: Handle base types
      resultTy <- lookupFnResultType f
      case resultTy of
        _ | isBaseType resultTy -> do
          param <- fresh (string2Name "p")
          convertAppHereUsing opened e [Moded Out param]
        TyVar layoutName  -> do
          layout <- lookupLayoutM (name2String layoutName)
          let params0 = getLayoutParams layout
          params <- mapM (fresh . modedNameName) params0
          let modedParams = zipWith Moded (map getMode params0) params
          convertAppHereUsing opened e modedParams
convertAppHere opened e = convertExpr opened e

convertApps :: (Monad m, HasCallStack) =>
  [OpenedArgBody] -> Pika.Expr ->
  [([ModedName PikaCore.Expr], PikaCore.Expr)] ->
  PikaConvert m PikaCore.Expr
convertApps opened e [] = convertExpr opened e
convertApps opened e ((vs, z) : rest) = undefined
  -- convertApp opened e vs (

convertApp :: (Monad m, HasCallStack) =>
  [OpenedArgBody] -> Pika.Expr ->
  ([ModedName PikaCore.Expr], PikaCore.Expr) ->
  PikaConvert m PikaCore.Expr
convertApp openedArgLayouts (Pika.ApplyLayout app@(Pika.App f args) layoutName) (vs, z)
  -- | isConstructor f =
  --     lowerConstructor openedArgLayouts f args (name2String layoutName)
  -- | otherwise =
  | not (isConstructor f) =
      lowerApp openedArgLayouts (PikaCore.FnName f) args (vs, z)
convertApp openedArgLayouts e0@(Pika.App f args) (vs, z)
  | isConstructor f = error $ "convertApp: Found constructor that's not applied to a layout: " ++ ppr' e0
  | otherwise = do
        -- TODO: Handle base types
      -- TyVar layoutName <- lookupFnResultType f
      lowerApp openedArgLayouts (PikaCore.FnName f) args (vs, z)
convertApp openedArgLayouts e (vs, z) =
  PikaCore.WithIn
    <$> convertExpr openedArgLayouts e
    <*> pure (bind vs z)

lowerApp ::  Monad m =>
  [OpenedArgBody] -> PikaCore.FnName -> [Pika.Expr] ->
  ([ModedName PikaCore.Expr], PikaCore.Expr) ->
  PikaConvert m PikaCore.Expr
lowerApp openedArgLayouts (PikaCore.FnName f) args (vs, z) = do
  args' <- mapM (convertExpr openedArgLayouts) args
  fnDef <- lookupFnDef f
  layouts <- asks _origLayoutEnv
  let szs =
        case Pika.getResultAllocSizeInts layouts fnDef of
          [] -> [0] -- Result has a base type (Int or Bool)
          xs -> xs
  pure $
    PikaCore.WithIn
      (PikaCore.App (PikaCore.FnName f) szs args')
      (bind vs z)

lowerConstructor :: Monad m =>
  [OpenedArgBody] -> String -> [Pika.Expr] -> String ->
  -- ([ModedName PikaCore.Expr], PikaCore.Expr) ->
  PikaConvert m PikaCore.Expr
lowerConstructor openedArgLayouts f args layoutName
  | not (isConstructor f) = error $ "lowerConstructor: Requires constructor, got " ++ f
  | otherwise = do
      args' <- mapM (convertExpr openedArgLayouts) args

      layout <- lookupLayoutM layoutName
      applied <- applyLayout' layout f args'
      (params, existsBnd) <- unbind applied
      (_, GhostCondition _ (LayoutBody body)) <- freshOpenExists existsBnd
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
    go ((layoutName, _ghosts, exprArg, layoutVarExprs) : xs) z =
      convertLApply layoutName exprArg layoutVarExprs
        =<< go xs z

-- convertExpr :: forall m. (Monad m, HasCallStack) => [OpenedArg] -> Pika.Expr -> PikaConvert m PikaCore.Expr
convertExpr :: forall m. (Monad m, HasCallStack) => [OpenedArgBody] -> Pika.Expr -> PikaConvert m PikaCore.Expr
convertExpr openedArgLayouts = go
  where
    go :: Pika.Expr -> PikaConvert m PikaCore.Expr
    go (Pika.V x) = do
      x' <- internExprName x
      case lookupVar openedArgLayouts x' of
      -- case undefined of
        [v] -> pure $ PikaCore.V v
        vs -> pure $ PikaCore.LayoutV (map PikaCore.V vs)
    go e0@(Pika.LayoutLambda {}) = error $ "convertExpr: " ++ ppr' e0
    go (Pika.ApplyLayout (Pika.V v) layoutName) = PikaCore.V <$> internExprName v -- TODO: Is this correct?

    go e@(Pika.ApplyLayout (Pika.App f args) layoutName)
      | isConstructor f = lowerConstructor openedArgLayouts f args (name2String layoutName)
    go e@(Pika.ApplyLayout {}) = convertAppHere openedArgLayouts e
    go e@(Pika.App {}) = convertAppHere openedArgLayouts e
    go (Pika.Div x y) = PikaCore.Div <$> go x <*> go y
    go (Pika.Mod x y) = PikaCore.Mod <$> go x <*> go y
    go (Pika.And x y) = PikaCore.And <$> go x <*> go y
    go (Pika.Add x y) = PikaCore.Add <$> go x <*> go y
    go (Pika.Mul x y) = PikaCore.Mul <$> go x <*> go y
    go (Pika.Not x) = PikaCore.Not <$> go x
    go (Pika.Sub x y) = PikaCore.Sub <$> go x <*> go y
    go (Pika.Equal x y) = PikaCore.Equal <$> go x <*> go y
    go (Pika.Lt x y) = PikaCore.Lt <$> go x <*> go y
    go (Pika.IntLit i) = pure $ PikaCore.IntLit i
    go Pika.EmptySet = pure PikaCore.EmptySet
    go (Pika.SingletonSet x) = PikaCore.SingletonSet <$> go x
    go (Pika.SetUnion x y) = PikaCore.SetUnion <$> go x <*> go y
    go (Pika.BoolLit b) = pure $ PikaCore.BoolLit b

lookupVar :: HasCallStack => [OpenedArgBody] -> Name PikaCore.Expr -> [Name PikaCore.Expr]
lookupVar [] v = [v] --error $ "lookupVar: cannot find " ++ show v
lookupVar (BaseArg x y : rest) v
  | v == x = [y]
lookupVar (LayoutArg x : rest) v =
  case lookupVar1 (_unLayoutBody x) v of
    Nothing -> lookupVar rest v
    Just r -> r
lookupVar (_ : rest) v = lookupVar rest v

-- lookupVar Nothing v = [v]
-- lookupVar (Just openedArgLayouts) v =
--   case foldr (<|>) Nothing (map (`lookupVar1` v) (map _unLayoutBody openedArgLayouts)) of
--     Nothing -> error $ "lookupVar: Cannot find " ++ show v ++ " in " ++ show openedArgLayouts
--     Just r -> r

lookupVar1 :: [LayoutHeaplet PikaCore.Expr] -> Name PikaCore.Expr -> Maybe [Name PikaCore.Expr]
lookupVar1 [] v = Nothing
lookupVar1 (LPointsTo ((x :+ i) :-> PikaCore.V rhs) : rest) v
  | rhs == v = Just [v]
lookupVar1 (LApply layoutName _ghosts (PikaCore.V patVar) layoutParams : rest) v
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


convertLayout :: (Monad m, HasCallStack) => Layout Pika.Expr -> PikaConvert m (Layout PikaCore.Expr)
convertLayout layout0 = do
  (ghosts, bnd) <- unbind $ _layoutBranches layout0
  branches <- onBind (mapM internModedExprName) (mapM convertLayoutBranch) bnd
  -- params <- mapM internModedExprName $ _layoutSigParams $ _layoutSig layout0

  pure $
    Layout
    { _layoutName = _layoutName layout0
    , _layoutBranches = bind (map (mapGhostName (string2Name . name2String)) ghosts) branches
    , _layoutAdt = _layoutAdt layout0
    }

convertLayoutBranch :: forall m. (Monad m, HasCallStack) => LayoutBranch Pika.Expr -> PikaConvert m (LayoutBranch PikaCore.Expr)
convertLayoutBranch (LayoutBranch branch) = do
    LayoutBranch <$> onPatternMatch internExprName goExists branch
  where
    goExists ::
      Bind [Exists Pika.Expr] (GhostCondition Pika.Expr (LayoutBody Pika.Expr)) ->
      PikaConvert m (Bind [Exists PikaCore.Expr] (GhostCondition PikaCore.Expr (LayoutBody PikaCore.Expr)))
    goExists = onBind (mapM internExists)
                 go
                    --(fmap undefined . go . _unLayoutBody)

    go :: GhostCondition Pika.Expr (LayoutBody Pika.Expr) -> PikaConvert m (GhostCondition PikaCore.Expr (LayoutBody PikaCore.Expr))
    go (GhostCondition x (LayoutBody y)) = do
      x' <- traverse (convertExpr mempty) x
      GhostCondition x' . LayoutBody <$> traverse go' y

    go' :: LayoutHeaplet Pika.Expr -> PikaConvert m (LayoutHeaplet PikaCore.Expr)
    go' (LApply layoutName ghosts patVar vs) = do
      patVar' <- convertExpr mempty patVar
      vs' <- mapM internExprName (map getName vs)
      ghosts' <- mapM (convertExpr mempty) ghosts
      pure $ LApply layoutName ghosts' patVar' (map PikaCore.V vs')

    go' (LPointsTo ((Pika.V a :+ i) :-> b)) = do
      a' <- internExprName a
      b' <- convertExpr mempty b
      pure (LPointsTo ((PikaCore.V a' :+ i) :-> b'))

convertPattern :: Monad m => Pattern Pika.Expr -> PikaConvert m (Pattern PikaCore.Expr)
convertPattern (PatternVar v) =
  PatternVar <$> internExprName v
convertPattern (Pattern constructor vars) =
  Pattern constructor <$> mapM internExprName vars

lookupTypeLayout :: Fresh m => [Layout PikaCore.Expr] -> Type -> m OpenedArgLayout
lookupTypeLayout layouts (TyVar n) = pure . LayoutArg $ lookupLayout layouts (name2String n)
lookupTypeLayout _ IntType = BaseArg () <$> fresh (string2Name "i")
lookupTypeLayout _ BoolType = BaseArg () <$> fresh (string2Name "b")

