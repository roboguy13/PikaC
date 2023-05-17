{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wincomplete-patterns #-}

module PikaC.Stage.ToPikaCore
  where

import qualified PikaC.Syntax.Pika.Expr as Pika
import qualified PikaC.Syntax.Pika.FnDef as Pika

import qualified PikaC.Syntax.PikaCore.Expr as PikaCore
import PikaC.Syntax.PikaCore.Expr (base, simple)
import qualified PikaC.Syntax.PikaCore.FnDef as PikaCore

import PikaC.Syntax.Pika.Layout
import PikaC.Syntax.Type
import PikaC.Syntax.Heaplet
import PikaC.Syntax.Pika.Pattern
import PikaC.Syntax.Pika.Expr
import PikaC.Ppr

import PikaC.Stage.ToPikaCore.Monad

import Unbound.Generics.LocallyNameless

import GHC.Stack

import Data.Void
import Data.List
import Data.Maybe

import Control.Lens hiding (simple)

import Control.Applicative
import Control.Arrow (second)

import Control.Monad.Reader

import Debug.Trace

toPikaCore :: [Layout Pika.Expr] -> Pika.FnDef -> PikaCore.FnDef
toPikaCore layouts fn = runFreshM $ do
  outParams <- generateParams layouts resultType
  inParams <- concat <$> mapM (generateParams layouts) argTypes
  let params = inParams ++ outParams

  newBranches <- mapM (branchToPikaCore layouts outParams argTypes) (Pika.fnDefBranches fn)

  pure $
    PikaCore.FnDef
      { PikaCore.fnDefName = Pika.fnDefName fn
      , PikaCore.fnDefParams = params
      , PikaCore.fnDefBranches = newBranches
      }
  where
    (argTypes, resultType) = splitFnType (Pika.fnDefType fn)

branchToPikaCore :: [Layout Pika.Expr] -> [LocName] -> [Type] -> Pika.FnDefBranch -> FreshM PikaCore.FnDefBranch
branchToPikaCore layouts outParams argTypes branch = runPikaIntern' $ do
  -- params <- concat <$> mapM (generateParams layouts) (argTypes ++ [resultType])
  let tyPats = zip argTypes (Pika.fnBranchPats branch)

  (vsubst, layoutBodies) <- mkVSubsts layouts tyPats

  runPikaConvert'' vsubst layouts $ do
    -- traceM ("layoutBodies = " ++ show layoutBodies)
    -- traceM ("vsubst = " ++ show vsubst)
    exprAsns <- mapM toExprAsn layoutBodies

    newBranchBody <- convertExpr (Pika.fnBranchBody branch)

    pure $
      PikaCore.FnDefBranch
        { PikaCore.fnDefOutputParams = outParams
        , PikaCore.fnDefBranchInputAssertions = exprAsns
        , PikaCore.fnDefBranchBody = newBranchBody
        }

-- | Get location variables associated to expression variable, based on the
-- LApply's in a LayoutBranch
findLApplies :: LayoutBody Pika.Expr -> ExprName -> Maybe (String, [LocName])
findLApplies body n =
    second (map unLocVar) <$> go' (unLayoutBody body)
  where
    go' [] = Nothing
    go' (x:xs) = go x <|> go' xs

    go (LPointsTo {}) = Nothing
    go (LApply layout (Pika.V n') vars)
      | n' == n = Just (layout, vars)
    go _ = Nothing

lookupVSubst :: ExprName -> LayoutVarSubst -> Maybe (String, LayoutArg)
lookupVSubst e (LayoutVarSubst s) = lookup e s

mkVSubsts :: MonadPikaIntern m => [Layout Pika.Expr] -> [(Type, Pattern Pika.Expr)] -> m (LayoutVarSubst, [LayoutBody Pika.Expr])
mkVSubsts layouts tyPats =
  fmap (go . unzip) (traverse (uncurry (mkVSubst layouts)) tyPats)
  where
    go :: ([LayoutVarSubst], [LayoutBody Pika.Expr]) -> (LayoutVarSubst, [LayoutBody Pika.Expr])
    go = _1 %~ mconcat

mkVSubst :: MonadPikaIntern m => [Layout Pika.Expr] -> Type -> Pattern Pika.Expr -> m (LayoutVarSubst, LayoutBody Pika.Expr)
mkVSubst _ IntType pat = do
  let [v] = _patVars pat
  v' <- internLocName v
  pure (LayoutVarSubst [(v, ("Int", [v']))], mempty)

mkVSubst _ BoolType pat = do
  let [v] = _patVars pat
  v' <- internLocName v
  pure (LayoutVarSubst [(v, ("Bool", [v']))], mempty)

mkVSubst layouts (TyVar n) pat = do
  let layoutString = name2String n
      layout = lookupLayout layouts layoutString
      -- branch = lookupLayoutBranch' layout (_patConstructor pat)
      vars = _patVars pat
  let body = applyLayout' layout (_patConstructor pat) (map Pika.V vars)
      go v = do
        v' <- internLocName v
        pure $ fromMaybe (layoutString, [v']) (findLApplies body v)

  locNameLists <- mapM go vars

  locs <- (traversed._2.traversed %%~ fresh) locNameLists
  pure (LayoutVarSubst $ zip vars locs, body) --_layoutBody branch)

mkVSubst layouts t@(FnType {}) _ = error $ "mkVSubst: " ++ show t

-- expr2LocName :: ExprName -> LocName
-- expr2LocName = string2Name . name2String

generatePatternAsn :: Type -> Pattern Pika.Expr -> PikaConvert PikaCore.ExprAssertion
generatePatternAsn IntType pat = pure []
generatePatternAsn BoolType pat = pure []
generatePatternAsn (FnType {}) _ = error "generatePatternAsn: function type"
generatePatternAsn (TyVar n) pat = do
  layout <- lookupLayoutM (name2String n)
  let LayoutBody z = applyLayout' layout (_patConstructor pat) (map Pika.V (_patVars pat))
  concat <$> mapM toExprHeaplets z

toExprAsn :: LayoutBody Pika.Expr -> PikaConvert PikaCore.ExprAssertion
toExprAsn (LayoutBody xs) =
  concat <$> mapM toExprHeaplets xs

-- | Precondition the expressions in 'LApply's should be fully reduced
toExprHeaplets :: LayoutHeaplet Pika.Expr -> PikaConvert [PikaCore.PointsToExpr]
toExprHeaplets (LPointsTo x) = do
    z <- mapM (fmap toBase . convertExpr) x
    pure [z]
    -- pure $ (:[]) $ fmap (toBase . convertExpr vsubst layouts) x
toExprHeaplets (LApply layout (Pika.V _) params) = pure []
toExprHeaplets e0@(LApply layout (Pika.App f xs) params) =
  isConstructor f >>= \case
    False -> error $ "toExprHeaplet: Got application that is not a constructor application" ++ show e0
    True ->
      generatePatternAsn (TyVar (string2Name layout)) 
        Pattern
          { _patConstructor = f
          , _patVars = map toV xs
          }
toExprHeaplets e0@(LApply {}) = error $ "toExprHeaplets: " ++ show e0

-- convertName :: Pika.ExprName -> PikaCore.ExprName
-- convertName = string2Name . name2String

toV :: HasCallStack => Pika.Expr -> Pika.ExprName
toV (Pika.V x) = x
toV e = error $ "toV: got " ++ show e

toBase :: HasCallStack => PikaCore.Expr -> PikaCore.Base
toBase (PikaCore.SimpleExpr (PikaCore.BaseExpr e)) = e
toBase e = error $ "toBase: " ++ ppr' e

applyLayoutOrNOP :: Layout Pika.Expr -> String -> [Pika.Expr] -> PikaConvert PikaCore.Expr
applyLayoutOrNOP layout constructor args = do
  args' <- mapM convertExpr args
  fmap (fromMaybe (PikaCore.App constructor args')) $ do
    let params = _layoutParams layout
    
    case applyLayout layout constructor args of
      Nothing -> pure Nothing
      Just asns -> do
        asns' <- mapM convertExpr asns

        pure . pure . PikaCore.SimpleExpr $ PikaCore.SslAssertion params (map (fmap toBase) (getPointsTos asns'))

getPointsTos :: LayoutBody a -> [PointsTo a]
getPointsTos (LayoutBody xs0) = go xs0
  where
    go [] = []
    go (LApply {} : xs) = go xs
    go (LPointsTo p : xs) = p : go xs

-- convertLayout :: Layout Pika.Expr -> PikaConvert (Layout PikaCore.Base)
-- convertLayout layout = do
--   branches <- mapM convertLayoutBranch (_layoutBranches layout)
--   pure $
--     Layout
--       { _layoutName = _layoutName layout
--       , _layoutParams = _layoutParams layout
--       , _layoutBranches = branches
--       }

convertLayoutBranch :: LayoutBranch Pika.Expr -> PikaConvert (LayoutBranch PikaCore.Base)
convertLayoutBranch branch = do
  body <- convertLayoutBody (_layoutBody branch)
  pat <- convertPattern (_layoutPattern branch)
  pure $
    LayoutBranch
      { _layoutPattern = pat
      , _layoutBody = body
      }

convertLayoutBody :: LayoutBody Pika.Expr -> PikaConvert (LayoutBody PikaCore.Base)
convertLayoutBody =
  fmap (LayoutBody . map LPointsTo) . toExprAsn

convertPattern :: Pattern Pika.Expr -> PikaConvert (Pattern PikaCore.Base)
convertPattern (PatternVar n) = PatternVar <$> internExprName n
convertPattern (Pattern constructor vars) = do
  vars' <- mapM internExprName vars
  pure $ Pattern constructor vars'

-- convertLayoutHeaplet :: LayoutVarSubst -> [Layout Pika.Expr] -> LayoutHeaplet Pika.Expr -> FreshM [LayoutHeaplet PikaCore.Expr]
-- convertLayoutHeaplet vsubst layouts =
--   toExprAsn 

convertExpr :: Pika.Expr -> PikaConvert PikaCore.Expr
convertExpr = go . reduceLayouts
  where
    goBase :: Pika.Expr -> PikaConvert PikaCore.Base
    goBase (Pika.V n) = PikaCore.V <$> internExprName n
    goBase (Pika.Add x y) = PikaCore.Add <$> goBase x <*> goBase y
    goBase (Pika.Sub x y) = PikaCore.Sub <$> goBase x <*> goBase y
    goBase (Pika.Equal x y) = PikaCore.Equal <$> goBase x <*> goBase y
    goBase (Pika.IntLit i) = pure $ PikaCore.IntLit i
    goBase (Pika.BoolLit b) = pure $ PikaCore.BoolLit b
    goBase e = error $ "convertExpr.goBase: " ++ show e

    go :: Pika.Expr -> PikaConvert PikaCore.Expr
    go (Pika.LocV x) = pure . base $ PikaCore.LocV $ LocVar x
    go e0@(Pika.LName x) = error $ "convertExpr.go: " ++ show e0
    go (Pika.V n) = base . PikaCore.V <$> internExprName n
    -- go e0@(Pika.LayoutTypeArg {}) =
    --   error $ "convertExpr: " ++ show e0
    go (Pika.IntLit i) = pure . base $ PikaCore.IntLit i
    go (Pika.BoolLit b) = pure . base $ PikaCore.BoolLit b
    go e0@(Pika.LayoutLambda {}) =
      error $ "convertExpr: " ++ show e0
    -- go e0@(Pika.Lower e (LayoutNameB layout)) =
      -- error $ "convertExpr: Free layout variable in: " ++ show e0
    go e0@(Pika.ApplyLayout (Pika.V x) layoutName) = do
      let layoutString = name2String (unTypeVar layoutName)
      (n, y) <- vsubstLookupM x
      if n == layoutString
        then pure . base $ PikaCore.LayoutV (map LocVar y)
        else error $ "convertExpr: layout " ++ layoutString ++ " does not match " ++ n ++ " in " ++ show e0

    go e0@(Pika.ApplyLayout (Pika.App f xs) layoutName) = do
      rs <- generateParamsM (TyVar (unTypeVar layoutName))

      layout <- lookupLayoutM (name2String (unTypeVar layoutName))

      app <- applyLayoutOrNOP layout f xs

      pure $
        simple $ PikaCore.WithIn
          app
          $ bind rs
              (PikaCore.BaseExpr (PikaCore.LayoutV (map LocVar rs)))
    go e0@(Pika.ApplyLayout {}) = error $ "convertExpr.go: " ++ show e0
    go e0@(Pika.App {}) = error $ "convertExpr.go: " ++ show e0 -- TODO: Is this right?
    go (Pika.Add x y) =
      fmap base (PikaCore.Add <$> goBase x <*> goBase y)
    go (Pika.Sub x y) =
      fmap base (PikaCore.Sub <$> goBase x <*> goBase y)
    go (Pika.Equal x y) =
      fmap base (PikaCore.Equal <$> goBase x <*> goBase y)
    -- go (Pika.Not x) =
    --   fmap base (PikaCore.Not <$> go x)

isConstructor :: String -> PikaConvert Bool
isConstructor c = do
  layouts <- asks _layoutEnv
  let constructors =
        layouts ^.. (traversed.layoutBranches.traversed.layoutPattern.patConstructor)
  pure $ c `elem` constructors

generatePatternParams :: Fresh m => [Layout Pika.Expr] -> Type -> Pattern Pika.Expr -> m [LocName]
generatePatternParams layouts IntType pat = generateParams layouts IntType
generatePatternParams layouts BoolType pat = generateParams layouts BoolType
-- generatePatternParams layouts (TyVar v) pat = generateParams layouts (TyVar v)
generatePatternParams layouts (TyVar n) pat = do
  let layout = lookupLayout layouts (name2String n)
      -- z = applyLayout layout (_patConstructor pat) (map Pika.V (_patVars pat))
  xs <- mapM fresh $ _layoutParams layout
  trace ("pattern params: " ++ show xs) $ pure xs
generatePatternParams layouts t@(FnType {}) pat = error $ "generatePatternParams: " ++ show t 

generateParams :: Fresh m => [Layout Pika.Expr] -> Type -> m [LocName]
generateParams layouts IntType = (:[]) <$> fresh (string2Name "i")
generateParams layouts BoolType = (:[]) <$> fresh (string2Name "b")
generateParams layouts t@(FnType {}) = error $ "generateParams: " ++ show t
-- generateParams _ (TyVar v) = error $ "generateParams: Still has layout type variable " ++ show v
generateParams layouts (TyVar layoutName) = do
  let layout = lookupLayout layouts (name2String layoutName)
  xs <- mapM fresh $ _layoutParams layout
  pure xs
  -- trace ("generated params: " ++ show xs ++ "; from " ++ show (_layoutParams layout)) $ pure xs

generatePatternParamsM :: Type -> Pattern Pika.Expr -> PikaConvert [LocName]
generatePatternParamsM ty pat = do
  layouts <- asks _layoutEnv
  generatePatternParams layouts ty pat

generateParamsM :: Type -> PikaConvert [LocName]
generateParamsM ty = do
  layouts <- asks _layoutEnv
  generateParams layouts ty

exampleSll :: Layout Pika.Expr
exampleSll =
  Layout
  { _layoutName = "Sll"
  , _layoutParams = [string2Name "x"]
  , _layoutBranches =
      [LayoutBranch
        { _layoutPattern = Pattern "Nil" []
        , _layoutBody =
            LayoutBody
              []
        }
      ,LayoutBranch
        { _layoutPattern = Pattern "Cons" [s2n "head", s2n "tail"]
        , _layoutBody =
            LayoutBody
              [ LPointsTo ((s2n "x" :+ 0) :-> Pika.V (s2n "head"))
              , LPointsTo ((s2n "x" :+ 1) :-> Pika.V (s2n "nxt"))
              , LApply "Sll" (Pika.V (s2n "tail")) [LocVar (s2n "nxt")]
              ]
        }
      ]
  }

exampleAdd1Head :: Pika.FnDef
exampleAdd1Head =
  Pika.FnDef
    { Pika.fnDefName = "add1Head"
    , Pika.fnDefType = FnType (TyVar (s2n "Sll")) (TyVar (s2n "Sll"))
    , Pika.fnDefBranches =
        [Pika.FnDefBranch
          { Pika.fnBranchPats = [Pattern "Nil" []]
          , Pika.fnBranchBody =
              (`ApplyLayout` TypeVar (s2n "Sll")) $
                Pika.App "Nil" []
          }

        ,Pika.FnDefBranch
          { Pika.fnBranchPats = [Pattern "Cons" [s2n "h", s2n "t"]]
          , Pika.fnBranchBody =
              (`ApplyLayout` TypeVar (s2n "Sll")) $
                Pika.App "Cons"
                  [Pika.Add (Pika.V (s2n "h")) (Pika.IntLit 1)
                  ,Pika.V (s2n "t")
                  ]
          }
        ]
    }

