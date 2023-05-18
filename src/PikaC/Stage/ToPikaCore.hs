{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wincomplete-patterns #-}

module PikaC.Stage.ToPikaCore
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

import PikaC.Utils

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
toPikaCore layouts0 fn = runFreshM $ do
  layouts <- runPikaIntern' $ mapM convertLayout layouts0

  outParams <- generateParams layouts resultType

  newBranches <- mapM (branchToPikaCore layouts outParams argTypes) (Pika.fnDefBranches fn)

  let inParams = map (locBase . pointsToLhs) $ concat $ concatMap PikaCore.fnDefBranchInputAssertions newBranches
  let params = fastNub inParams ++ outParams

  pure $
    PikaCore.FnDef
      { PikaCore.fnDefName = Pika.fnDefName fn
      , PikaCore.fnDefParams = params
      , PikaCore.fnDefBranches = newBranches
      }
  where
    (argTypes, resultType) = splitFnType (Pika.fnDefType fn)

branchToPikaCore :: [Layout PikaCore.Expr] -> [PikaCore.ExprName] -> [Type] -> Pika.FnDefBranch -> FreshM PikaCore.FnDefBranch
branchToPikaCore layouts outParams argTypes branch = runPikaIntern' $ do
  -- params <- concat <$> mapM (generateParams layouts) (argTypes ++ [resultType])
  pats <- mapM convertPattern (Pika.fnBranchPats branch)
  let tyPats = zip argTypes pats

  -- (vsubst, layoutBodies) <- mkVSubsts layouts tyPats


  runPikaConvert'' mempty layouts $ do
    let exprAsns = -- TODO: Handle base types
          map getPointsTos $
          zipWith3 applyLayout'
            layouts
            (map _patConstructor pats)
            (map (map PikaCore.V . _patVars) pats)
    -- let exprAsns = map truncatedLayoutBody $ concatMap (map _layoutBody . _layoutBranches) layouts


    newBranchBody <- convertExpr (Pika.fnBranchBody branch)

    pure $
      PikaCore.FnDefBranch
        { PikaCore.fnDefOutputParams = outParams
        , PikaCore.fnDefBranchInputAssertions = exprAsns
        , PikaCore.fnDefBranchBody = newBranchBody
        }

-- | Remove all LApply's. NOTE: Does not substitute using LApply's
truncatedLayoutBody :: LayoutBody a -> [PointsTo a]
truncatedLayoutBody (LayoutBody xs0) = go xs0
  where
    go [] = []
    go (LApply {} : xs) = go xs
    go (LPointsTo p : xs) = p : go xs

convertPattern :: MonadPikaIntern m => Pattern Pika.Expr -> m (Pattern PikaCore.Expr)
convertPattern (PatternVar v) = PatternVar <$> internExprName v
convertPattern pat@(Pattern {}) = do
  vars <- mapM internExprName $ _patVars pat
  pure $
    Pattern
    { _patConstructor = _patConstructor pat
    , _patVars = vars
    }

convertBase :: (MonadPikaIntern m, HasCallStack) => Pika.Expr -> m PikaCore.Expr
convertBase (Pika.V n) = PikaCore.V <$> internExprName n
-- convertBase (Pika.LocV n) = pure $ PikaCore.LocV (LocVar n)
convertBase (Pika.Add x y) = PikaCore.Add <$> convertBase x <*> convertBase y
convertBase (Pika.Sub x y) = PikaCore.Sub <$> convertBase x <*> convertBase y
convertBase (Pika.Equal x y) = PikaCore.Equal <$> convertBase x <*> convertBase y
convertBase (Pika.IntLit i) = pure $ PikaCore.IntLit i
convertBase (Pika.BoolLit b) = pure $ PikaCore.BoolLit b
convertBase e = error $ "convertBase: " ++ show e

convertLayout :: forall m. MonadPikaIntern m => Layout Pika.Expr -> m (Layout PikaCore.Expr)
convertLayout layout0 = scoped $ do
  branches <- mapM convertLayoutBranch $ _layoutBranches layout0
  params <- mapM internExprName $ _layoutParams layout0

  pure $
    Layout
    { _layoutName = _layoutName layout0
    , _layoutBranches = branches
    , _layoutParams = params
    }

convertLayoutBranch :: forall m. MonadPikaIntern m => LayoutBranch Pika.Expr -> m (LayoutBranch PikaCore.Expr)
convertLayoutBranch branch0 = do
    body <- (unLayoutBody %%~ go) (_layoutBody branch0)
    pat <- convertPattern $ _layoutPattern branch0

    pure $ LayoutBranch
      { _layoutPattern = pat
      , _layoutBody = body
      }
  where
    go :: [LayoutHeaplet Pika.Expr] -> m [LayoutHeaplet PikaCore.Expr]
    go = traverse go'

    go' :: LayoutHeaplet Pika.Expr -> m (LayoutHeaplet PikaCore.Expr)
    go' (LApply layoutName patVar vs) = do
      patVar' <- convertBase patVar
      vs' <- mapM internExprName vs
      pure $ LApply layoutName patVar' vs'

    go' (LPointsTo ((a :+ i) :-> b)) = do
      a' <- internExprName a
      b' <- convertBase b
      pure (LPointsTo ((a' :+ i) :-> b'))

generateParams :: Fresh m => [Layout PikaCore.Expr] -> Type -> m [PikaCore.ExprName]
generateParams layouts IntType = (:[]) <$> fresh (string2Name "i")
generateParams layouts BoolType = (:[]) <$> fresh (string2Name "b")
generateParams layouts t@(FnType {}) = error $ "generateParams: " ++ show t
generateParams layouts (TyVar layoutName) = do
  let layout = lookupLayout layouts (name2String layoutName)
  mapM fresh $ _layoutParams layout

convertExpr :: Pika.Expr -> PikaConvert PikaCore.Expr
convertExpr = go . Pika.reduceLayouts
  where
    go :: Pika.Expr -> PikaConvert PikaCore.Expr
    -- go (Pika.LocV x) = pure . base $ PikaCore.LocV $ LocVar x
    go e0@(Pika.LName x) = error $ "convertExpr.go: " ++ show e0
    go (Pika.V n) = PikaCore.V <$> internExprName n
    -- go e0@(Pika.LayoutTypeArg {}) =
    --   error $ "convertExpr: " ++ show e0
    go (Pika.IntLit i) = pure $ PikaCore.IntLit i
    go (Pika.BoolLit b) = pure $ PikaCore.BoolLit b
    go e0@(Pika.LayoutLambda {}) =
      error $ "convertExpr: " ++ show e0
    -- go e0@(Pika.Lower e (LayoutNameB layout)) =
      -- error $ "convertExpr: Free layout variable in: " ++ show e0
    go e0@(Pika.ApplyLayout (Pika.V x) layoutName) = do
      let layoutString = name2String (unTypeVar layoutName)
      (n, y) <- vsubstLookupM x
      if n == layoutString
        then pure $ PikaCore.LayoutV y
        else error $ "convertExpr: layout " ++ layoutString ++ " does not match " ++ n ++ " in " ++ show e0

    go e0@(Pika.ApplyLayout (Pika.App f xs) layoutName) = scoped $ do
      rs <- generateParamsM (TyVar (unTypeVar layoutName))

      layout <- lookupLayoutM (name2String (unTypeVar layoutName))


      app <- applyLayoutOrNOP layout f xs

      -- trace ("args = " ++ show xs ++ "; app = " ++ show app) $
      pure $
        PikaCore.WithIn
          app
          rs
           (PikaCore.LayoutV rs)
    go e0@(Pika.ApplyLayout {}) = error $ "convertExpr.go: " ++ show e0
    go e0@(Pika.App f xs) = do
      -- traceM $ "Translating apply: " ++ show e0
      PikaCore.App f <$> mapM go xs
      -- traceM $ "Got: " ++ show r
    -- go e0@(Pika.App {}) = error $ "convertExpr.go: " ++ show e0 -- TODO: Is this right?
    go (Pika.Add x y) =
      PikaCore.Add <$> convertBase x <*> convertBase y
    go (Pika.Sub x y) =
      PikaCore.Sub <$> convertBase x <*> convertBase y
    go (Pika.Equal x y) =
      PikaCore.Equal <$> convertBase x <*> convertBase y
    -- go (Pika.Not x) =
    --   fmap base (PikaCore.Not <$> go x)
applyLayoutOrNOP :: Layout PikaCore.Expr -> String -> [Pika.Expr] -> PikaConvert PikaCore.Expr
applyLayoutOrNOP layout constructor args = do
  args' <- mapM convertExpr args
  fmap (fromMaybe (PikaCore.App constructor args')) $ do
    let params = _layoutParams layout
    -- layout' <- convertLayout layout
    let xs = applyLayout layout constructor args'

    case applyLayout layout constructor args' of
      Nothing -> pure Nothing
      Just asns -> do
        -- asns' <- convertLayoutBody asns

        -- trace ("args = " ++ show args ++ "; args' = " ++ show args' ++ "asns = " ++ show asns) .
        -- pure . pure . PikaCore.SimpleExpr $ PikaCore.SslAssertion params (getPointsTos asns)
        Just <$> freshSslAssertion params (getPointsTos asns)

getLApplies :: LayoutBody a -> [(String, a, [Name a])]
getLApplies (LayoutBody x) =
  toListOf (traversed._LApply) x

getSingletonLApplies :: [(String, a, [Name a])] -> [(Name a, (a, String))]
getSingletonLApplies =
  mapMaybe go
  where
    go (x, y, [z]) = Just (z, (y, x))
    go _ = Nothing

substWithLApplies :: [(String, PikaCore.Expr, [PikaCore.ExprName])] -> [PointsTo PikaCore.Expr] -> [PointsTo PikaCore.Expr]
substWithLApplies = go . getSingletonLApplies
  where
    go :: [(PikaCore.ExprName, (PikaCore.Expr, String))] -> [PointsTo PikaCore.Expr] -> [PointsTo PikaCore.Expr]
    go [] p = p
    go e0@((x, (e, z)):xs) p = subst x e (go xs p)
      -- error $ "here " ++ show e0 ++ "; " ++ show x ++ "-- " ++ show (subst x e p)
      -- error $ "here " ++ show e0 ++ "; " ++ show n ++ "-- " ++ show (subst n e p)
      -- subst (string2Name (name2String (unLocVar x))) y (go xs p)
    -- go ((x, (PikaCore.SimpleExpr (PikaCore.BaseExpr (PikaCore.LocV y)), z)):xs) p = error "there"
      -- subst (unLocVar x) y (go xs p)
    -- go (_:xs) p = go xs p

getPointsTos :: LayoutBody PikaCore.Expr -> [PointsTo PikaCore.Expr]
getPointsTos b@(LayoutBody xs0) = substWithLApplies (getLApplies b) $ go xs0
  where
    go [] = []
    go (LApply {} : xs) = go xs
    go (LPointsTo p : xs) = p : go xs

generateParamsM :: Type -> PikaConvert [PikaCore.ExprName]
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
              , LApply "Sll" (Pika.V (s2n "tail")) [s2n "nxt"]
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
              (`Pika.ApplyLayout` TypeVar (s2n "Sll")) $
                Pika.App "Nil" []
          }

        ,Pika.FnDefBranch
          { Pika.fnBranchPats = [Pattern "Cons" [s2n "h", s2n "t"]]
          , Pika.fnBranchBody =
              (`Pika.ApplyLayout` TypeVar (s2n "Sll")) $
                Pika.App "Cons"
                  [Pika.Add (Pika.V (s2n "h")) (Pika.IntLit 1)
                  ,Pika.V (s2n "t")
                  ]
          }
        ]
    }

exampleMapAdd1 :: Pika.FnDef
exampleMapAdd1 =
  Pika.FnDef
    { Pika.fnDefName = "mapAdd1"
    , Pika.fnDefType = FnType (TyVar (s2n "Sll")) (TyVar (s2n "Sll"))
    , Pika.fnDefBranches =
        [Pika.FnDefBranch
          { Pika.fnBranchPats = [Pattern "Nil" []]
          , Pika.fnBranchBody =
              (`Pika.ApplyLayout` TypeVar (s2n "Sll")) $
                Pika.App "Nil" []
          }

        ,Pika.FnDefBranch
          { Pika.fnBranchPats = [Pattern "Cons" [s2n "h", s2n "t"]]
          , Pika.fnBranchBody =
              (`Pika.ApplyLayout` TypeVar (s2n "Sll")) $
                Pika.App "Cons"
                  [Pika.Add (Pika.V (s2n "h")) (Pika.IntLit 1)
                  ,Pika.App "mapAdd1" [Pika.V (s2n "t")]
                  ]
          }
        ]
    }

--
-- -- | Get location variables associated to expression variable, based on the
-- -- LApply's in a LayoutBranch
-- findLApplies :: LayoutBody PikaCore.Expr -> PikaCore.ExprName -> Maybe (String, [PikaCore.ExprName])
-- findLApplies body n =
--     go' (unLayoutBody body)
--   where
--     go' [] = Nothing
--     go' (x:xs) = go x <|> go' xs
--
--     go (LPointsTo {}) = Nothing
--     go (LApply layout (PikaCore.SimpleExpr (PikaCore.BaseExpr (PikaCore.V n'))) vars)
--       | n' == n = Just (layout, vars)
--     go _ = Nothing
--
-- lookupVSubst :: ExprName -> LayoutVarSubst -> Maybe (String, LayoutArg PikaCore.Expr)
-- lookupVSubst e (LayoutVarSubst s) = lookup e s
--
-- mkVSubsts :: MonadPikaIntern m => [Layout PikaCore.Expr] -> [(Type, Pattern Pika.Expr)] -> m (LayoutVarSubst, [LayoutBody PikaCore.Expr])
-- mkVSubsts layouts tyPats =
--   fmap (go . unzip) (traverse (uncurry (mkVSubst layouts)) tyPats)
--   where
--     -- go :: ([LayoutVarSubst], [LayoutBody Pika.Expr]) -> (LayoutVarSubst, [LayoutBody Pika.Expr])
--     go = _1 %~ mconcat
--
-- mkVSubst :: forall m. MonadPikaIntern m => [Layout PikaCore.Expr] -> Type -> Pattern Pika.Expr -> m (LayoutVarSubst, LayoutBody PikaCore.Expr)
-- mkVSubst _ IntType pat = do
--   let [v] = _patVars pat
--   v' <- internExprName v
--   pure (LayoutVarSubst [( v, ("Int", [v']))], mempty)
--
-- mkVSubst _ BoolType pat = do
--   let [v] = _patVars pat
--   v' <- internExprName v
--   pure (LayoutVarSubst [(v, ("Bool", [v']))], mempty)
--
-- mkVSubst layouts (TyVar n) pat = do
--   let layoutString = name2String n
--       layout = lookupLayout layouts layoutString
--       -- branch = lookupLayoutBranch' layout (_patConstructor pat)
--       vars = _patVars pat
--
--   vars' <- mapM internExprName vars
--
--   let body = applyLayout' layout (_patConstructor pat) (map (PikaCore.base . PikaCore.V) vars')
--
--       go :: PikaCore.ExprName -> m ([Char], [PikaCore.ExprName])
--       go v = do
--         pure $ fromMaybe (layoutString, [v]) (findLApplies body v)
--
--   locNameLists <- mapM go vars'
--
--   -- locs <- (traversed._2.traversed %%~ internExprName) locNameLists
--   let locs = locNameLists
--   pure (LayoutVarSubst $ zip vars locs, body) --_layoutBody branch)
--
-- mkVSubst layouts t@(FnType {}) _ = error $ "mkVSubst: " ++ show t
--
-- -- expr2LocName :: ExprName -> LocName
-- -- expr2LocName = string2Name . name2String
--
-- -- generatePatternAsn :: Type -> Pattern Pika.Expr -> PikaConvert PikaCore.ExprAssertion
-- -- generatePatternAsn IntType pat = pure []
-- -- generatePatternAsn BoolType pat = pure []
-- -- generatePatternAsn (FnType {}) _ = error "generatePatternAsn: function type"
-- -- generatePatternAsn (TyVar n) pat = do
-- --   layout <- lookupLayoutM (name2String n)
-- --   let LayoutBody z = applyLayout' layout (_patConstructor pat) (map Pika.V (_patVars pat))
-- --   concat <$> mapM toExprHeaplets z
--
-- -- toExprAsn :: LayoutBody PikaCore.Expr -> PikaConvert PikaCore.ExprAssertion
-- -- toExprAsn (LayoutBody xs) = pure xs
-- --   -- concat <$> mapM toExprHeaplets xs
--
-- -- -- | Precondition the expressions in 'LApply's should be fully reduced
-- -- toExprHeaplets :: LayoutHeaplet Pika.Expr -> PikaConvert [PikaCore.PointsToExpr]
-- -- toExprHeaplets (LPointsTo x) = do
-- --     z <- mapM (fmap toBase . convertExpr) x
-- --     pure [fmap PikaCore.base z]
-- --     -- pure $ (:[]) $ fmap (toBase . convertExpr vsubst layouts) x
-- -- toExprHeaplets (LApply layout (Pika.V _) params) = pure []
-- -- toExprHeaplets e0@(LApply layout (Pika.App f xs) params) =
-- --   isConstructor f >>= \case
-- --     False -> error $ "toExprHeaplet: Got application that is not a constructor application" ++ show e0
-- --     True ->
-- --       generatePatternAsn (TyVar (string2Name layout)) 
-- --         Pattern
-- --           { _patConstructor = f
-- --           , _patVars = map toV xs
-- --           }
-- -- toExprHeaplets e0@(LApply {}) = error $ "toExprHeaplets: " ++ show e0
--
-- -- convertName :: Pika.ExprName -> PikaCore.ExprName
-- -- convertName = string2Name . name2String
--
-- toV :: HasCallStack => Pika.Expr -> Pika.ExprName
-- toV (Pika.V x) = x
-- toV e = error $ "toV: got " ++ show e
--
--
-- applyLayoutOrNOP :: Layout PikaCore.Expr -> String -> [Pika.Expr] -> PikaConvert PikaCore.Expr
-- applyLayoutOrNOP layout constructor args = do
--   args' <- mapM convertExpr args
--   fmap (fromMaybe (PikaCore.App constructor args')) $ do
--     let params = _layoutParams layout
--     -- layout' <- convertLayout layout
--     let xs = applyLayout layout constructor args'
--     
--     case applyLayout layout constructor args' of
--       Nothing -> pure Nothing
--       Just asns -> do
--         -- asns' <- convertLayoutBody asns
--
--         trace ("args = " ++ show args ++ "; args' = " ++ show args' ++ "asns = " ++ show asns) .
--           pure . pure . PikaCore.SimpleExpr $ PikaCore.SslAssertion params (getPointsTos asns)
--
-- getLApplies :: LayoutBody a -> [(String, a, [Name a])]
-- getLApplies (LayoutBody x) =
--   toListOf (traversed._LApply) x
--
-- getSingletonLApplies :: [(String, a, [Name a])] -> [(Name a, (a, String))]
-- getSingletonLApplies =
--   mapMaybe go
--   where
--     go (x, y, [z]) = Just (z, (y, x))
--     go _ = Nothing
--
-- substWithLApplies :: [(String, PikaCore.Expr, [PikaCore.ExprName])] -> [PointsTo PikaCore.Expr] -> [PointsTo PikaCore.Expr]
-- substWithLApplies = go . getSingletonLApplies
--   where
--     go :: [(PikaCore.ExprName, (PikaCore.Expr, String))] -> [PointsTo PikaCore.Expr] -> [PointsTo PikaCore.Expr]
--     go [] p = p
--     go e0@((x, (e, z)):xs) p =
--       error $ "here " ++ show e0 ++ "; " ++ show x ++ "-- " ++ show (subst x e p)
--       -- error $ "here " ++ show e0 ++ "; " ++ show n ++ "-- " ++ show (subst n e p)
--       -- subst (string2Name (name2String (unLocVar x))) y (go xs p)
--     -- go ((x, (PikaCore.SimpleExpr (PikaCore.BaseExpr (PikaCore.LocV y)), z)):xs) p = error "there"
--       -- subst (unLocVar x) y (go xs p)
--     go (_:xs) p = go xs p
--
-- -- TODO: This needs to actually substitute the LApply's in
-- getPointsTos :: LayoutBody PikaCore.Expr -> [PointsTo PikaCore.Expr]
-- getPointsTos b@(LayoutBody xs0) = substWithLApplies (getLApplies b) $ go xs0
--   where
--     go [] = []
--     go (LApply {} : xs) = go xs
--     go (LPointsTo p : xs) = p : go xs
--
-- convertLayout :: MonadPikaIntern m => Layout Pika.Expr -> m (Layout PikaCore.Expr)
-- convertLayout layout = do
--   params <- mapM internExprName $ _layoutParams layout
--   branches <- mapM convertLayoutBranch (_layoutBranches layout)
--   pure $
--     Layout
--       { _layoutName = _layoutName layout
--       , _layoutParams = params
--       , _layoutBranches = branches
--       }
--
-- convertLayoutBranch :: MonadPikaIntern m => LayoutBranch Pika.Expr -> m (LayoutBranch PikaCore.Expr)
-- convertLayoutBranch branch = do
--   pat <- convertPattern (_layoutPattern branch)
--   body <- convertLayoutBody (_layoutBody branch)
--   pure $
--     LayoutBranch
--       { _layoutPattern = pat
--       , _layoutBody = body
--       }
--
-- convertLayoutBody :: forall m. MonadPikaIntern m => LayoutBody Pika.Expr -> m (LayoutBody PikaCore.Expr)
-- convertLayoutBody =
--   fmap LayoutBody . go . unLayoutBody --toExprAsn
--   where
--     go :: [LayoutHeaplet Expr] -> m [LayoutHeaplet PikaCore.Expr]
--     go = traverse (traverse (fmap PikaCore.base . convertBase))
--
-- convertPattern :: MonadPikaIntern m => Pattern Pika.Expr -> m (Pattern PikaCore.Expr)
-- convertPattern (PatternVar n) = PatternVar <$> internExprName n
-- convertPattern (Pattern constructor vars) = do
--   vars' <- mapM internExprName vars
--   pure $ Pattern constructor vars'
--
-- -- convertLayoutHeaplet :: LayoutVarSubst -> [Layout Pika.Expr] -> LayoutHeaplet Pika.Expr -> FreshM [LayoutHeaplet PikaCore.Expr]
-- -- convertLayoutHeaplet vsubst layouts =
-- --   toExprAsn 
-- convertBase :: (MonadPikaIntern m, HasCallStack) => Pika.Expr -> m PikaCore.Base
-- convertBase (Pika.V n) = PikaCore.V <$> internExprName n
-- -- convertBase (Pika.LocV n) = pure $ PikaCore.LocV (LocVar n)
-- convertBase (Pika.Add x y) = PikaCore.Add <$> convertBase x <*> convertBase y
-- convertBase (Pika.Sub x y) = PikaCore.Sub <$> convertBase x <*> convertBase y
-- convertBase (Pika.Equal x y) = PikaCore.Equal <$> convertBase x <*> convertBase y
-- convertBase (Pika.IntLit i) = pure $ PikaCore.IntLit i
-- convertBase (Pika.BoolLit b) = pure $ PikaCore.BoolLit b
-- convertBase e = error $ "convertBase: " ++ show e
--
-- convertExpr :: Pika.Expr -> PikaConvert PikaCore.Expr
-- convertExpr = go . reduceLayouts
--   where
--
--     go :: Pika.Expr -> PikaConvert PikaCore.Expr
--     -- go (Pika.LocV x) = pure . base $ PikaCore.LocV $ LocVar x
--     go e0@(Pika.LName x) = error $ "convertExpr.go: " ++ show e0
--     go (Pika.V n) = base . PikaCore.V <$> internExprName n
--     -- go e0@(Pika.LayoutTypeArg {}) =
--     --   error $ "convertExpr: " ++ show e0
--     go (Pika.IntLit i) = pure . base $ PikaCore.IntLit i
--     go (Pika.BoolLit b) = pure . base $ PikaCore.BoolLit b
--     go e0@(Pika.LayoutLambda {}) =
--       error $ "convertExpr: " ++ show e0
--     -- go e0@(Pika.Lower e (LayoutNameB layout)) =
--       -- error $ "convertExpr: Free layout variable in: " ++ show e0
--     go e0@(Pika.ApplyLayout (Pika.V x) layoutName) = do
--       let layoutString = name2String (unTypeVar layoutName)
--       (n, y) <- vsubstLookupM x
--       if n == layoutString
--         then pure . base $ PikaCore.LayoutV (map undefined y)
--         else error $ "convertExpr: layout " ++ layoutString ++ " does not match " ++ n ++ " in " ++ show e0
--
--     go e0@(Pika.ApplyLayout (Pika.App f xs) layoutName) = do
--       rs <- generateParamsM (TyVar (unTypeVar layoutName))
--
--       layout <- lookupLayoutM (name2String (unTypeVar layoutName))
--
--
--       app <- applyLayoutOrNOP layout f xs
--
--       -- trace ("args = " ++ show xs ++ "; app = " ++ show app) $
--       pure $
--         simple $ PikaCore.WithIn
--           app
--           $ bind rs
--               (PikaCore.BaseExpr (PikaCore.LayoutV (map undefined rs)))
--     go e0@(Pika.ApplyLayout {}) = error $ "convertExpr.go: " ++ show e0
--     go e0@(Pika.App f xs) = do
--       -- traceM $ "Translating apply: " ++ show e0
--       r <- PikaCore.App f <$> mapM go xs
--       -- traceM $ "Got: " ++ show r
--       pure r
--     -- go e0@(Pika.App {}) = error $ "convertExpr.go: " ++ show e0 -- TODO: Is this right?
--     go (Pika.Add x y) =
--       fmap base (PikaCore.Add <$> convertBase x <*> convertBase y)
--     go (Pika.Sub x y) =
--       fmap base (PikaCore.Sub <$> convertBase x <*> convertBase y)
--     go (Pika.Equal x y) =
--       fmap base (PikaCore.Equal <$> convertBase x <*> convertBase y)
--     -- go (Pika.Not x) =
--     --   fmap base (PikaCore.Not <$> go x)
--
-- isConstructor :: String -> PikaConvert Bool
-- isConstructor c = do
--   layouts <- asks _layoutEnv
--   let constructors =
--         layouts ^.. (traversed.layoutBranches.traversed.layoutPattern.patConstructor)
--   pure $ c `elem` constructors
--
-- generatePatternParams :: Fresh m => [Layout Pika.Expr] -> Type -> Pattern Pika.Expr -> m [PikaCore.ExprName]
-- generatePatternParams layouts IntType pat = generateParams layouts IntType
-- generatePatternParams layouts BoolType pat = generateParams layouts BoolType
-- generatePatternParams layouts (TyVar n) pat = do
--   let layout = lookupLayout layouts (name2String n)
--   xs <- mapM fresh $ _layoutParams layout
--   trace ("pattern params: " ++ show xs) $ pure xs
-- generatePatternParams layouts t@(FnType {}) pat = error $ "generatePatternParams: " ++ show t 
--
-- generateParams :: Fresh m => [Layout Pika.Expr] -> Type -> m [PikaCore.ExprName]
-- generateParams layouts IntType = (:[]) <$> fresh (string2Name "i")
-- generateParams layouts BoolType = (:[]) <$> fresh (string2Name "b")
-- generateParams layouts t@(FnType {}) = error $ "generateParams: " ++ show t
-- generateParams layouts (TyVar layoutName) = do
--   let layout = lookupLayout layouts (name2String layoutName)
--   mapM fresh $ _layoutParams layout
--
-- generatePatternParamsM :: Type -> Pattern Pika.Expr -> PikaConvert [PikaCore.ExprName]
-- generatePatternParamsM ty pat = do
--   layouts <- asks _layoutEnv
--   generatePatternParams layouts ty pat
--
-- generateParamsM :: Type -> PikaConvert [PikaCore.ExprName]
-- generateParamsM ty = do
--   layouts <- asks _layoutEnv
--   generateParams layouts ty
--
--
--
