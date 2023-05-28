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
import PikaC.Stage.ToPikaCore.Simplify

import PikaC.Utils

import Unbound.Generics.LocallyNameless

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
toPikaCore :: [Layout Pika.Expr] -> Pika.FnDef -> PikaCore.FnDef
toPikaCore layouts0 fn = runFreshM . runPikaIntern' $ do
  let (argTypes, resultType) = splitFnType (_typeSigTy (Pika.fnDefTypeSig fn))

  layouts <- mapM (runPikaConvert'' mempty . convertLayout) layouts0
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
    runPikaConvert'' layouts $ mapM (convertBranch openedArgLayouts) $ Pika.fnDefBranches fn

  pure $ PikaCore.FnDef
    { PikaCore._fnDefName = Pika.fnDefName fn
    , PikaCore._fnDefBranches =
        bind (concat inParams)
          (bind outParams branches')
    }

convertBranch :: [Maybe ([ModedName PikaCore.Expr], OpenedLayout PikaCore.Expr)] -> Pika.FnDefBranch -> PikaConvert PikaCore.FnDefBranch
convertBranch openedArgLayouts (Pika.FnDefBranch matches0) = do

  let argMatches = map (map _layoutMatch) . map snd $ catMaybes openedArgLayouts

  openedArgBranches <- mapM openPatternMatch $ concat argMatches
  openedLayoutBodies <- map snd <$>
    (mapM (freshOpen . snd) openedArgBranches
      :: PikaConvert [([Name PikaCore.Expr], LayoutBody PikaCore.Expr)])


  matches <- convertPatternMatches openedLayoutBodies matches0
  (_, bodyExpr) <- openPatternMatches matches

  -- inAsns <- zipWithM getAssertion openedArgLayouts $ patternMatchesPats matches
  let inAsns = map getPointsTos openedLayoutBodies

  pure $ PikaCore.FnDefBranch
    { PikaCore._fnDefBranchInputAssertions = inAsns
    , PikaCore._fnDefBranchBody = bodyExpr
    }

-- chooseExistVars :: OpenedLayout a -> [P

convertPatternMatches :: [LayoutBody PikaCore.Expr] -> PatternMatches Pika.Expr Pika.Expr -> PikaConvert (PatternMatches PikaCore.Expr PikaCore.Expr)
convertPatternMatches argLayoutBodies =
  onPatternMatches internExprName (convertExpr argLayoutBodies)

-- getAssertion :: forall m. Fresh m => Maybe ([Moded PikaCore.ExprName], OpenedLayout PikaCore.Expr) -> Pattern PikaCore.Expr -> m PikaCore.ExprAssertion
-- getAssertion _ (PatternVar {}) = error "getAssertion: Pattern variables not yet supported" -- TODO: Handle this by "expanding" the pattern variable matches into all the corresponding constructor matches, using the ADT definition, before we reach this
-- getAssertion Nothing pat = error "getAssertion Nothing"  -- TODO: Handle this case
-- getAssertion (Just (vars, opened)) (Pattern constructor xs) = do
--   let match = lookupPatternMatch' (map _layoutMatch opened) constructor
--       bndExists = applyPatternMatch' match constructor (map PikaCore.V xs)
--   (_, body) <- freshOpen bndExists :: m ([Name PikaCore.Expr], LayoutBody PikaCore.Expr)
--   pure $ getPointsTos body

getParameters :: Fresh m => Maybe ([Moded PikaCore.ExprName], OpenedLayout PikaCore.Expr) -> m [ModedName PikaCore.Expr]
getParameters Nothing = ((:[]) . Moded In) <$> fresh (string2Name "ww")
getParameters (Just (vars, _)) = pure vars

convertExpr :: [LayoutBody PikaCore.Expr] -> Pika.Expr -> PikaConvert PikaCore.Expr
convertExpr openedArgLayouts = go
  where
    go :: Pika.Expr -> PikaConvert PikaCore.Expr
    go (Pika.V x) = do
      x' <- internExprName x
      case lookupVar openedArgLayouts x' of
        [v] -> pure $ PikaCore.V v
        vs -> pure $ PikaCore.LayoutV vs
    go e0@(Pika.LayoutLambda {}) = error $ "convertExpr: " ++ ppr' e0
    go (Pika.ApplyLayout (Pika.V v) layoutName) = PikaCore.V <$> internExprName v -- TODO: Is this correct?

    go (Pika.ApplyLayout app@(Pika.App f xs) layoutName)
      | isConstructor f = do
          layout <- lookupLayoutM (name2String layoutName)

          app' <- go app
          (vars, branch) <- unbindLayout layout
          rs <- mapM freshModed vars

          pure $
            PikaCore.WithIn
              app'
              (bind rs
                (PikaCore.LayoutV (map modedNameName rs)))

          -- rs <- freshLayoutParams layout
          --
          -- -- let layoutParams = _layoutSigParams (_layoutSig layout)
          -- --     rs = layoutParams
          --
          -- -- rs <- mapM fresh (replicate (length layoutParams) (string2Name "cc"))
          --
          -- xs' <- mapM go xs
          --
          -- let applied = applyLayout' layout (map modedExpr rs) f xs'
          --     -- bnd = undefined
          --     bnd = PikaCore.SslAssertion (bind rs (getPointsTos applied))
          -- -- bnd <- freshSslAssertion layoutParams (getPointsTos applied)
          --
          -- pure $
          --   PikaCore.WithIn
          --     bnd
          --     (bind rs
          --       (PikaCore.LayoutV (map modedNameName rs)))

      | otherwise = do
          layout <- lookupLayoutM (name2String layoutName)
          (vars, branch) <- unbindLayout layout
          rs <- mapM freshModed vars
          app' <- go app
          pure $
            PikaCore.WithIn
              app'
              (bind rs
                (PikaCore.LayoutV (map modedNameName rs)))
          -- rs <- generateParamsM (TyVar layoutName)
          -- app <- applyLayoutOrNOP layout rs f xs
          --
          -- pure $
          --   PikaCore.WithIn
          --     app
          --     (bind rs
          --        (PikaCore.LayoutV (map modedNameName rs)))

    go (Pika.App f xs) = PikaCore.App f <$> mapM go xs
    go e0@(Pika.ApplyLayout {}) = error $ "convertExpr: " ++ ppr' e0
    go (Pika.Add x y) = PikaCore.Add <$> go x <*> go y
    go (Pika.IntLit i) = pure $ PikaCore.IntLit i


lookupVar :: [LayoutBody PikaCore.Expr] -> Name PikaCore.Expr -> [Name PikaCore.Expr]
lookupVar openedArgLayouts v =
  case foldr (<|>) Nothing (map (`lookupVar1` v) (map _unLayoutBody openedArgLayouts)) of
    Nothing -> error $ "lookupVar: Cannot find " ++ show v
    Just r -> r

lookupVar1 :: [LayoutHeaplet PikaCore.Expr] -> Name PikaCore.Expr -> Maybe [Name PikaCore.Expr]
lookupVar1 [] v = Nothing
lookupVar1 (LPointsTo ((x :+ i) :-> PikaCore.V rhs) : rest) v
  | rhs == v = Just [v]
lookupVar1 (LApply layoutName (PikaCore.V patVar) layoutParams : rest) v
  | patVar == v = Just layoutParams
lookupVar1 (_ : rest) v = lookupVar1 rest v

--   let branches = map (convertFnBranch layouts argLayouts argTypes outParams) (Pika.fnDefBranches fn)
--
--   -- let inParams = fastNub . map (locBase . pointsToLhs) $ concat $ concatMap PikaCore._fnDefBranchInputAssertions freshenedBranches
--   let inParams = undefined
--   let params = inParams ++ outParams
--
--   -- simplifyFnDef $
--   pure $
--     PikaCore.FnDef
--       { PikaCore._fnDefName = Pika.fnDefName fn
--       -- , PikaCore._fnDefParams = params
--       , PikaCore._fnDefBranches = branches
--       }
--

-- convertBind :: (HasCallStack, Subst Pika.Expr b) =>
--   (a -> PikaConvert b) ->
--   Bind [ModedName Pika.Expr] a ->
--   PikaConvert (Bind [ModedName PikaCore.Expr] b)
-- convertBind act (B vs body) = do
--   let es = map (Pika.V . modedNameName) vs
--
--   vs' <- map getV <$> mapM convertExpr es
--
--   body' <- substs (zip (map modedNameName vs) es) <$> act body
--   pure (B (zipWith Moded (map getMode vs) vs') body')
--
-- convertBindList :: (HasCallStack, Subst Pika.Expr b) =>
--   (a -> PikaConvert b) ->
--   Bind [ModedName Pika.Expr] [a] ->
--   PikaConvert (Bind [ModedName PikaCore.Expr] [b])
-- convertBindList act = convertBind (mapM act)

getV :: HasCallStack => PikaCore.Expr -> Name PikaCore.Expr
getV (PikaCore.V x) = x
getV e = error $ "getV: " ++ ppr' e

onBind :: (Alpha a, Alpha a', Alpha b, Alpha b', Fresh m) => (a -> m a') -> (b -> m b') -> Bind a b -> m (Bind a' b')
onBind varFn bodyFn bnd = do
  (vars, body) <- unbind bnd
  bind <$> varFn vars <*> bodyFn body

convertLayout :: Layout Pika.Expr -> PikaConvert (Layout PikaCore.Expr)
convertLayout layout0 = do
  branches <- onBind (mapM internModedExprName) (mapM convertLayoutBranch) $ _layoutBranches layout0
  -- params <- mapM internModedExprName $ _layoutSigParams $ _layoutSig layout0

  pure $
    Layout
    { _layoutName = _layoutName layout0
    , _layoutBranches = branches
    , _layoutAdt = _layoutAdt layout0
    }

convertLayoutBranch :: LayoutBranch Pika.Expr -> PikaConvert (LayoutBranch PikaCore.Expr)
convertLayoutBranch (LayoutBranch branch) = do
    LayoutBranch <$> onPatternMatch internExprName goExists branch
  where
    goExists ::
      Bind [Exists Pika.Expr] (LayoutBody Pika.Expr) ->
      PikaConvert (Bind [Exists PikaCore.Expr] (LayoutBody PikaCore.Expr))
    goExists = onBind (mapM internExists) (fmap LayoutBody . go . _unLayoutBody)

    go :: [LayoutHeaplet Pika.Expr] -> PikaConvert [LayoutHeaplet PikaCore.Expr]
    go = traverse go'

    go' :: LayoutHeaplet Pika.Expr -> PikaConvert (LayoutHeaplet PikaCore.Expr)
    go' (LApply layoutName patVar vs) = do
      patVar' <- convertExpr mempty patVar
      vs' <- mapM internExprName vs
      pure $ LApply layoutName patVar' vs'

    go' (LPointsTo ((a :+ i) :-> b)) = do
      a' <- internExprName a
      b' <- convertExpr mempty b
      pure (LPointsTo ((a' :+ i) :-> b'))

-- convertExpr (Pika.V x) = PikaCore.V <$> internExprName x
-- convertExpr (Pika.IntLit i) = pure (PikaCore.IntLit i)
-- convertExpr (Pika.BoolLit b) = pure (PikaCore.BoolLit b)
-- convertExpr e0@(Pika.LayoutLambda {}) = error $ "convertExpr: " ++ ppr' e0
-- convertExpr (Pika.ApplyLayout (Pika.V v) layoutName) = PikaCore.V <$> internExprName v -- TODO: Is this correct?
--
-- convertExpr (Pika.ApplyLayout app@(Pika.App f xs) layoutName)
--   | isConstructor f = do
--       layout <- lookupLayoutM (name2String layoutName)
--
--       rs <- freshLayoutParams layout
--
--       -- let layoutParams = _layoutSigParams (_layoutSig layout)
--       --     rs = layoutParams
--
--       -- rs <- mapM fresh (replicate (length layoutParams) (string2Name "cc"))
--
--       xs' <- mapM convertExpr xs
--
--       let applied = applyLayout' layout (map modedExpr rs) f xs'
--           -- bnd = undefined
--           bnd = PikaCore.SslAssertion (bind rs (getPointsTos applied))
--       -- bnd <- freshSslAssertion layoutParams (getPointsTos applied)
--
--       pure $
--         PikaCore.WithIn
--           bnd
--           (bind rs
--             (PikaCore.LayoutV (map modedNameName rs)))
--
--   | otherwise = do
--       rs <- generateParamsM (TyVar layoutName)
--       layout <- lookupLayoutM (name2String layoutName)
--       app <- applyLayoutOrNOP layout rs f xs
--
--       pure $
--         PikaCore.WithIn
--           app
--           (bind rs
--              (PikaCore.LayoutV (map modedNameName rs)))
--
-- convertExpr (Pika.App f xs) = PikaCore.App f <$> mapM convertExpr xs
-- convertExpr e0@(Pika.ApplyLayout {}) = error $ "convertExpr: " ++ ppr' e0
-- convertExpr (Pika.Add x y) =
--   PikaCore.Add <$> convertExpr x <*> convertExpr y
-- convertExpr (Pika.Sub x y) =
--   PikaCore.Sub <$> convertExpr x <*> convertExpr y
-- convertExpr (Pika.Equal x y) =
--   PikaCore.Equal <$> convertExpr x <*> convertExpr y
--
-- convertExpr e0@(Pika.LName {}) = error $ "convertExpr: " ++ ppr' e0
--
convertPattern :: Pattern Pika.Expr -> PikaConvert (Pattern PikaCore.Expr)
convertPattern (PatternVar v) =
  PatternVar <$> internExprName v
convertPattern (Pattern constructor vars) =
  Pattern constructor <$> mapM internExprName vars

isConstructor :: String -> Bool
isConstructor = isUpper . head

--
--
-- --   layouts <- mapM (runPikaConvert'' mempty . convertLayout) layouts0
-- --   outParams <- generateParams layouts resultType
-- --
-- --   let argLayouts = map (lookupTypeLayout layouts) argTypes
-- --   rhos <- mapM (traverse layoutParamRename) argLayouts
-- --
-- --   let branches = map (convertFnBranch layouts argLayouts rhos argTypes outParams) (Pika.fnDefBranches fn)
-- --
-- --   -- let inParams = fastNub . map (locBase . pointsToLhs) $ concat $ concatMap PikaCore._fnDefBranchInputAssertions branches
-- --   let inParams :: [PikaCore.ExprName]
-- --       inParams =
-- --         concatMap (\case
-- --                      Nothing -> error "TODO: Implement non-constructor application pattern variables"
-- --                      Just x -> _layoutSigParams (_layoutSig x))
-- --         $ zipWith renameMaybe rhos argLayouts
-- --
-- --   -- let freshenedBranches = trace ("rhos = " ++ show rhos) (map (freshenBranchAsn rhos) branches)
-- --
-- --   simplifyFnDef
-- --   -- pure
-- --     $ PikaCore.FnDef
-- --       { PikaCore._fnDefBranches = branches
-- --       , PikaCore._fnDefName = Pika.fnDefName fn
-- --       , PikaCore._fnDefParams = inParams ++ outParams
-- --       }
-- --   where
-- --     (argTypes, resultType) = splitFnType (_typeSigTy (Pika.fnDefTypeSig fn))
-- --
-- convertFnBranch :: [Layout PikaCore.Expr] -> [Maybe (Layout PikaCore.Expr)] -> [Type] -> [ModedName PikaCore.Expr] -> Pika.FnDefBranch -> PikaCore.FnDefBranch
-- convertFnBranch layouts argLayouts argTypes outParams branch = runPikaConvert layouts $ do
--   pats <- mapM convertPattern $ Pika.fnBranchPats branch
--
--   -- let argLayouts = map (lookupTypeLayout layouts) argTypes
--   -- let paramMap = makeParameterMap (zipWith renameMaybe rhos argLayouts) pats
--
--   let exprAsns :: [PikaCore.ExprAssertion]
--       exprAsns =
--         map getPointsTos $
--         zipWith (getArgAsn layouts) argTypes pats
--
--   body <- convertExpr (Pika.fnBranchBody branch)
--   -- body <- convertBase (Pika.fnBranchBody branch)
--
--   pure
--     $ PikaCore.FnDefBranch
--       -- { PikaCore._fnDefOutputParams = outParams
--       { PikaCore._fnDefBranchBody = body
--       , PikaCore._fnDefBranchInputAssertions = exprAsns
--       }
-- --
-- getArgAsn :: [Layout PikaCore.Expr] -> Type -> Pattern PikaCore.Expr -> LayoutBody PikaCore.Expr
-- getArgAsn layouts ty (PatternVar {}) = error "getArgAsn: PatternVar {} case unimplemented"
-- getArgAsn layouts (TyVar layoutName) (Pattern constructor vars) =
--   let layout = lookupLayout layouts (name2String layoutName)
--   in
--   applyLayout' layout constructor (map PikaCore.V vars)
-- getArgAsn layouts ty pat@(Pattern {}) = error $ "getArgAsn: Cannot match type " ++ ppr' ty ++ " against pattern " ++ ppr' pat
-- --
-- -- -- renameMaybe (Just rho) = rename rho
-- -- -- renameMaybe Nothing = id
-- --
-- --
-- -- convertBase :: Pika.Expr -> PikaConvert PikaCore.Expr
-- -- convertBase = go
-- --   where
-- --     go :: Pika.Expr -> PikaConvert PikaCore.Expr
-- --     go (Pika.V v) = PikaCore.V <$> internExprName v
-- --     go (Pika.IntLit i) = pure $ PikaCore.IntLit i
-- --     go (Pika.LayoutLambda {}) = error $ "convertBase: There should be no layout lambdas when converting from Pika to PikaCore"
-- --     go (Pika.ApplyLayout (Pika.V v) layoutName) = PikaCore.V <$> internExprName v -- TODO: Is this correct?
-- --
-- --     go (Pika.ApplyLayout (Pika.App f xs) layoutName)
-- --       | isConstructor f = do
-- --           layout <- lookupLayoutM (name2String layoutName)
-- --           let layoutParams = _layoutSigParams (_layoutSig layout)
-- --           rs <- mapM fresh (replicate (length layoutParams) (string2Name "cc"))
-- --           xs' <- mapM convertBase xs
-- --           let applied = applyLayout' layout f xs'
-- --           bnd <- freshSslAssertion layoutParams (getPointsTos applied)
-- --           trace ("applyLayout args = " ++ show (xs, xs') ++ "; applied " ++ show applied) $
-- --             pure $
-- --               PikaCore.WithIn
-- --                 bnd
-- --                 rs
-- --                 (PikaCore.LayoutV rs)
-- --
-- --     go e0@(Pika.ApplyLayout (Pika.App f xs) layoutName) = do
-- --       rs <- generateParamsM (TyVar layoutName)
-- --
-- --       layout <- lookupLayoutM (name2String layoutName)
-- --
-- --       app <- applyLayoutOrNOP layout rs f xs
-- --
-- --       -- traceM $ "generating layout-applied app: " ++ show e0 ++ "\n  ^--> " ++ show app
-- --       pure $
-- --         PikaCore.WithIn
-- --           app
-- --           rs
-- --            (PikaCore.LayoutV rs)
-- --     go (Pika.App f xs) = PikaCore.App f <$> mapM go xs
-- --     go e0@(Pika.ApplyLayout {}) = error $ "convertBase: " ++ show e0
-- --     go (Pika.Add x y) =
-- --       PikaCore.Add <$> go x <*> go y
-- --     go (Pika.Sub x y) =
-- --       PikaCore.Sub <$> go x <*> convertBase y
-- --     go (Pika.Equal x y) =
-- --       PikaCore.Equal <$> go x <*> go y
-- -- --
-- -- --     go e0@(Pika.ApplyLayout (Pika.V x) layoutName) = do
-- -- --       let layoutString = name2String layoutName
-- -- --       (n, y) <- vsubstLookupM x
-- -- --       if n == layoutString
-- -- --         then pure $ PikaCore.LayoutV y
-- -- --         else error $ "convertExpr: layout " ++ layoutString ++ " does not match " ++ n ++ " in " ++ show e0
-- -- --
-- -- --     go e0@(Pika.ApplyLayout (Pika.App f xs) layoutName) = scoped $ do
-- -- --       rs <- generateParamsM (TyVar layoutName)
-- -- --
-- -- --       layout <- lookupLayoutM (name2String layoutName)
-- -- --
-- -- --       app <- applyLayoutOrNOP layout f xs
-- -- --
-- -- --       traceM $ "generating layout-applied app: " ++ show e0 ++ "\n  ^--> " ++ show app
-- -- --
-- -- --       pure $
-- -- --         PikaCore.WithIn
-- -- --           app
-- -- --           rs
-- -- --            (PikaCore.LayoutV rs)
-- --
lookupTypeLayout :: [Layout a] -> Type -> Maybe (Layout a)
lookupTypeLayout layouts (TyVar n) = Just $ lookupLayout layouts (name2String n)
lookupTypeLayout _ _ = Nothing
-- --
-- -- convertLayout :: Layout Pika.Expr -> PikaConvert (Layout PikaCore.Expr)
-- -- convertLayout layout0 = do
-- --   branches <- mapM convertLayoutBranch $ _layoutBranches layout0
-- --   params <- mapM internExprName $ _layoutSigParams $ _layoutSig layout0
-- --
-- --   pure $
-- --     Layout
-- --     { _layoutName = _layoutName layout0
-- --     , _layoutBranches = branches
-- --     , _layoutSig = (_layoutSig layout0) { _layoutSigParams = params }
-- --     }
-- --
-- -- convertLayoutBranch :: LayoutBranch Pika.Expr -> PikaConvert (LayoutBranch PikaCore.Expr)
-- -- convertLayoutBranch branch0 = do
-- --     body <- (unLayoutBody %%~ go) (_layoutBody branch0)
-- --     pat <- convertPattern $ _layoutPattern branch0
-- --
-- --     pure $ LayoutBranch
-- --       { _layoutPattern = pat
-- --       , _layoutBody = body
-- --       }
-- --   where
-- --     go :: [LayoutHeaplet Pika.Expr] -> PikaConvert [LayoutHeaplet PikaCore.Expr]
-- --     go = traverse go'
-- --
-- --     go' :: LayoutHeaplet Pika.Expr -> PikaConvert (LayoutHeaplet PikaCore.Expr)
-- --     go' (LApply layoutName patVar vs) = do
-- --       patVar' <- convertBase patVar
-- --       vs' <- mapM internExprName vs
-- --       pure $ LApply layoutName patVar' vs'
-- --
-- --     go' (LPointsTo ((a :+ i) :-> b)) = do
-- --       a' <- internExprName a
-- --       b' <- convertBase b
-- --       pure (LPointsTo ((a' :+ i) :-> b'))
-- --
-- -- convertPattern :: MonadPikaIntern m => Pattern Pika.Expr -> m (Pattern PikaCore.Expr)
-- -- convertPattern (PatternVar v) = PatternVar <$> internExprName v
-- -- convertPattern (Pattern constructor vars) = do
-- --   vars <- mapM internExprName vars
-- --   pure $ Pattern constructor vars
-- --
-- -- applyParameterMap :: ParameterMap PikaCore.ExprName -> PikaCore.Expr -> PikaCore.Expr
-- -- applyParameterMap [] e = e
-- -- applyParameterMap ((x, ys) : rest) e =
-- --   case ys of
-- --     [] -> error $ "applyParameterMap: empty list for name " ++ show x
-- --     [y] -> subst x (PikaCore.V y) (applyParameterMap rest e)
-- --     _ -> subst x (PikaCore.LayoutV ys) (applyParameterMap rest e)
-- --
-- -- type ParameterMap a = [(a, [a])]
-- --
-- -- makeParameterMap ::
-- --   [Maybe (Layout PikaCore.Expr)] ->
-- --   [Pattern PikaCore.Expr] ->
-- --   ParameterMap PikaCore.ExprName
-- -- makeParameterMap layouts = concat . zipWith makeParameterMapOne layouts
-- --
-- -- makeParameterMapOne ::
-- --   Maybe (Layout PikaCore.Expr) ->
-- --   Pattern PikaCore.Expr ->
-- --   ParameterMap PikaCore.ExprName
-- -- makeParamaterMapOne Nothing pat =
-- --   case getPatternNames pat of
-- --     [v] -> [(v, [v])]
-- --     vs -> map (\x -> (x, [x])) vs
-- -- makeParameterMapOne (Just layout) (PatternVar _) = error "makeParameterMapOne: unimplemented for pattern variables" -- TODO: Implement
-- -- makeParameterMapOne (Just layout) pat@(Pattern constructor vars) =
-- --   -- trace ("** " ++ show (pat, layoutHeaplets)) $
-- --   map go vars
-- --   where
-- --     layoutHeaplets :: [LayoutHeaplet PikaCore.Expr]
-- --     layoutHeaplets =
-- --       _unLayoutBody $ applyLayout' layout constructor (map PikaCore.V vars)
-- --       --layout ^. to (`lookupLayoutBranch'` constructor).layoutBody.unLayoutBody
-- --
-- --     go v = (v, lookupLApply layoutHeaplets v)
-- --
-- -- lookupLApply :: [LayoutHeaplet PikaCore.Expr] -> PikaCore.ExprName -> [PikaCore.ExprName]
-- -- lookupLApply [] v = error $ "lookupLApply: Cannot find " ++ show v
-- -- lookupLApply (LApply _ (PikaCore.V v') params : _) v
-- --   | v' == v = params
-- -- lookupLApply (LPointsTo (_ :-> PikaCore.V v') : xs) v
-- --   | v' == v = [v]
-- -- lookupLApply (_ : xs) v = lookupLApply xs v
-- --
-- -- -- toPikaCore :: [Layout Pika.Expr] -> Pika.FnDef -> PikaCore.FnDef
-- -- -- toPikaCore layouts0 fn = runFreshM . runPikaIntern' $ do
-- -- --   layouts <- mapM convertLayout layouts0
-- -- --   outParams <- generateParams layouts resultType
-- -- --
-- -- --   newBranches <- mapM (branchToPikaCore layouts outParams argTypes) (Pika.fnDefBranches fn)
-- -- --
-- -- --   let argLayouts = map (typeLayout layouts) argTypes
-- -- --   rhos <- mapM (traverse layoutParamRename) argLayouts
-- -- --
-- -- --   
-- -- --   let freshenedBranches = trace ("rhos = " ++ show rhos) (map (freshenBranchAsn rhos) newBranches)
-- -- --
-- -- --   let inParams = fastNub . map (locBase . pointsToLhs) $ concat $ concatMap PikaCore._fnDefBranchInputAssertions freshenedBranches
-- -- --   let params = inParams ++ outParams
-- -- --
-- -- --
-- -- --   -- simplifyFnDef $
-- -- --   pure $
-- -- --     PikaCore.FnDef
-- -- --       { PikaCore._fnDefName = Pika.fnDefName fn
-- -- --       , PikaCore._fnDefParams = params
-- -- --       , PikaCore._fnDefBranches = freshenedBranches
-- -- --       }
-- -- --   where
-- -- --     (argTypes, resultType) = splitFnType (_typeSigTy (Pika.fnDefTypeSig fn))
-- -- --
-- -- -- | One rename for the layout associated to each parameter
-- -- freshenBranchAsn :: [Maybe (Rename PikaCore.ExprName)] -> PikaCore.FnDefBranch -> PikaCore.FnDefBranch
-- -- freshenBranchAsn rhos branch =
-- --     let asns = PikaCore._fnDefBranchInputAssertions branch
-- --         asns' = zipWith go asns rhos
-- --     in
-- --     branch
-- --       { PikaCore._fnDefBranchInputAssertions = asns'
-- --       }
-- --   where
-- --     go :: PikaCore.ExprAssertion -> Maybe (Rename PikaCore.ExprName) -> PikaCore.ExprAssertion
-- --     go asn (Just rho) = rename rho asn
-- --     go asn Nothing = asn
-- -- --
-- -- -- -- freshenAsnLhs :: Fresh m => PikaCore.ExprAssertion -> m PikaCore.ExprAssertion
-- -- -- -- freshenAsnLhs [] = pure []
-- -- -- -- freshenAsnLhs ((x :+ i) :-> y : rest) = do
-- -- -- --   x' <- fresh x
-- -- -- --   fmap ((x' :+ i) :-> y :) (freshenAsnLhs rest)
-- -- --
-- -- -- branchToPikaCore :: [Layout PikaCore.Expr] -> [PikaCore.ExprName] -> [Type] -> Pika.FnDefBranch -> PikaIntern PikaCore.FnDefBranch
-- -- -- branchToPikaCore layouts outParams argTypes branch = do
-- -- --   pats <- mapM convertPattern (Pika.fnBranchPats branch)
-- -- --   let tyPats = zip argTypes pats
-- -- --
-- -- --   runPikaConvert'' mempty layouts $ do
-- -- --     let exprAsns =
-- -- --           map getPointsTos $
-- -- --           zipWith (handlePattern layouts) argTypes pats
-- -- --
-- -- --
-- -- --     traceM $ "handled argTypes, pats: " ++ show (zipWith (handlePattern layouts) argTypes pats)
-- -- --     newBranchBody <- convertExpr (Pika.fnBranchBody branch)
-- -- --
-- -- --     pure $
-- -- --       PikaCore.FnDefBranch
-- -- --         { PikaCore._fnDefOutputParams = outParams
-- -- --         , PikaCore._fnDefBranchInputAssertions = exprAsns
-- -- --         , PikaCore._fnDefBranchBody = newBranchBody
-- -- --         }
-- -- --
-- -- --  -- TODO: Handle base types
-- -- -- handlePattern :: [Layout PikaCore.Expr] -> Type -> Pattern PikaCore.Expr -> LayoutBody PikaCore.Expr
-- -- -- -- handlePattern IntType (PatternVar v) =
-- -- -- -- handlePattern BoolType (PatternVar v) =
-- -- -- handlePattern layouts ty (PatternVar v) =
-- -- --   error $ "handlePattern: pattern variable " ++ show v ++ ", for type " ++ show ty
-- -- -- handlePattern layouts (TyVar n) (Pattern c vars) =
-- -- --   let layout = lookupLayout layouts (name2String n)
-- -- --       applied = applyLayout' layout c (map PikaCore.V vars)
-- -- --   in
-- -- --   applied
-- -- --
-- -- -- handlePattern _ ty pat@(Pattern {}) =
-- -- --   error $ "handlePattern: Trying to match with pattern " ++ ppr' pat ++ " on type " ++ ppr' ty
-- -- --
-- -- -- typeLayout :: [Layout a] -> Type -> Maybe (Layout a)
-- -- -- typeLayout layouts (TyVar n) = Just $ lookupLayout layouts (name2String n)
-- -- -- typeLayout _ _ = Nothing
-- -- --
-- -- -- -- | Remove all LApply's. NOTE: Does not substitute using LApply's
-- -- -- truncatedLayoutBody :: LayoutBody a -> [PointsTo a]
-- -- -- truncatedLayoutBody (LayoutBody xs0) = go xs0
-- -- --   where
-- -- --     go [] = []
-- -- --     go (LApply {} : xs) = go xs
-- -- --     go (LPointsTo p : xs) = p : go xs
-- -- --
-- -- -- convertPattern :: MonadPikaIntern m => Pattern Pika.Expr -> m (Pattern PikaCore.Expr)
-- -- -- convertPattern (PatternVar v) = PatternVar <$> internExprName v
-- -- -- convertPattern (Pattern constructor vars) = do
-- -- --   vars <- mapM internExprName vars
-- -- --   pure $ Pattern constructor vars
-- -- --
-- -- -- convertBase :: (MonadPikaIntern m, HasCallStack) => Pika.Expr -> m PikaCore.Expr
-- -- -- convertBase (Pika.V n) = PikaCore.V <$> internExprName n
-- -- -- -- convertBase (Pika.LocV n) = pure $ PikaCore.LocV (LocVar n)
-- -- -- convertBase (Pika.Add x y) = PikaCore.Add <$> convertBase x <*> convertBase y
-- -- -- convertBase (Pika.Sub x y) = PikaCore.Sub <$> convertBase x <*> convertBase y
-- -- -- convertBase (Pika.Equal x y) = PikaCore.Equal <$> convertBase x <*> convertBase y
-- -- -- convertBase (Pika.IntLit i) = pure $ PikaCore.IntLit i
-- -- -- convertBase (Pika.BoolLit b) = pure $ PikaCore.BoolLit b
-- -- -- convertBase e = error $ "convertBase: " ++ show e
-- -- --
-- -- -- convertLayout :: forall m. MonadPikaIntern m => Layout Pika.Expr -> m (Layout PikaCore.Expr)
-- -- -- convertLayout layout0 = do
-- -- --   branches <- mapM convertLayoutBranch $ _layoutBranches layout0
-- -- --   params <- mapM internExprName $ _layoutSigParams $ _layoutSig layout0
-- -- --
-- -- --   pure $
-- -- --     Layout
-- -- --     { _layoutName = _layoutName layout0
-- -- --     , _layoutBranches = branches
-- -- --     -- , _layoutParams = params
-- -- --     , _layoutSig = (_layoutSig layout0) { _layoutSigParams = params }
-- -- --     }
-- -- --
-- -- -- convertLayoutBranch :: forall m. MonadPikaIntern m => LayoutBranch Pika.Expr -> m (LayoutBranch PikaCore.Expr)
-- -- -- convertLayoutBranch branch0 = do
-- -- --     body <- (unLayoutBody %%~ go) (_layoutBody branch0)
-- -- --     pat <- convertPattern $ _layoutPattern branch0
-- -- --
-- -- --     pure $ LayoutBranch
-- -- --       { _layoutPattern = pat
-- -- --       , _layoutBody = body
-- -- --       }
-- -- --   where
-- -- --     go :: [LayoutHeaplet Pika.Expr] -> m [LayoutHeaplet PikaCore.Expr]
-- -- --     go = traverse go'
-- -- --
-- -- --     go' :: LayoutHeaplet Pika.Expr -> m (LayoutHeaplet PikaCore.Expr)
-- -- --     go' (LApply layoutName patVar vs) = do
-- -- --       patVar' <- convertBase patVar
-- -- --       vs' <- mapM internExprName vs
-- -- --       pure $ LApply layoutName patVar' vs'
-- -- --
-- -- --     go' (LPointsTo ((a :+ i) :-> b)) = do
-- -- --       a' <- internExprName a
-- -- --       b' <- convertBase b
-- -- --       pure (LPointsTo ((a' :+ i) :-> b'))
-- -- --
-- generateParams :: Fresh m => [Layout PikaCore.Expr] -> Type -> m [ModedName PikaCore.Expr]
-- generateParams layouts IntType = (:[]) . Moded Out <$> fresh (string2Name "i")
-- generateParams layouts BoolType = (:[]) . Moded Out <$> fresh (string2Name "b")
-- generateParams layouts t@(FnType {}) = error $ "generateParams: " ++ show t
-- generateParams layouts (TyVar layoutName) = do
--   let layout = lookupLayout layouts (name2String layoutName)
--   pure $ _layoutSigParams $ _layoutSig layout
--   -- mapM fresh $ _layoutSigParams $ _layoutSig layout
-- -- --
-- -- -- convertExpr :: Pika.Expr -> PikaConvert PikaCore.Expr
-- -- -- convertExpr = go . Pika.reduceLayouts
-- -- --   where
-- -- --     go :: Pika.Expr -> PikaConvert PikaCore.Expr
-- -- --     go e0@(Pika.LName x) = error $ "convertExpr.go: " ++ show e0
-- -- --     go (Pika.V n) = PikaCore.V <$> internExprName n
-- -- --     go (Pika.IntLit i) = pure $ PikaCore.IntLit i
-- -- --     go (Pika.BoolLit b) = pure $ PikaCore.BoolLit b
-- -- --     go e0@(Pika.LayoutLambda {}) =
-- -- --       error $ "convertExpr: " ++ show e0
-- -- --     go e0@(Pika.ApplyLayout (Pika.V x) layoutName) = do
-- -- --       let layoutString = name2String layoutName
-- -- --       (n, y) <- vsubstLookupM x
-- -- --       if n == layoutString
-- -- --         then pure $ PikaCore.LayoutV y
-- -- --         else error $ "convertExpr: layout " ++ layoutString ++ " does not match " ++ n ++ " in " ++ show e0
-- -- --
-- -- --     go e0@(Pika.ApplyLayout (Pika.App f xs) layoutName) = scoped $ do
-- -- --       rs <- generateParamsM (TyVar layoutName)
-- -- --
-- -- --       layout <- lookupLayoutM (name2String layoutName)
-- -- --
-- -- --       app <- applyLayoutOrNOP layout f xs
-- -- --
-- -- --       traceM $ "generating layout-applied app: " ++ show e0 ++ "\n  ^--> " ++ show app
-- -- --
-- -- --       pure $
-- -- --         PikaCore.WithIn
-- -- --           app
-- -- --           rs
-- -- --            (PikaCore.LayoutV rs)
-- -- --     go e0@(Pika.ApplyLayout {}) = error $ "convertExpr.go: " ++ show e0
-- -- --     go e0@(Pika.App f xs) = do
-- -- --       traceM $ "App: " ++ show e0
-- -- --       PikaCore.App f <$> mapM go xs
-- -- --     go (Pika.Add x y) =
-- -- --       PikaCore.Add <$> convertBase x <*> convertBase y
-- -- --     go (Pika.Sub x y) =
-- -- --       PikaCore.Sub <$> convertBase x <*> convertBase y
-- -- --     go (Pika.Equal x y) =
-- -- --       PikaCore.Equal <$> convertBase x <*> convertBase y
-- -- --
--
-- applyLayoutOrNOP :: Layout PikaCore.Expr -> [ModedName PikaCore.Expr] -> String -> [Pika.Expr] -> PikaConvert PikaCore.Expr
-- applyLayoutOrNOP layout outVars constructor args = do
--   args' <- mapM convertExpr args
--   fmap (fromMaybe (PikaCore.App constructor args')) $ do
--     let params = _layoutSigParams $ _layoutSig layout
--     let xs = applyLayout layout constructor args'
--
--     case applyLayoutLApplies layout constructor args' of
--       Nothing -> do
--         pure Nothing
--       Just asns -> do
--         --           ++ "\n ^--> getPointsTos: " ++ show (getPointsTos asns)
--         fmap Just $ lappliesToWiths outVars (getLApplies asns) =<< undefined params (getPointsTos asns)
--         -- e <- Just <$> freshSslAssertion params (getPointsTos asns)
--
-- -- | Only apply App's in LApply's, not LPointsTo's. Apply everything else
-- -- everywhere
-- applyLayoutLApplies :: Layout PikaCore.Expr -> String -> [PikaCore.Expr] -> Maybe (LayoutBody PikaCore.Expr) 
-- applyLayoutLApplies layout constructor args = do
--   branch <- lookupLayoutBranch layout constructor
--   let body = _layoutBody branch
--       st = patternMatchSubst' (_layoutPattern branch) constructor args
--       nonApplySubsts = filter (not . is PikaCore._App . snd) st
--   Just
--     (body & (unLayoutBody.traversed._LApply) %~ substs st -- Apply App's
--           & (unLayoutBody.traversed) %~ substs nonApplySubsts) -- Apply non-App's
--
-- lappliesToWiths :: [ModedName PikaCore.Expr] -> [(String, PikaCore.Expr, [PikaCore.ExprName])] -> PikaCore.Expr -> PikaConvert PikaCore.Expr
-- lappliesToWiths outVars [] e = pure e
-- lappliesToWiths outVars ((layout, pikaApp@(PikaCore.App {}), layoutArgs):rest) e = do
--   -- args <- mapM fresh layoutArgs
--   body <- lappliesToWiths outVars rest (substs (zip layoutArgs (map (PikaCore.V . modedNameName) outVars)) e)
--
--   pure $ PikaCore.WithIn
--       pikaApp
--       (bind outVars body)
--
-- lappliesToWiths outVars (_:rest) e = lappliesToWiths outVars rest e
--
-- getLApplies :: LayoutBody a -> [(String, a, [Name a])]
-- getLApplies (LayoutBody x) =
--   toListOf (traversed._LApply) x
-- --
-- -- getSingletonLApplies :: [(String, a, [Name a])] -> [(Name a, (a, String))]
-- -- getSingletonLApplies =
-- --   mapMaybe go
-- --   where
-- --     go (x, y, [z]) = Just (z, (y, x))
-- --     go _ = Nothing
-- --
-- -- substWithLApplies :: [(String, PikaCore.Expr, [PikaCore.ExprName])] -> [PointsTo PikaCore.Expr] -> [PointsTo PikaCore.Expr]
-- -- substWithLApplies = go . getSingletonLApplies
-- --   where
-- --     go :: [(PikaCore.ExprName, (PikaCore.Expr, String))] -> [PointsTo PikaCore.Expr] -> [PointsTo PikaCore.Expr]
-- --     go [] p = p
-- --     go e0@((x, (e, z)):xs) p = subst x e (go xs p)
-- --
-- getPointsTos :: LayoutBody PikaCore.Expr -> [PointsTo PikaCore.Expr]
-- getPointsTos b@(LayoutBody xs0) = go xs0
--   where
--     go [] = []
--     go (LApply {} : xs) = go xs
--     go (LPointsTo p : xs) = p : go xs
-- --
-- -- -- getPointsTos :: LayoutBody PikaCore.Expr -> [PointsTo PikaCore.Expr]
-- -- -- getPointsTos b@(LayoutBody xs0) = substWithLApplies (getLApplies b) $ go xs0
-- -- --   where
-- -- --     go [] = []
-- -- --     go (LApply {} : xs) = go xs
-- -- --     go (LPointsTo p : xs) = p : go xs
-- --
-- --
-- -- --
-- generateParamsM :: Type -> PikaConvert [ModedName PikaCore.Expr]
-- generateParamsM ty = do
--   layouts <- asks _layoutEnv
--   generateParams layouts ty
-- -- --
-- -- -- -- exampleSll :: Layout Pika.Expr
-- -- -- -- exampleSll =
-- -- -- --   Layout
-- -- -- --   { _layoutName = "Sll"
-- -- -- --   , _layoutSig =
-- -- -- --       LayoutSig
-- -- -- --         { _layoutSigAdt = AdtName "List"
-- -- -- --         , _layoutSigParams = [s2n "x"]
-- -- -- --         }
-- -- -- --   , _layoutBranches =
-- -- -- --       [LayoutBranch
-- -- -- --         { _layoutPattern = Pattern "Nil" []
-- -- -- --         , _layoutBody =
-- -- -- --             LayoutBody
-- -- -- --               []
-- -- -- --         }
-- -- -- --       ,LayoutBranch
-- -- -- --         { _layoutPattern = Pattern "Cons" [s2n "head", s2n "tail"]
-- -- -- --         , _layoutBody =
-- -- -- --             LayoutBody
-- -- -- --               [ LPointsTo ((s2n "x" :+ 0) :-> Pika.V (s2n "head"))
-- -- -- --               , LPointsTo ((s2n "x" :+ 1) :-> Pika.V (s2n "nxt"))
-- -- -- --               , LApply "Sll" (Pika.V (s2n "tail")) [s2n "nxt"]
-- -- -- --               ]
-- -- -- --         }
-- -- -- --       ]
-- -- -- --   }
-- -- -- --
-- -- -- -- exampleAdd1Head :: Pika.FnDef
-- -- -- -- exampleAdd1Head =
-- -- -- --   Pika.FnDef
-- -- -- --     { Pika.fnDefName = "add1Head"
-- -- -- --     , Pika.fnDefTypeSig =
-- -- -- --         TypeSig
-- -- -- --           { _typeSigLayoutConstraints = []
-- -- -- --           , _typeSigTy = FnType (TyVar (s2n "Sll")) (TyVar (s2n "Sll"))
-- -- -- --           }
-- -- -- --     , Pika.fnDefBranches =
-- -- -- --         [Pika.FnDefBranch
-- -- -- --           { Pika.fnBranchPats = [Pattern "Nil" []]
-- -- -- --           , Pika.fnBranchBody =
-- -- -- --               (`Pika.ApplyLayout` (s2n "Sll")) $
-- -- -- --                 Pika.App "Nil" []
-- -- -- --           }
-- -- -- --
-- -- -- --         ,Pika.FnDefBranch
-- -- -- --           { Pika.fnBranchPats = [Pattern "Cons" [s2n "h", s2n "t"]]
-- -- -- --           , Pika.fnBranchBody =
-- -- -- --               (`Pika.ApplyLayout` (s2n "Sll")) $
-- -- -- --                 Pika.App "Cons"
-- -- -- --                   [Pika.Add (Pika.V (s2n "h")) (Pika.IntLit 1)
-- -- -- --                   ,Pika.V (s2n "t")
-- -- -- --                   ]
-- -- -- --           }
-- -- -- --         ]
-- -- -- --     }
-- -- -- --
-- -- -- -- exampleMapAdd1 :: Pika.FnDef
-- -- -- -- exampleMapAdd1 =
-- -- -- --   Pika.FnDef
-- -- -- --     { Pika.fnDefName = "mapAdd1"
-- -- -- --     , Pika.fnDefTypeSig =
-- -- -- --         TypeSig
-- -- -- --           { _typeSigLayoutConstraints = []
-- -- -- --           , _typeSigTy = FnType (TyVar (s2n "Sll")) (TyVar (s2n "Sll"))
-- -- -- --           }
-- -- -- --     , Pika.fnDefBranches =
-- -- -- --         [Pika.FnDefBranch
-- -- -- --           { Pika.fnBranchPats = [Pattern "Nil" []]
-- -- -- --           , Pika.fnBranchBody =
-- -- -- --               (`Pika.ApplyLayout` (s2n "Sll")) $
-- -- -- --                 Pika.App "Nil" []
-- -- -- --           }
-- -- -- --
-- -- -- --         ,Pika.FnDefBranch
-- -- -- --           { Pika.fnBranchPats = [Pattern "Cons" [s2n "h", s2n "t"]]
-- -- -- --           , Pika.fnBranchBody =
-- -- -- --               (`Pika.ApplyLayout` (s2n "Sll")) $
-- -- -- --                 Pika.App "Cons"
-- -- -- --                   [Pika.Add (Pika.V (s2n "h")) (Pika.IntLit 1)
-- -- -- --                   ,Pika.App "mapAdd1" [Pika.V (s2n "t")]
-- -- -- --                   ]
-- -- -- --           }
-- -- -- --         ]
-- -- -- --     }
-- -- -- --
-- -- -- -- example_ :: PikaCore.FnDef
-- -- -- -- example_ = 
-- -- -- --     PikaCore.FnDef
-- -- -- --       {PikaCore._fnDefName = "example_"
-- -- -- --       ,PikaCore._fnDefBranches =
-- -- -- --           [PikaCore.FnDefBranch
-- -- -- --             {PikaCore._fnDefOutputParams = [(string2Name "x4")]
-- -- -- --             ,PikaCore._fnDefBranchInputAssertions = [[]]
-- -- -- --             ,PikaCore._fnDefBranchBody = PikaCore.WithIn (PikaCore.SslAssertion [(string2Name "x6")] []) [(string2Name "x5")] (PikaCore.LayoutV [(string2Name "x5")])}
-- -- -- --             ,PikaCore.FnDefBranch
-- -- -- --               {PikaCore._fnDefOutputParams = [(string2Name "x4")]
-- -- -- --               ,PikaCore._fnDefBranchInputAssertions = [[((string2Name "x") :+ 0) :-> PikaCore.V (string2Name "h7"),((string2Name "x") :+ 1) :-> PikaCore.V (string2Name "t8")]]
-- -- -- --               ,PikaCore._fnDefBranchBody =
-- -- -- --                 (PikaCore.SslAssertion [(string2Name "x10")] [((string2Name "x10") :+ 0) :-> PikaCore.Add (PikaCore.V (string2Name "h7")) (PikaCore.IntLit 1),((string2Name "x10") :+ 1) :-> PikaCore.V (string2Name "t8")])
-- -- -- --               }
-- -- -- --           ]
-- -- -- --       ,PikaCore._fnDefParams = [(string2Name "x"),(string2Name "x4")]}
-- -- -- --
