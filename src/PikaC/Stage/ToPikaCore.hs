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

import Control.Applicative
import Control.Arrow (second)

import Control.Monad.Reader

import Debug.Trace

toPikaCore :: [Layout Pika.Expr] -> Pika.FnDef -> PikaCore.FnDef
toPikaCore layouts0 fn = runFreshM . runPikaIntern' $ do
  layouts <- mapM convertLayout layouts0

  outParams <- generateParams layouts resultType

  newBranches <- mapM (branchToPikaCore layouts outParams argTypes) (Pika.fnDefBranches fn)

  let argLayouts = map (typeLayout layouts) argTypes
  rhos <- mapM (traverse layoutParamRename) argLayouts

  let freshenedBranches = map (freshenBranchAsn rhos) newBranches

  let inParams = fastNub . map (locBase . pointsToLhs) $ concat $ concatMap PikaCore._fnDefBranchInputAssertions freshenedBranches
  let params = inParams ++ outParams

  simplifyFnDef $
  -- pure $
    PikaCore.FnDef
      { PikaCore._fnDefName = Pika.fnDefName fn
      , PikaCore._fnDefParams = params
      , PikaCore._fnDefBranches = freshenedBranches
      }
  where
    (argTypes, resultType) = splitFnType (_typeSigTy (Pika.fnDefTypeSig fn))

-- | One rename for the layout associated to each parameter
freshenBranchAsn :: [Maybe (Rename PikaCore.ExprName)] -> PikaCore.FnDefBranch -> PikaCore.FnDefBranch
freshenBranchAsn rhos branch =
    let asns = PikaCore._fnDefBranchInputAssertions branch
        asns' = zipWith go asns rhos
    in
    branch
      { PikaCore._fnDefBranchInputAssertions = asns'
      }
  where
    go :: PikaCore.ExprAssertion -> Maybe (Rename PikaCore.ExprName) -> PikaCore.ExprAssertion
    go asn (Just rho) = rename rho asn
    go asn Nothing = asn

freshenAsnLhs :: Fresh m => PikaCore.ExprAssertion -> m PikaCore.ExprAssertion
freshenAsnLhs [] = pure []
freshenAsnLhs ((x :+ i) :-> y : rest) = do
  x' <- fresh x
  fmap ((x' :+ i) :-> y :) (freshenAsnLhs rest)

branchToPikaCore :: [Layout PikaCore.Expr] -> [PikaCore.ExprName] -> [Type] -> Pika.FnDefBranch -> PikaIntern PikaCore.FnDefBranch
branchToPikaCore layouts outParams argTypes branch = do
  pats <- mapM convertPattern (Pika.fnBranchPats branch)
  let tyPats = zip argTypes pats

  runPikaConvert'' mempty layouts $ do
    let exprAsns =
          map getPointsTos $
          zipWith (handlePattern layouts) argTypes pats


    newBranchBody <- convertExpr (Pika.fnBranchBody branch)

    pure $
      PikaCore.FnDefBranch
        { PikaCore._fnDefOutputParams = outParams
        , PikaCore._fnDefBranchInputAssertions = exprAsns
        , PikaCore._fnDefBranchBody = newBranchBody
        }

 -- TODO: Handle base types
handlePattern :: [Layout PikaCore.Expr] -> Type -> Pattern PikaCore.Expr -> LayoutBody PikaCore.Expr
-- handlePattern IntType (PatternVar v) =
-- handlePattern BoolType (PatternVar v) =
handlePattern layouts ty (PatternVar v) =
  error $ "handlePattern: pattern variable " ++ show v ++ ", for type " ++ show ty
handlePattern layouts (TyVar n) (Pattern c vars) =
  let layout = lookupLayout layouts (name2String n)
      applied = applyLayout' layout c (map PikaCore.V vars)
  in
  applied

handlePattern _ ty pat@(Pattern {}) =
  error $ "handlePattern: Trying to match with pattern " ++ ppr' pat ++ " on type " ++ ppr' ty

typeLayout :: [Layout a] -> Type -> Maybe (Layout a)
typeLayout layouts (TyVar n) = Just $ lookupLayout layouts (name2String n)
typeLayout _ _ = Nothing

-- | Remove all LApply's. NOTE: Does not substitute using LApply's
truncatedLayoutBody :: LayoutBody a -> [PointsTo a]
truncatedLayoutBody (LayoutBody xs0) = go xs0
  where
    go [] = []
    go (LApply {} : xs) = go xs
    go (LPointsTo p : xs) = p : go xs

convertPattern :: MonadPikaIntern m => Pattern Pika.Expr -> m (Pattern PikaCore.Expr)
convertPattern (PatternVar v) = PatternVar <$> internExprName v
convertPattern (Pattern constructor vars) = do
  vars <- mapM internExprName vars
  pure $ Pattern constructor vars

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
convertLayout layout0 = do
  branches <- mapM convertLayoutBranch $ _layoutBranches layout0
  params <- mapM internExprName $ _layoutSigParams $ _layoutSig layout0

  pure $
    Layout
    { _layoutName = _layoutName layout0
    , _layoutBranches = branches
    -- , _layoutParams = params
    , _layoutSig = (_layoutSig layout0) { _layoutSigParams = params }
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
  mapM fresh $ _layoutSigParams $ _layoutSig layout

convertExpr :: Pika.Expr -> PikaConvert PikaCore.Expr
convertExpr = go . Pika.reduceLayouts
  where
    go :: Pika.Expr -> PikaConvert PikaCore.Expr
    go e0@(Pika.LName x) = error $ "convertExpr.go: " ++ show e0
    go (Pika.V n) = PikaCore.V <$> internExprName n
    go (Pika.IntLit i) = pure $ PikaCore.IntLit i
    go (Pika.BoolLit b) = pure $ PikaCore.BoolLit b
    go e0@(Pika.LayoutLambda {}) =
      error $ "convertExpr: " ++ show e0
    go e0@(Pika.ApplyLayout (Pika.V x) layoutName) = do
      let layoutString = name2String layoutName
      (n, y) <- vsubstLookupM x
      if n == layoutString
        then pure $ PikaCore.LayoutV y
        else error $ "convertExpr: layout " ++ layoutString ++ " does not match " ++ n ++ " in " ++ show e0

    go e0@(Pika.ApplyLayout (Pika.App f xs) layoutName) = scoped $ do
      rs <- generateParamsM (TyVar layoutName)

      layout <- lookupLayoutM (name2String layoutName)

      app <- applyLayoutOrNOP layout f xs

      pure $
        PikaCore.WithIn
          app
          rs
           (PikaCore.LayoutV rs)
    go e0@(Pika.ApplyLayout {}) = error $ "convertExpr.go: " ++ show e0
    go e0@(Pika.App f xs) = do
      PikaCore.App f <$> mapM go xs
    go (Pika.Add x y) =
      PikaCore.Add <$> convertBase x <*> convertBase y
    go (Pika.Sub x y) =
      PikaCore.Sub <$> convertBase x <*> convertBase y
    go (Pika.Equal x y) =
      PikaCore.Equal <$> convertBase x <*> convertBase y

applyLayoutOrNOP :: Layout PikaCore.Expr -> String -> [Pika.Expr] -> PikaConvert PikaCore.Expr
applyLayoutOrNOP layout constructor args = do
  args' <- mapM convertExpr args
  fmap (fromMaybe (PikaCore.App constructor args')) $ do
    let params = _layoutSigParams $ _layoutSig layout
    let xs = applyLayout layout constructor args'

    case applyLayout layout constructor args' of
      Nothing -> pure Nothing
      Just asns -> do
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

-- exampleSll :: Layout Pika.Expr
-- exampleSll =
--   Layout
--   { _layoutName = "Sll"
--   , _layoutSig =
--       LayoutSig
--         { _layoutSigAdt = AdtName "List"
--         , _layoutSigParams = [s2n "x"]
--         }
--   , _layoutBranches =
--       [LayoutBranch
--         { _layoutPattern = Pattern "Nil" []
--         , _layoutBody =
--             LayoutBody
--               []
--         }
--       ,LayoutBranch
--         { _layoutPattern = Pattern "Cons" [s2n "head", s2n "tail"]
--         , _layoutBody =
--             LayoutBody
--               [ LPointsTo ((s2n "x" :+ 0) :-> Pika.V (s2n "head"))
--               , LPointsTo ((s2n "x" :+ 1) :-> Pika.V (s2n "nxt"))
--               , LApply "Sll" (Pika.V (s2n "tail")) [s2n "nxt"]
--               ]
--         }
--       ]
--   }
--
-- exampleAdd1Head :: Pika.FnDef
-- exampleAdd1Head =
--   Pika.FnDef
--     { Pika.fnDefName = "add1Head"
--     , Pika.fnDefTypeSig =
--         TypeSig
--           { _typeSigLayoutConstraints = []
--           , _typeSigTy = FnType (TyVar (s2n "Sll")) (TyVar (s2n "Sll"))
--           }
--     , Pika.fnDefBranches =
--         [Pika.FnDefBranch
--           { Pika.fnBranchPats = [Pattern "Nil" []]
--           , Pika.fnBranchBody =
--               (`Pika.ApplyLayout` (s2n "Sll")) $
--                 Pika.App "Nil" []
--           }
--
--         ,Pika.FnDefBranch
--           { Pika.fnBranchPats = [Pattern "Cons" [s2n "h", s2n "t"]]
--           , Pika.fnBranchBody =
--               (`Pika.ApplyLayout` (s2n "Sll")) $
--                 Pika.App "Cons"
--                   [Pika.Add (Pika.V (s2n "h")) (Pika.IntLit 1)
--                   ,Pika.V (s2n "t")
--                   ]
--           }
--         ]
--     }
--
-- exampleMapAdd1 :: Pika.FnDef
-- exampleMapAdd1 =
--   Pika.FnDef
--     { Pika.fnDefName = "mapAdd1"
--     , Pika.fnDefTypeSig =
--         TypeSig
--           { _typeSigLayoutConstraints = []
--           , _typeSigTy = FnType (TyVar (s2n "Sll")) (TyVar (s2n "Sll"))
--           }
--     , Pika.fnDefBranches =
--         [Pika.FnDefBranch
--           { Pika.fnBranchPats = [Pattern "Nil" []]
--           , Pika.fnBranchBody =
--               (`Pika.ApplyLayout` (s2n "Sll")) $
--                 Pika.App "Nil" []
--           }
--
--         ,Pika.FnDefBranch
--           { Pika.fnBranchPats = [Pattern "Cons" [s2n "h", s2n "t"]]
--           , Pika.fnBranchBody =
--               (`Pika.ApplyLayout` (s2n "Sll")) $
--                 Pika.App "Cons"
--                   [Pika.Add (Pika.V (s2n "h")) (Pika.IntLit 1)
--                   ,Pika.App "mapAdd1" [Pika.V (s2n "t")]
--                   ]
--           }
--         ]
--     }
--
-- example_ :: PikaCore.FnDef
-- example_ = 
--     PikaCore.FnDef
--       {PikaCore._fnDefName = "example_"
--       ,PikaCore._fnDefBranches =
--           [PikaCore.FnDefBranch
--             {PikaCore._fnDefOutputParams = [(string2Name "x4")]
--             ,PikaCore._fnDefBranchInputAssertions = [[]]
--             ,PikaCore._fnDefBranchBody = PikaCore.WithIn (PikaCore.SslAssertion [(string2Name "x6")] []) [(string2Name "x5")] (PikaCore.LayoutV [(string2Name "x5")])}
--             ,PikaCore.FnDefBranch
--               {PikaCore._fnDefOutputParams = [(string2Name "x4")]
--               ,PikaCore._fnDefBranchInputAssertions = [[((string2Name "x") :+ 0) :-> PikaCore.V (string2Name "h7"),((string2Name "x") :+ 1) :-> PikaCore.V (string2Name "t8")]]
--               ,PikaCore._fnDefBranchBody =
--                 (PikaCore.SslAssertion [(string2Name "x10")] [((string2Name "x10") :+ 0) :-> PikaCore.Add (PikaCore.V (string2Name "h7")) (PikaCore.IntLit 1),((string2Name "x10") :+ 1) :-> PikaCore.V (string2Name "t8")])
--               }
--           ]
--       ,PikaCore._fnDefParams = [(string2Name "x"),(string2Name "x4")]}
--
