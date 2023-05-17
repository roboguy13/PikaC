{-# LANGUAGE TypeApplications #-}

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

import Unbound.Generics.LocallyNameless

import GHC.Stack

import Data.Void

import Control.Lens hiding (simple)

toPikaCore :: [Layout] -> Pika.FnDef -> PikaCore.FnDef
toPikaCore layouts fn = runFreshM $ do
  outParams <- generateParams layouts resultType
  inParams <- concat <$> mapM (generateParams layouts) argTypes
  let params = inParams ++ outParams
  pure $
    PikaCore.FnDef
      { PikaCore.fnDefName = Pika.fnDefName fn
      , PikaCore.fnDefParams = params
      , PikaCore.fnDefBranches = map (branchToPikaCore layouts outParams) (Pika.fnDefBranches fn)
      }
  where
    (argTypes, resultType) = splitFnType (Pika.fnDefType fn)

branchToPikaCore :: [Layout] -> [LocName] -> Pika.FnDefBranch -> PikaCore.FnDefBranch
branchToPikaCore layouts outParams branch = runFreshM $ do
  -- params <- concat <$> mapM (generateParams layouts) (argTypes ++ [resultType])
  pure $
    PikaCore.FnDefBranch
      { PikaCore.fnDefOutputParams = outParams
      }

mkVSubst :: [Layout] -> Type -> Pattern -> FreshM LayoutVarSubst
mkVSubst _ IntType pat = undefined
mkVSubst layouts (TyVar n) pat =
  let layout = lookupLayout layouts (name2String n)
      branch = lookupLayoutBranch' layout (_patConstructor pat)
  in
  undefined

generatePatternAsn :: LayoutVarSubst -> [Layout] -> Type -> Pattern -> FreshM PikaCore.ExprAssertion
generatePatternAsn vsubst layouts IntType pat = pure []
generatePatternAsn vsubst layouts BoolType pat = pure []
generatePatternAsn vsubst layouts (TyVar n) pat =
  let layout = lookupLayout layouts (name2String n)
      LayoutBody z = applyLayout' layout (_patConstructor pat) (map Pika.V (_patVars pat))
  in
  concat <$> mapM (toExprHeaplets vsubst layouts) z

-- | Precondition the expressions in 'LApply's should be fully reduced
toExprHeaplets :: LayoutVarSubst -> [Layout] -> LayoutHeaplet -> FreshM [PikaCore.PointsToExpr]
toExprHeaplets vsubst layouts (LPointsTo x) =
    let z = mapM (fmap toBase . convertExpr vsubst layouts) x
    in
    undefined
    -- pure $ (:[]) $ fmap (toBase . convertExpr vsubst layouts) x
toExprHeaplets vsubst layouts (LApply layout (Pika.V _) params) = pure []
toExprHeaplets vsubst layouts e0@(LApply layout (Pika.App f xs) params)
  | not (isConstructor layouts f) =
      error $ "toExprHeaplet: Got application that is not a constructor application" ++ show e0
  | otherwise =
      generatePatternAsn vsubst layouts (TyVar (string2Name layout)) 
        Pattern
          { _patConstructor = f
          , _patVars = map toV xs
          }

convertName :: Pika.ExprName -> PikaCore.ExprName
convertName = string2Name . name2String

toV :: HasCallStack => Pika.Expr -> Pika.ExprName
toV (Pika.V x) = x
toV e = error $ "toV: got " ++ show e

toBase :: HasCallStack => PikaCore.Expr -> PikaCore.Base
toBase (PikaCore.SimpleExpr (PikaCore.BaseExpr e)) = e
toBase e = error $ "toBase: " ++ ppr' e

type LayoutVarSubst = [(ExprName, (String, LayoutArg))]

convertExpr :: LayoutVarSubst -> [Layout] -> Pika.Expr -> FreshM PikaCore.Expr
convertExpr vsubst layouts = go . reduceLayouts
  where
    goBase :: Pika.Expr -> FreshM PikaCore.Base
    goBase (Pika.V n) = pure $ PikaCore.V $ convertName n
    goBase (Pika.Add x y) = PikaCore.Add <$> goBase x <*> goBase y
    goBase (Pika.Sub x y) = PikaCore.Sub <$> goBase x <*> goBase y
    goBase (Pika.Equal x y) = PikaCore.Equal <$> goBase x <*> goBase y
    goBase (Pika.IntLit i) = pure $ PikaCore.IntLit i
    goBase (Pika.BoolLit b) = pure $ PikaCore.BoolLit b
    goBase e = error $ "convertExpr.goBase: " ++ show e

    go :: Pika.Expr -> FreshM PikaCore.Expr
    go (Pika.V n) = pure . base $ PikaCore.V $ convertName n
    -- go e0@(Pika.LayoutTypeArg {}) =
    --   error $ "convertExpr: " ++ show e0
    go (Pika.IntLit i) = pure . base $ PikaCore.IntLit i
    go (Pika.BoolLit b) = pure . base $ PikaCore.BoolLit b
    go e0@(Pika.LayoutLambda {}) =
      error $ "convertExpr: " ++ show e0
    -- go e0@(Pika.Lower e (LayoutNameB layout)) =
      -- error $ "convertExpr: Free layout variable in: " ++ show e0
    go e0@(Pika.ApplyLayout (Pika.V x) layoutName) =
      let layoutString = name2String (unTypeVar layoutName)
      in
      case lookup x vsubst of
        Nothing -> error $ "convertExpr: Cannot find variable " ++ show x ++ " in variable -> layout variable list conversion table"
        Just (n, y)
          | n == layoutString -> pure . base $ PikaCore.LayoutV (map LocVar y)
          | otherwise ->
              error $ "convertExpr: layout " ++ layoutString ++ " does not match " ++ n ++ " in " ++ show e0
    go e0@(Pika.ApplyLayout (Pika.App f xs) layoutName) = do
      rs <- generateParams layouts (TyVar (unTypeVar layoutName))
      ys <- mapM go xs
      pure $
        simple $ PikaCore.WithIn
          (PikaCore.App f ys)
          $ bind rs
              (PikaCore.BaseExpr (PikaCore.LayoutV (map LocVar rs)))
    go (Pika.Add x y) =
      fmap base (PikaCore.Add <$> goBase x <*> goBase y)
    go (Pika.Sub x y) =
      fmap base (PikaCore.Sub <$> goBase x <*> goBase y)
    go (Pika.Equal x y) =
      fmap base (PikaCore.Equal <$> goBase x <*> goBase y)
    -- go (Pika.Not x) =
    --   fmap base (PikaCore.Not <$> go x)

isConstructor :: [Layout] -> String -> Bool
isConstructor layouts c =
  let constructors =
        layouts ^.. (traversed.layoutBranches.traversed.layoutPattern.patConstructor)
  in
  c `elem` constructors

generatePatternParams :: [Layout] -> Type -> Pattern -> FreshM [LocName]
generatePatternParams layouts IntType pat = generateParams layouts IntType
generatePatternParams layouts BoolType pat = generateParams layouts BoolType
-- generatePatternParams layouts (TyVar v) pat = generateParams layouts (TyVar v)
generatePatternParams layouts (TyVar n) pat =
  let layout = lookupLayout layouts (name2String n)
      -- z = applyLayout layout (_patConstructor pat) (map Pika.V (_patVars pat))
  in
  mapM fresh $ _layoutParams layout

generateParams :: [Layout] -> Type -> FreshM [LocName]
generateParams layouts IntType = (:[]) <$> fresh (string2Name "i")
generateParams layouts BoolType = (:[]) <$> fresh (string2Name "b")
-- generateParams _ (TyVar v) = error $ "generateParams: Still has layout type variable " ++ show v
generateParams layouts (TyVar layoutName) =
  let layout = lookupLayout layouts (name2String layoutName)
  in
  mapM fresh $ _layoutParams layout

