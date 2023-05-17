{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

import Unbound.Generics.LocallyNameless

import GHC.Stack

import Data.Void
import Data.List
import Data.Maybe

import Control.Lens hiding (simple)

import Control.Applicative
import Control.Arrow (second)

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
branchToPikaCore layouts outParams argTypes branch = do
  -- params <- concat <$> mapM (generateParams layouts) (argTypes ++ [resultType])
  let tyPats = zip argTypes (Pika.fnBranchPats branch)

  (vsubst, layoutBodies) <- mkVSubsts layouts tyPats
  exprAsns <- mapM (toExprAsn vsubst layouts) layoutBodies

  newBranchBody <- convertExpr vsubst layouts (Pika.fnBranchBody branch)

  pure $
    PikaCore.FnDefBranch
      { PikaCore.fnDefOutputParams = outParams
      , PikaCore.fnDefBranchInputAssertions = exprAsns
      , PikaCore.fnDefBranchBody = newBranchBody
      }

newtype LayoutVarSubst = LayoutVarSubst [(ExprName, (String, LayoutArg))]
  deriving (Semigroup, Monoid)

-- | Get location variables associated to expression variable, based on the
-- LApply's in a LayoutBranch
findLApplies :: LayoutBranch Pika.Expr -> ExprName -> Maybe (String, [LocName])
findLApplies branch n =
    second (map undefined) <$> go' (unLayoutBody (_layoutBody branch))
  where
    go' [] = Nothing
    go' (x:xs) = go x <|> go' xs

    go (LPointsTo {}) = Nothing
    go (LApply layout (Pika.V n') vars)
      | n' == n = Just (layout, vars)
    go _ = Nothing

lookupVSubst :: ExprName -> LayoutVarSubst -> Maybe (String, LayoutArg)
lookupVSubst e (LayoutVarSubst s) = lookup e s

mkVSubsts :: [Layout Pika.Expr] -> [(Type, Pattern Pika.Expr)] -> FreshM (LayoutVarSubst, [LayoutBody Pika.Expr])
mkVSubsts layouts tyPats =
  fmap (go . unzip) (traverse (uncurry (mkVSubst layouts)) tyPats)
  where
    go :: ([LayoutVarSubst], [LayoutBody Pika.Expr]) -> (LayoutVarSubst, [LayoutBody Pika.Expr])
    go = _1 %~ mconcat

mkVSubst :: [Layout Pika.Expr] -> Type -> Pattern Pika.Expr -> FreshM (LayoutVarSubst, LayoutBody Pika.Expr)
mkVSubst _ IntType pat = do
  let [v] = _patVars pat
  v' <- fresh v
  pure (LayoutVarSubst [(v, ("Int", [expr2LocName v']))], mempty)

mkVSubst _ BoolType pat = do
  let [v] = _patVars pat
  v' <- fresh v
  pure (LayoutVarSubst [(v, ("Bool", [expr2LocName v']))], mempty)

mkVSubst layouts (TyVar n) pat = do
  let layoutString = name2String n
      layout = lookupLayout layouts layoutString
      branch = lookupLayoutBranch' layout (_patConstructor pat)
      vars = _patVars pat
      go v = fromMaybe (layoutString, [expr2LocName v]) (findLApplies branch v)
      locNameLists = map go vars

  locs <- (traversed._2.traversed %%~ fresh) locNameLists
  pure (LayoutVarSubst $ zip vars locs, _layoutBody branch)

mkVSubst layouts t@(FnType {}) _ = error $ "mkVSubst: " ++ show t

expr2LocName :: ExprName -> LocName
expr2LocName = string2Name . name2String

generatePatternAsn :: LayoutVarSubst -> [Layout Pika.Expr] -> Type -> Pattern Pika.Expr -> FreshM PikaCore.ExprAssertion
generatePatternAsn vsubst layouts IntType pat = pure []
generatePatternAsn vsubst layouts BoolType pat = pure []
generatePatternAsn _ _ (FnType {}) _ = error "generatePatternAsn: function type"
generatePatternAsn vsubst layouts (TyVar n) pat =
  let layout = lookupLayout layouts (name2String n)
      LayoutBody z = applyLayout' layout (_patConstructor pat) (map Pika.V (_patVars pat))
  in
  concat <$> mapM (toExprHeaplets vsubst layouts) z

toExprAsn :: LayoutVarSubst -> [Layout Pika.Expr] -> LayoutBody Pika.Expr -> FreshM PikaCore.ExprAssertion
toExprAsn vsubst layouts (LayoutBody xs) =
  concat <$> mapM (toExprHeaplets vsubst layouts) xs

-- | Precondition the expressions in 'LApply's should be fully reduced
toExprHeaplets :: LayoutVarSubst -> [Layout Pika.Expr] -> LayoutHeaplet Pika.Expr -> FreshM [PikaCore.PointsToExpr]
toExprHeaplets vsubst layouts (LPointsTo x) = do
    z <- mapM (fmap toBase . convertExpr vsubst layouts) x
    pure [z]
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
toExprHeaplets vsubst layouts e0@(LApply {}) = error $ "toExprHeaplets: " ++ show e0

convertName :: Pika.ExprName -> PikaCore.ExprName
convertName = string2Name . name2String

toV :: HasCallStack => Pika.Expr -> Pika.ExprName
toV (Pika.V x) = x
toV e = error $ "toV: got " ++ show e

toBase :: HasCallStack => PikaCore.Expr -> PikaCore.Base
toBase (PikaCore.SimpleExpr (PikaCore.BaseExpr e)) = e
toBase e = error $ "toBase: " ++ ppr' e

-- applyLayoutOrNOP :: Layout -> String -> [Pika.Expr] -> PikaCore.Expr
-- applyLayoutOrNOP layout constructor args =
--   let params = _layoutParams layout
--       LayoutBody asns = applyLayout layout constructor args
--   in
--   fromMaybe (App constructor args)
--             (PikaCore.SimpleExpr $ SslAssertion undefined)

convertExpr :: LayoutVarSubst -> [Layout Pika.Expr] -> Pika.Expr -> FreshM PikaCore.Expr
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
    go (Pika.LocV x) = pure . base $ PikaCore.LocV $ LocVar x
    go e0@(Pika.LName x) = error $ "convertExpr.go: " ++ show e0
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
      case lookupVSubst x vsubst of
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

isConstructor :: [Layout a] -> String -> Bool
isConstructor layouts c =
  let constructors =
        layouts ^.. (traversed.layoutBranches.traversed.layoutPattern.patConstructor)
  in
  c `elem` constructors

generatePatternParams :: [Layout a] -> Type -> Pattern Pika.Expr -> FreshM [LocName]
generatePatternParams layouts IntType pat = generateParams layouts IntType
generatePatternParams layouts BoolType pat = generateParams layouts BoolType
-- generatePatternParams layouts (TyVar v) pat = generateParams layouts (TyVar v)
generatePatternParams layouts (TyVar n) pat =
  let layout = lookupLayout layouts (name2String n)
      -- z = applyLayout layout (_patConstructor pat) (map Pika.V (_patVars pat))
  in
  mapM fresh $ _layoutParams layout
generatePatternParams _ t@(FnType {}) pat = error $ "generatePatternParams: " ++ show t 

generateParams :: [Layout a] -> Type -> FreshM [LocName]
generateParams layouts IntType = (:[]) <$> fresh (string2Name "i")
generateParams layouts BoolType = (:[]) <$> fresh (string2Name "b")
generateParams layouts t@(FnType {}) = error $ "generateParams: " ++ show t
-- generateParams _ (TyVar v) = error $ "generateParams: Still has layout type variable " ++ show v
generateParams layouts (TyVar layoutName) =
  let layout = lookupLayout layouts (name2String layoutName)
  in
  mapM fresh $ _layoutParams layout

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

