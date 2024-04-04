{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PikaC.TypeChecker.Elaborate
  (TypedExpr (..)
  ,elaborate
  ,elaborateFnDef
  ,inferExpr
  ,CheckEnv (..)
  ,toPikaModuleElaborated_unsafe
  )
  where

import PikaC.Syntax.Type
import PikaC.Syntax.Pika.Expr
import PikaC.Syntax.Pika.FnDef
import PikaC.Syntax.Pika.Pattern
import PikaC.Syntax.Pika.Layout (LayoutName)
import PikaC.Syntax.Pika.Parser
import PikaC.Stage.Defunctionalize.Mangle
import PikaC.Stage.Defunctionalize.Defunctionalize


import PikaC.TypeChecker.Monad
import PikaC.TypeChecker.Unify

import PikaC.Utils
import PikaC.Ppr

import Control.Monad.State
import Control.Monad.Reader

import Control.Lens

import Control.Monad
import Control.Applicative

import Control.Monad.Morph

import Data.List
import Data.Maybe

import Data.Foldable

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Unsafe

import Debug.Trace

data TypedExpr = Expr ::: Type
  deriving (Show)

toPikaModuleElaborated_unsafe :: PikaModule -> PikaModuleElaborated
toPikaModuleElaborated_unsafe pikaModule =
  defunctionalizeModule $
  pikaModule { moduleFnDefs = map (mangleFnDef . overTypedBranches fromTypeSig_unsafe) (moduleFnDefs pikaModule) }

-- | Eta-expand to layout lambdas applied to placeholder variables
etaExpand :: Expr -> Check s Expr
etaExpand = go
  where
    go (LayoutLambda adt bnd) = do
      (v, body) <- unbind bnd
      case body of
        App f args -> App f <$> traverse go args
        _ -> LayoutLambda adt <$> bind v <$> go body

    go (App f xs)
      | isConstructor f = constructorToLayoutLambda f xs
      | otherwise = do
          fmap (lookup (name2String f)) (asks _fnEnv) >>= \case
            Just (TypeSig sigBnd) -> do
              (vs, (ConstrainedType cts ty, ())) <- unbind sigBnd
              toLayoutLambdas cts =<< (App f <$> traverse go xs)

    go e = plate go e -- TODO: Does this work?

-- | Generate layout lambdas with applications to placeholder variables
toLayoutLambdas :: Fresh m => [LayoutConstraint] -> Expr -> m Expr
toLayoutLambdas [] e = pure e
toLayoutLambdas ((v :~ adt) : rest) e = do
  pv <- fresh v
  ApplyLayout
    <$> toLayoutLambdas rest e --(LayoutLambda adt <$> bind v <$> toLayoutLambdas rest e)
    -- <*> pure (TyVar pv)
    <*> pure (PlaceholderVar pv)

-- | Eta-expand the layout lambdas of data type constructors, applied to
-- placeholder variables
constructorToLayoutLambda :: ExprName -> [Expr] -> Check s Expr
constructorToLayoutLambda c args = do
  cts <- constructorConstraints c
  toLayoutLambdas cts (App c args)

constructorConstraints :: ExprName -> Check s [LayoutConstraint]
constructorConstraints c = do
  fmap (lookup (name2String c)) (asks _constructorTypes) >>= \case
    Just ty -> do
      forAllsToCts ty
  
data Ctx = Ctx { _ctxDelta :: [(TypeName, AdtName)], _ctxGamma :: [(ExprName, Type)] }
  deriving (Show)

ctxSingleDelta :: TypeName -> AdtName -> Ctx
ctxSingleDelta x y = mempty { _ctxDelta = [(x, y)] }

ctxSingleGamma :: ExprName -> Type -> Ctx
ctxSingleGamma x y = mempty { _ctxGamma = [(x, y)] }

instance Semigroup Ctx where
  Ctx xs1 ys1 <> Ctx xs2 ys2 = Ctx (xs1 <> xs2) (ys1 <> ys2)

instance Monoid Ctx where
  mempty = Ctx mempty mempty

makeLenses ''Ctx

ctxLookupExpr :: ExprName -> Ctx -> Maybe Type
ctxLookupExpr n = lookup n . _ctxGamma

ctxLookupType :: TypeName -> Ctx -> Maybe AdtName
ctxLookupType n = lookup n . _ctxDelta

ctxExtendExpr :: ExprName -> Type -> Ctx -> Ctx
ctxExtendExpr n ty = ctxGamma %~ ((n, ty) :)

ctxExtendType :: TypeName -> AdtName -> Ctx -> Ctx
ctxExtendType n adt = ctxDelta %~ ((n, adt) :)

lookupFnType_maybe :: String -> Check s (Maybe Type)
lookupFnType_maybe f
  | isConstructor (string2Name f) =
      fmap (lookup f) (asks _constructorTypes) >>= \case
        -- Nothing -> checkError $ text "Cannot find constructor" <+> text f
        Nothing -> pure Nothing
        Just r -> pure $ Just r
  | otherwise =
      asks (checkEnvLookupFn f) >>= \case
        Nothing -> pure Nothing
        Just sig -> pure $ Just $ typePairType $ fromTypeSig sig

lookupFnType :: String -> Check s Type
lookupFnType f =
  lookupFnType_maybe f >>= \case
    Nothing -> checkError $ text "Cannot find function" <+> text f
    Just r -> pure r

lookupType :: Ctx -> ExprName -> Check s Type
lookupType ctx x =
  liftA2 (<|>)
    (pure (ctxLookupExpr x ctx))
    (lookupFnType_maybe (name2String x)) >>= \case
      Nothing -> checkError $ text "Cannot find type for name" <+> ppr x
      Just ty -> pure ty

scopeCheck :: Ctx -> Type -> Check s ()
scopeCheck ctx = \case
  TyVar v ->
    case ctxLookupType v ctx of
      Nothing -> checkError $ text "Cannot find type variable" $$ nest 2 (ppr v)
      Just adt -> pure ()
        -- | otherwise -> checkError $ text "Type variable" $$ nest 2 (ppr v) $$ text "is constrained to layouts for the ADT" $$ nest 2 (ppr adt') $$ text "while I expected it to be for ADT" $$ nest 2 (ppr adt)

  ForAll bnd -> do
    ((v, Embed adt), body) <- unbind bnd
    scopeCheck (ctxExtendType v adt ctx) body

  ty -> mapM_ (scopeCheck ctx) $ children ty

splitTypedExprs :: [TypedExpr] -> ([Expr], [Type])
splitTypedExprs = unzip . map (\(e ::: t) -> (e, t))

genElaborationConstraints :: [Equal] -> Ctx -> Type -> Expr -> Check s [Equal]
genElaborationConstraints eqs = go
  where
    go :: Ctx -> Type -> Expr -> Check s [Equal]
    go ctx ty = \case
      V x -> pure []
      e0@(ApplyLayout body (PlaceholderVar x)) -> do
        -- (ForAll bodyBnd) <- inferExpr ctx body
        bodyTy <- inferExpr ctx body
        -- ((v, Embed _adt), bodyTy) <- unbind bodyBnd
        -- trace ("bodyTy = " ++ ppr' bodyTy ++ ", ty = " ++ ppr' ty ++ ", v = " ++ ppr' v)
        unifyWith e0 bodyTy ty eqs

      LayoutLambda adt bnd -> do
        (v, body) <- unbind bnd
        tyVar <- fresh v
        go (ctxExtendType v adt ctx) (ForAll (bind (v, Embed adt) ty)) body

      e -> do
        let cs = children e
        cTypes <- traverse (inferExpr ctx) cs
        concat <$> zipWithM (go ctx) cTypes cs

fillInPlaceholders :: [TypeName] -> [Equal] -> Expr -> Check s Expr
fillInPlaceholders sigVars eqs = go
  where
    go (ApplyLayout body ty) =
      ApplyLayout body <$> getNewType sigVars eqs ty
    go e = plate go e

-- TODO: Generate PlaceholderVars
elaborate :: [TypeName] -> Type -> Ctx -> Expr -> Check s TypedExpr
elaborate sigVars expectedType ctx e0 = do 

  e <- etaExpand e0
  e0Ty <- inferExpr ctx e
  -- ty <- trace (" e0 = " ++ show e ++ ", e0Ty = " ++ ppr' e0Ty) $ inferExpr ctx e
  ty <- inferExpr ctx e
  eqs0 <- unify e0 e0Ty expectedType
  eqs <- genElaborationConstraints eqs0 ctx ty e
  e' <- fillInPlaceholders sigVars eqs e
  pure (e' ::: ty)

elaborateFnDef :: CheckEnv -> FnDef' TypeSig' -> Either String (FnDef' TypeSig')
elaborateFnDef env fnDef = runCheck env $ do
  (sigVars, (ty, branches)) <- unbind $ _typeSigConstrainedType $ fnDefTypedBranches fnDef
  let (argTys, resultTy) = splitFnType $ _ctypeTy ty

  branches' <- mapM (go sigVars argTys resultTy) branches
  pure $ fnDef { fnDefTypedBranches = TypeSig $ bind sigVars (ty, branches') }
  where

    go :: [TypeName] -> [Type] -> Type -> FnDefBranch -> Check s FnDefBranch
    go sigVars argTys resultTy (FnDefBranch (PatternMatches bnd)) = do
      (pats, body) <- unbind bnd

      ctx <- handlePatterns sigVars argTys pats

      FnDefBranch . PatternMatches <$> bind pats <$> go' sigVars argTys resultTy ctx body 

    go' :: [TypeName] -> [Type] -> Type -> Ctx -> GuardedExpr -> Check s GuardedExpr
    go' sigVars argTys resultTy ctx (GuardedExpr cond body) = do
      -- body' ::: _ <- elaborate sigVars (mkFnType (argTys ++ [resultTy])) ctx body
      body' ::: _ <- elaborate sigVars resultTy ctx body
      pure $ GuardedExpr cond body'

handlePatterns :: [TypeName] -> [Type] -> [Pattern Expr] -> Check s Ctx
handlePatterns sigVars patTys =
  fmap mconcat . zipWithM (handlePattern sigVars) patTys

handlePattern :: [TypeName] -> Type -> Pattern Expr -> Check s Ctx
handlePattern sigVars patTy (PatternVar x) = pure $ ctxSingleGamma x patTy
handlePattern sigVars patTy pat@(Pattern cName _) = do
  cTyAssocs <- asks _constructorTypes
  fmap (lookup cName) (asks _constructorTypes) >>= \case
    Just cTy -> handlePattern' sigVars patTy cTy pat
    Nothing -> error $ "handlePattern: Cannot find type for constructor " ++ ppr' cName ++ " in " ++ show cTyAssocs

-- TODO: Handle ADTs that reference other ADTs
-- | Uses the constructor type
handlePattern' :: [TypeName] -> Type -> Type -> Pattern Expr -> Check s Ctx
handlePattern' sigVars patTy cTy (PatternVar x) = pure $ ctxSingleGamma x patTy
handlePattern' _ _ _ (Pattern _ []) = pure mempty
handlePattern' sigVars patTy (ForAll cTy) pat0@(Pattern _ vs) = do
  (_vs, cTy') <- unbind cTy

  let (cArgTys, cResultTy) = splitFnType cTy'
  eqs <- unify pat0 cResultTy patTy

  cArgTys' <- mapM (getNewType sigVars eqs) cArgTys

  pure $ foldMap (uncurry ctxSingleGamma) $ zip vs cArgTys'
handlePattern' _ _ cTy _ = error $ "handlePattern': got constructor type: " ++ ppr' cTy

-- | Does not check constraints
inferExpr :: Ctx -> Expr -> Check s Type
inferExpr = go
  where
    go :: Ctx -> Expr -> Check s Type
    go ctx (V x) = do
      ty <- lookupType ctx x
      pure ty

    go ctx e@IntLit{} = do
      pure IntType

    go ctx e@BoolLit{} = do
      pure BoolType

    go ctx e0@(App f args) = do
      fType <- lookupFnType (name2String f)

      let (fArgTys, fResultTy) = splitFnType fType
      argTys <- traverse (go ctx) args

      -- trace ("- e0 = " ++ ppr' e0 ++ ":  fType = " ++ show fType ++ ", fArgTys = " ++ show fArgTys ++ ", argTys = " ++ show argTys)
      eqs <- fmap concat . sequence $ zipWith (unify e0) fArgTys argTys

      getNewType (toListOf fv (fArgTys ++ [fResultTy])) eqs fResultTy -- TODO: Is this correct?

    go ctx (LayoutLambda adt bnd) = do
      (tyVar, body) <- unbind bnd
      bodyTy <- go (ctxExtendType tyVar adt ctx) body
      pure $ ForAll $ bind (tyVar, Embed adt) bodyTy

    -- TODO: Find a better way before it reaches here
    go ctx e0@(ApplyLayout (App f args) appTy)
      | isConstructor f = do
          fType <- go ctx (ApplyLayout (V f) appTy)
          let (fArgTys, fResultTy) = splitFnType fType
          argTys <- traverse (go ctx) args

          sequence $ zipWith (unify e0) fArgTys argTys

          pure fResultTy -- TODO: Is this correct?

    go ctx e0@(ApplyLayout body appTy) =
      -- | Just tyVar <- getUnifyVar appTy =
          go ctx body >>= \case
            ForAll forallBnd -> do
              ((forallV, _), forallBody) <- unbind forallBnd
              ty' <- go ctx body
              pure $ subst forallV appTy ty'
              -- trace ("forallBnd = " ++ show forallBnd ++ ", " ++ show appTy) $ pure $ instantiate forallBnd [appTy]
            ty -> checkError $ text "Expected quantified forall type, found" $$ nest 2 (ppr ty)

    go ctx (Div x y) = checkType2 ctx x IntType y IntType *> pure IntType
    go ctx (Mod x y) = checkType2 ctx x IntType y IntType *> pure IntType
    go ctx (Add x y) = checkType2 ctx x IntType y IntType *> pure IntType
    go ctx (Mul x y) = checkType2 ctx x IntType y IntType *> pure IntType
    go ctx (Sub x y) = checkType2 ctx x IntType y IntType *> pure IntType

    go ctx (And x y) = checkType2 ctx x BoolType y BoolType *> pure BoolType
    go ctx (Equal x y) = checkType2 ctx x IntType y IntType *> pure BoolType
    go ctx (Lt x y) = checkType2 ctx x IntType y IntType *> pure BoolType
    go ctx (Le x y) = checkType2 ctx x IntType y IntType *> pure BoolType
    go ctx (Not x) = checkType ctx x BoolType *> pure BoolType


checkType :: Ctx -> Expr -> Type -> Check s ()
checkType ctx e ty = do
  ty' <- inferExpr ctx e
  requireType ty ty'

checkType2 :: Ctx -> Expr -> Type -> Expr -> Type -> Check s ()
checkType2 ctx e1 ty1 e2 ty2 = do
  checkType ctx e1 ty1
  checkType ctx e2 ty2

