{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PikaC.TypeChecker.Elaborate
  where

import PikaC.Syntax.Type
import PikaC.Syntax.Pika.Expr
import PikaC.Syntax.Pika.FnDef
import PikaC.Syntax.Pika.Layout (LayoutName)

import PikaC.TypeChecker.Monad
import PikaC.TypeChecker.Unify

import PikaC.Utils
import PikaC.Ppr

import Control.Monad.State
import Control.Monad.Reader

import Control.Lens

import Control.Monad
import Control.Applicative

import Data.List
import Data.Maybe

import Unbound.Generics.LocallyNameless

-- import Data.Equivalence.Monad
-- import Data.Equivalence.STT (Class)

-- newtype ElaborateEnv = [(TypeName

-- newtype ElaborateM a = ElaborateM (State

-- data Desc = ConcreteLayout LayoutName | UnifyVar TypeName
--   deriving (Show)

-- type Desc = [TypeName]

-- combineDesc :: Desc -> Desc -> Either Doc Desc
-- combineDesc (ConcreteLayout x) (UnifyVar _) = Right $ ConcreteLayout x
-- -- combineDesc (

-- deriving instance MonadEquiv (Class s d v) v d m =>
--   MonadEquiv (Class s d v) v d (FreshMT m)


-- freshUnifierVar :: TypeName -> Check s TypeName
-- freshUnifierVar a = do
--   new <- fresh a
--   typeEqs %= ((new, a) :)
--   pure new

lookupLocalType :: ExprName -> Check s (Maybe Type)
lookupLocalType v =
  asks (checkEnvLookupLocal v) >>= \case
    Nothing -> pure Nothing --checkError $ text "Cannot find type for variable" <+> ppr v
    Just ty -> pure $ Just ty

lookupFnType :: String -> Check s (Maybe Type)
lookupFnType f =
  asks (checkEnvLookupFn f) >>= \case
    Nothing -> pure Nothing --checkError $ text "Cannot find function" <+> text f
    Just sig -> pure $ Just $ fromTypeSig sig

lookupType :: ExprName -> Check s Type
lookupType x =
  liftA2 (<|>)
    (lookupLocalType x)
    (lookupFnType (name2String x)) >>= \case
      Nothing -> checkError $ text "Cannot find type for name" <+> ppr x
      Just ty -> pure ty

requireCheckType :: Type -> Expr -> Check s ()
requireCheckType ty e = do
  ty' <- checkExpr e
  requireType ty ty'

requireCheckType2 :: Type -> Expr -> Expr -> Check s ()
requireCheckType2 ty x y = do
  requireCheckType ty x
  requireCheckType ty y

checkExprSig :: TypeSig -> Expr -> Check s ()
checkExprSig = undefined

unify :: Type -> Type -> Check s Type
unify = undefined

-- checkAndElaborate = undefined

checkAndRequire :: Expr -> Type -> Check s Type
checkAndRequire e ty = do
  ty' <- checkExpr e
  if not (aeq ty' ty)
    then checkError $ text "Cannot match expected type" $$ nest 2 (ppr ty) $$ text "with actual type" $$ nest 2 (ppr ty') $$ text "in expression" $$ ppr e
    else pure ty'

checkExpr :: Expr -> Check s Type
checkExpr = \case
  V x -> lookupType x
  IntLit {} -> pure IntType
  BoolLit {} -> pure BoolType

  LayoutLambda adt bnd -> do
    (v, body) <- unbind bnd

    bodyTy <- local (layoutConstraints %~ ((v :~ adt) :))
      $ checkExpr body
    pure $ ForAll $ bind (v, Embed adt) bodyTy

  ApplyLayout e ty -> do
    tyAdt <- case ty of
        TyVar v -> lookupConstraint v
        LayoutId layoutName ->
          fmap (lookup layoutName) (asks _layoutAdts) >>= \case
            Nothing -> checkError $ text "Cannot find ADT for layout" <+> ppr layoutName
            Just adt -> pure adt
    (v, eAdt, bnd) <- checkExpr e >>= \case
      ForAll bnd -> do
        ((v, Embed adt), resultTy) <- unbind bnd
        pure (v, adt, bind v resultTy)
    --   TyVar v -> lookupConstraint v
    --   LayoutId layoutName ->
    --       fmap (lookup layoutName) (asks _layoutAdts) >>= \case
    --         Nothing -> checkError $ text "Cannot find ADT for layout" <+> ppr layoutName
    --         Just adt -> pure adt
    if eAdt == tyAdt
      then pure $ substBind bnd ty
      else
        checkError $ text "When checking the type of" $$ nest 2 (ppr e) $$ text "cannot match expected layout" $$ nest 2 (ppr tyAdt) $$ text "with actual layout" $$ nest 2 (ppr eAdt)

  App f xs -> do
    (argTys, resultTy) <- splitFnType <$> checkExpr (V (string2Name f))
    zipWithM checkAndRequire xs argTys
    pure resultTy

  Div x y -> requireCheckType2 IntType x y *> pure IntType
  Mod x y -> requireCheckType2 IntType x y *> pure IntType
  Add x y -> requireCheckType2 IntType x y *> pure IntType
  Mul x y -> requireCheckType2 IntType x y *> pure IntType
  Sub x y -> requireCheckType2 IntType x y *> pure IntType

  And x y -> requireCheckType2 BoolType x y *> pure BoolType

  Equal x y -> requireCheckType2 BoolType x y *> pure BoolType
  Le x y -> requireCheckType2 BoolType x y *> pure BoolType
  Lt x y -> requireCheckType2 BoolType x y *> pure BoolType

  Not x -> requireCheckType BoolType x *> pure BoolType
  
getAdt :: ExprName -> Check s AdtName
getAdt x
  | isConstructor (name2String x) = undefined

elaborateExpr :: Type -> Expr -> Check s Expr
elaborateExpr = undefined

-- checkExpr :: Type -> Expr -> Check Expr
-- checkExpr ty = rewriteM go
--   where
--     go :: Expr -> Check (Maybe Expr)
--     go (V x) = do
--       ty' <- lookupLocalType x
--       requireType ty ty'
--       pure Nothing
--
--     go (IntLit {}) = requireType ty IntType *> pure Nothing
--     go (BoolLit {}) = requireType ty BoolType *> pure Nothing
--
--     -- go (App f xs) = 
--
--     go (Div x y) = requireType ty IntType *> pure Nothing
--     go (Mod x y) = requireType ty IntType *> pure Nothing
--     go (Add x y) = requireType ty IntType *> pure Nothing
--     go (Mul x y) = requireType ty IntType *> pure Nothing
--     go (Sub x y) = requireType ty IntType *> pure Nothing
--
--     go (And x y) = requireType ty BoolType *> pure Nothing
--
--     go (Equal x y) = requireType ty BoolType *> pure Nothing
--     go (Le x y) = requireType ty BoolType *> pure Nothing
--     go (Lt x y) = requireType ty BoolType *> pure Nothing
--
--     go (Not x) = requireType ty BoolType *> pure Nothing
--
-- -- checkBin :: Type -> Expr -> Expr -> Check (Maybe Expr)
-- -- checkBin ty a b = do
-- --   checkExpr ty a
-- --   checkExpr ty b
-- --   pure ()
--
-- -- elaborateExpr :: Expr -> Either String Expr
-- -- elaborateExpr = go
-- --   where
-- --     go (V x) = Right $ V x
-- --     go (IntLit i) = Right $ IntLit i
--
