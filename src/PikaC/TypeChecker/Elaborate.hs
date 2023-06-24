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

import PikaC.Utils
import PikaC.Ppr

import Control.Monad.State
import Control.Monad.Reader

import Control.Lens

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

newtype Check s a = Check (FreshMT (ReaderT CheckEnv (Either String)) a)
  deriving (Functor, Applicative, Monad, MonadReader CheckEnv, Fresh)

data CheckEnv =
    CheckEnv
      { _localEnv :: [(ExprName, Type)]
      , _fnEnv :: [(String, TypeSig)]
      , _layoutAdts :: [(LayoutName, AdtName)]
      , _layoutConstraints :: [LayoutConstraint]
      , _typeEnv :: [(TypeName, Type)]
      , _typeEqs :: [(TypeName, TypeName)]
      }
  deriving (Show)

makeLenses ''CheckEnv

lookupConstraint :: TypeName -> Check s AdtName
lookupConstraint v =
  fmap (find go) (asks _layoutConstraints) >>= \case
    Nothing -> checkError $ text "Could not find constraint for type variable" <+> ppr v
    Just (_ :~ t) -> pure t
  where
    go (x :~ _) = x == v

-- consistencyCheck :: forall s. Check s ()
-- consistencyCheck = mapM_ go =<< values
--   where
--     getAdts :: [TypeName] -> Check s [(TypeName, AdtName)]
--     getAdts xs =
--       catMaybes . map strength . zip xs <$> mapM lookupConstraint xs
--
--     go :: TypeName -> Check s ()
--     go t = do
--       ts <- classDesc t
--       consistentConstraints =<< getAdts ts
--
-- consistentConstraints :: [(TypeName, AdtName)] -> Check s ()
-- consistentConstraints [] = pure ()
-- consistentConstraints ((n, adt) : rest) = mapM_ go rest
--   where
--     go (m, adt')
--       | adt' /= adt = checkError $ text "Type variable" $$ nest 2 (ppr m) $$ text "is a layout for type" $$ nest 2 (ppr adt') $$ text "but it is expected to be a layout for type" $$ nest 2 (ppr adt) $$ text "since it is unified with variable" $$ ppr n
--       | otherwise = pure ()

checkEnvLookupLocal :: ExprName -> CheckEnv -> Maybe Type
checkEnvLookupLocal v = lookup v . _localEnv

checkEnvExtendLocal :: ExprName -> Type -> CheckEnv -> CheckEnv
checkEnvExtendLocal v ty = localEnv %~ ((v, ty) :)

checkEnvLookupFn :: String -> CheckEnv -> Maybe TypeSig
checkEnvLookupFn v = lookup v . _fnEnv

checkError :: Doc -> Check s a
checkError = Check . lift . lift . Left . render

-- freshUnifierVar :: TypeName -> Check s TypeName
-- freshUnifierVar a = do
--   new <- fresh a
--   typeEqs %= ((new, a) :)
--   pure new

lookupLocalType :: ExprName -> Check s Type
lookupLocalType v =
  asks (checkEnvLookupLocal v) >>= \case
    Nothing -> checkError $ text "Cannot find type for variable" <+> ppr v
    Just ty -> pure ty

lookupFnType :: String -> Check s TypeSig
lookupFnType f =
  asks (checkEnvLookupFn f) >>= \case
    Nothing -> checkError $ text "Cannot find function" <+> text f
    Just sig -> pure sig

requireType :: Type -> Type -> Check s ()
requireType ty1 ty2
  | aeq ty1 ty2 = pure ()
  | otherwise = checkError $ text "Expected type" $$ nest 2 (ppr ty1) $$ text "Found type" $$ nest 2 (ppr ty2)

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

checkExpr :: Expr -> Check s Type
checkExpr = \case
  V x -> lookupLocalType x
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
      then pure $ substBind bnd undefined
      else
        checkError $ text "When checking the type of" $$ nest 2 (ppr e) $$ text "cannot match expected layout" $$ nest 2 (ppr tyAdt) $$ text "with actual layout" $$ nest 2 (ppr eAdt)

  App f xs -> undefined

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
