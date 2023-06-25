{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module PikaC.TypeChecker.Monad
  where

import Unbound.Generics.LocallyNameless
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens

import Data.List
import Data.Bifunctor

import Control.Monad.Morph

import PikaC.Ppr

import PikaC.Syntax.Type
import PikaC.Syntax.Pika.Expr
import PikaC.Syntax.Pika.Layout

newtype Check s a = Check (FreshMT (ReaderT CheckEnv (Either String)) a)
  deriving (Functor, Applicative, Monad, MonadReader CheckEnv, Fresh)

data CheckEnv =
    CheckEnv
      { _localEnv :: [(ExprName, Type)]
      , _fnEnv :: [(String, TypeSig)]
      , _layoutAdts :: [(LayoutName, AdtName)]
      , _layoutConstraints :: [LayoutConstraint]
      , _constructorTypes :: [(String, Type)]
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


requireType :: Type -> Type -> Check s ()
requireType ty1 ty2
  | aeq ty1 ty2 = pure ()
  | otherwise = checkError $ text "Expected type" $$ nest 2 (ppr ty1) $$ text "Found type" $$ nest 2 (ppr ty2)

