{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

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

instance MonadFail (Check s) where
  fail s = error $ "MonadFail (Check s): " ++ s

data CheckEnv =
    CheckEnv
      { _fnEnv :: [(String, TypeSig)]
      , _layoutAdts :: [(LayoutName, AdtName)]
      , _constructorTypes :: [(String, Type)]
      }
  deriving (Show)

makeLenses ''CheckEnv

runCheck :: CheckEnv -> (forall s. Check s a) -> Either String a
runCheck env (Check m) = runReaderT (runFreshMT m) env

checkEnvLookupFn :: String -> CheckEnv -> Maybe TypeSig
checkEnvLookupFn v = lookup v . _fnEnv

checkError :: Doc -> Check s a
checkError = Check . lift . lift . Left . render

requireType :: Type -> Type -> Check s ()
requireType expected found
  | aeq expected found = pure ()
  | otherwise = checkError $ text "Expected type" $$ nest 2 (ppr expected) $$ text "Found type" $$ nest 2 (ppr found)

