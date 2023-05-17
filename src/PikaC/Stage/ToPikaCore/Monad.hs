{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

module PikaC.Stage.ToPikaCore.Monad
  where

import qualified PikaC.Syntax.Pika.Expr as Pika
import qualified PikaC.Syntax.PikaCore.Expr as PikaCore
import PikaC.Syntax.Pika.Layout
import PikaC.Syntax.Heaplet

import Unbound.Generics.LocallyNameless

import Control.Monad.Reader
import Control.Monad.State

import GHC.Stack

newtype LayoutVarSubst = LayoutVarSubst [(Pika.ExprName, (String, LayoutArg))]
  deriving (Semigroup, Monoid)

type LayoutEnv = [Layout Pika.Expr]

data PikaConvertEnv =
  PikaConvertEnv
  { _layoutEnv :: LayoutEnv
  , _layoutVarSubst :: LayoutVarSubst
  }

data PikaConvertState =
  PikaConvertState
  { _pikaToPcExprNameMap :: [(Pika.ExprName, PikaCore.ExprName)]
  , _pikaToPcLocNameMap :: [(Pika.ExprName, LocName)]
  }

newtype PikaIntern a = PikaIntern { getPikaIntern :: StateT PikaConvertState FreshM a }
  deriving (Functor, Applicative, Monad, MonadState PikaConvertState, Fresh)

runPikaIntern' :: PikaIntern a -> FreshM a
runPikaIntern' (PikaIntern m) =
  evalStateT m (PikaConvertState mempty mempty)

type MonadPikaIntern m = (Fresh m, MonadState PikaConvertState m)

newtype PikaConvert a = PikaConvert (ReaderT PikaConvertEnv PikaIntern a)
  deriving (Functor, Applicative, Monad, MonadState PikaConvertState, MonadReader PikaConvertEnv, Fresh)

runPikaConvert :: LayoutVarSubst -> LayoutEnv -> PikaConvert a -> a
runPikaConvert vsubst layouts =
  runFreshM . runPikaConvert' vsubst layouts

runPikaConvert' :: LayoutVarSubst -> LayoutEnv -> PikaConvert a -> FreshM a
runPikaConvert' vsubst layouts =
  runPikaIntern' . runPikaConvert'' vsubst layouts

runPikaConvert'' :: LayoutVarSubst -> LayoutEnv -> PikaConvert a -> PikaIntern a
runPikaConvert'' vsubst layouts (PikaConvert m) = runReaderT m (PikaConvertEnv layouts vsubst)

lookupLayoutM :: String -> PikaConvert (Layout Pika.Expr)
lookupLayoutM layoutName = do
  layouts <- asks _layoutEnv
  pure $ lookupLayout layouts layoutName

vsubstLookupM :: HasCallStack => Pika.ExprName -> PikaConvert (String, LayoutArg)
vsubstLookupM e = do
  LayoutVarSubst assocs <- asks _layoutVarSubst
  case lookup e assocs of
    Nothing -> error $ "vsubstLookupM: Cannot find variable " ++ show e ++ " in variable -> layout variable list conversion table"
    Just r -> pure r

internExprName :: forall m. MonadPikaIntern m => Pika.ExprName -> m PikaCore.ExprName
internExprName n = do
  assocs <- gets _pikaToPcExprNameMap
  case lookup n assocs of
    Just n' -> pure n'
    Nothing -> fresh (string2Name (name2String n))

internLocName :: forall m. MonadPikaIntern m => Pika.ExprName -> m LocName
internLocName n = do
  assocs <- gets _pikaToPcLocNameMap
  case lookup n assocs of
    Just n' -> pure n'
    Nothing -> fresh (string2Name (name2String n))

