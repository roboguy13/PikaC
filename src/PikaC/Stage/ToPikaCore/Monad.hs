{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module PikaC.Stage.ToPikaCore.Monad
  where

import qualified PikaC.Syntax.Pika.Expr as Pika
import qualified PikaC.Syntax.PikaCore.Expr as PikaCore
import PikaC.Syntax.Pika.Layout
import PikaC.Syntax.Heaplet
import PikaC.Utils

import Unbound.Generics.LocallyNameless

import Control.Monad.Reader
import Control.Monad.State

import Control.Lens
import Control.Lens.TH

import GHC.Stack

import Debug.Trace

newtype LayoutVarSubst = LayoutVarSubst [(Pika.ExprName, (String, LayoutArg PikaCore.Expr))]
  deriving (Semigroup, Monoid, Show)

type LayoutEnv = [Layout PikaCore.Expr]

data PikaConvertEnv =
  PikaConvertEnv
  { _layoutEnv :: LayoutEnv
  , _layoutVarSubst :: LayoutVarSubst
  }

newtype PikaConvertState =
  PikaConvertState
  { _pikaToPcExprNameMap :: [(Pika.ExprName, PikaCore.ExprName)] -- TODO: Should we just use LayoutVarSubst instead of this? Probably not, since this could have local variables other than pattern variables.
  }

makeLenses ''PikaConvertState

newtype PikaIntern a = PikaIntern { getPikaIntern :: StateT PikaConvertState FreshM a }
  deriving (Functor, Applicative, Monad, MonadState PikaConvertState, Fresh)

runPikaIntern' :: PikaIntern a -> FreshM a
runPikaIntern' (PikaIntern m) =
  evalStateT m (PikaConvertState mempty)

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

lookupLayoutM :: String -> PikaConvert (Layout PikaCore.Expr)
lookupLayoutM layoutName = do
  layouts <- asks _layoutEnv
  pure $ lookupLayout layouts layoutName

vsubstLookupM :: HasCallStack => Pika.ExprName -> PikaConvert (String, LayoutArg PikaCore.Expr)
vsubstLookupM e = do
  LayoutVarSubst assocs <- asks _layoutVarSubst
  case lookup e assocs of
    Nothing -> error $ "vsubstLookupM: Cannot find variable " ++ show e ++ " in variable -> layout variable list conversion table"
    Just r -> pure r

scoped :: MonadPikaIntern m => m a -> m a
scoped m = do
  state <- get
  r <- m
  put state
  pure r

-- TODO: Does this obey scope properly? Should we be using 'scoped' with
-- this function?
internExprName :: forall m. MonadPikaIntern m => Pika.ExprName -> m PikaCore.ExprName
internExprName n = do
  assocs <- gets _pikaToPcExprNameMap
  case lookup n assocs of
    Just n' -> pure n'
    Nothing -> do
      n' <- fresh (string2Name (name2String n))
      pikaToPcExprNameMap %= ((n, n') :)
      pure n'

-- | Freshen the layout parameters
freshSslAssertion :: Fresh m => LayoutArg PikaCore.Expr -> PikaCore.ExprAssertion -> m PikaCore.Expr
freshSslAssertion params body = do
  params' <- mapM fresh params
  let body' = go (zip params params') body

  pure $ PikaCore.SslAssertion params' body'
  where
    go [] z = z
    go ((p, p'):xs) z =
      rename [(p, p')] (go xs z)

type Rename a = [(a, a)]

-- | For freshening layout pattern variables in function definitions
layoutParamRename :: forall m. Fresh m => Layout PikaCore.Expr -> m (Rename PikaCore.ExprName)
layoutParamRename layout =
    let params = _layoutSigParams (_layoutSig layout)
    in
    mapM go params
  where
    go :: PikaCore.ExprName -> m (PikaCore.ExprName, PikaCore.ExprName)
    go x = do
      x' <- fresh x
      pure (x, x')

-- unintern :: forall m. MonadPikaIntern m => PikaCore.ExprName -> m Pika.ExprName
-- unintern n =
--   fmap (lookup n . map swap) (gets _pikaToPcExprNameMap) >>= \case
--     Nothing -> error $ "unintern: The name " ++ show n ++ " was not interned"
--     Just r -> pure r
--   where
--     swap (a, b) = (b, a)

-- internLocName :: forall m. MonadPikaIntern m => Pika.ExprName -> m PikaCore.ExprName
-- internLocName n = do
--   assocs <- gets _pikaToPcLocNameMap
--   case lookup n assocs of
--     Just n' -> pure n'
--     Nothing -> fresh (string2Name (name2String n))

