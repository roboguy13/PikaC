-- TODO: Consider unifying this with Pika.Stage.ToPikaCore.Monad by
-- generalizing

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module PikaC.Backend.C.Monad
  where

import qualified PikaC.Syntax.PikaCore.Expr as PikaCore
import PikaC.Syntax.Heaplet

import PikaC.Utils

import qualified PikaC.Backend.C.Syntax as C
import PikaC.Backend.C.Syntax (CName, CExpr)

import Unbound.Generics.LocallyNameless

import Control.Monad.State

import Control.Lens
import Control.Lens.TH

import GHC.Stack

import Debug.Trace

newtype GenCState =
  GenCState
    { _namePcToC :: [(PikaCore.ExprName, CName)]
    }

makeLenses ''GenCState

newtype GenC a = GenC { unGenC :: StateT GenCState FreshM a }
  deriving (Functor, Applicative, Monad, MonadState GenCState, Fresh)

runGenC :: GenC a -> a
runGenC = runFreshM . runGenC'

runGenC' :: GenC a -> FreshM a
runGenC' (GenC m) = evalStateT m (GenCState mempty)

scoped :: MonadState GenCState m => m a -> m a
scoped m = do
  state <- get
  r <- m
  put state
  pure r

-- TODO: Does this obey scope properly?
internExprName :: HasCallStack => PikaCore.ExprName -> GenC CName
internExprName n =
  if not (isFreeName n)
    then error "internExprName: Bound variable"
    else do
      assocs <- gets _namePcToC
      case lookup n assocs of
        Just n' -> pure n'
        Nothing -> do
          n' <- fresh (string2Name (name2String n))
          namePcToC %= ((n, n') :)
          pure n'

