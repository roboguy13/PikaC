{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}

module PikaC.TypeChecker.Mode
  (modeCheck, ModeError)
  where

import PikaC.Syntax.Pika.Layout
import PikaC.Syntax.Heaplet
import PikaC.Ppr hiding (first)

import Control.Monad.Reader
import Control.Applicative
import Control.Monad.Morph

import Data.Bifunctor

import Data.Coerce

import Unbound.Generics.LocallyNameless

import Control.Lens

data ModeCheckEnv a =
  ModeCheckEnv
  { _mcLayoutEnv :: LayoutEnv a
  , _mcLocalModes :: ModeEnv a
  }

newtype ModeError = ModeError Doc
  deriving (Show)

instance Ppr ModeError where
  ppr (ModeError e) = e

type ModeCheckC a = (IsNested a, Subst a a, Ppr a)

modeErrorM :: Doc -> ModeCheck a r
modeErrorM = ModeCheck . lift . Left . ModeError

extendModeError :: Doc -> ModeCheck a r -> ModeCheck a r
extendModeError d = ModeCheck . hoist go . runModeCheck
  where
    go :: Either ModeError b -> Either ModeError b
    go = first (\x -> coerce (d $$ nest 2 (coerce x)))

newtype ModeCheck a r = ModeCheck { runModeCheck :: ReaderT (ModeCheckEnv a) (Either ModeError) r }
  deriving (Functor, Applicative, Monad, MonadReader (ModeCheckEnv a))

execModeCheck :: LayoutEnv a -> ModeEnv a -> ModeCheck a r -> Either ModeError r
execModeCheck layoutEnv localEnv (ModeCheck m) =
  runReaderT m (ModeCheckEnv layoutEnv localEnv)

modeCheck :: ModeCheckC a => LayoutEnv a -> Layout a -> Either ModeError ()
modeCheck layoutEnv layout =
  let localEnv = _layoutSigParams (_layoutSig layout)
      heaplets = view (layoutBranches.traversed.layoutBody.unLayoutBody) layout
  in
  execModeCheck layoutEnv localEnv $
    mapM_ modeCheckHeaplet heaplets

modeCheckHeaplet :: forall a. ModeCheckC a => LayoutHeaplet a -> ModeCheck a ()
modeCheckHeaplet h@(LPointsTo ((n :+ _) :-> _)) = do
  getModeM n >>= \case
    Out -> pure ()
    In -> modeErrorM $
            text "Variable" <+> ppr n <+> text "has input mode in left-hand side of points-to: " $$ nest 4 (ppr h)

modeCheckHeaplet h@(LApply layoutName _patVar layoutVars) = do
  layout <- asks (lookupLayout . _mcLayoutEnv) <*> pure layoutName
  zipWithM_ modeMatch' (_layoutSigParams (_layoutSig layout)) layoutVars
  where
    modeMatch' x y =
      extendModeError
        ((text "In" <+> ppr h) <> text ":")
        (modeMatch x y)

getModeM :: (Ppr (f a), Moded f) => f a -> ModeCheck a Mode
getModeM x = do
  locals <- asks _mcLocalModes
  case getMode locals x of
    Nothing ->
        modeErrorM $
          text "Cannot find mode for the variable" <+> ppr x
    Just r -> pure r

modeMatch :: (Ppr (f a), Ppr (g a), Moded f, Moded g) => f a -> g a -> ModeCheck a ()
modeMatch x y =
  liftA2 (,) (getModeM x) (getModeM y) >>= \case
    (In, Out) -> pure ()
    (Out, In) -> pure ()
    (modeX, modeY) ->
      modeErrorM $
        text "Cannot match modes:" $$
          nest 4 (text "Variable" <+> ppr x <+> text "has mode" <+> ppr modeX) $$
          nest 4 (text "Variable" <+> ppr y <+> text "has mode" <+> ppr modeY)

