{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}

module PikaC.TypeChecker.Mode
  (modeCheck, ModeError)
  where

import PikaC.Syntax.Pika.Layout
import PikaC.Syntax.Heaplet
import PikaC.Syntax.Pika.Pattern
import PikaC.Ppr hiding (first)
import PikaC.Utils

import Control.Monad.Reader
import Control.Applicative
import Control.Monad.Morph
import Control.Monad

import Data.Bifunctor

import Data.Coerce

import Data.Typeable

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Bind
import Unbound.Generics.LocallyNameless.Unsafe

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

modeCheck :: forall a. (IsName a a, Subst a (LayoutBranch a), ModeCheckC a, Subst a (LayoutBody a), Subst a (ModedName a), Typeable a, Alpha a, HasVar a) => LayoutEnv a -> Layout a -> Either ModeError ()
modeCheck layoutEnv layout = runFreshM $ do
  let (_, bnd) = unsafeUnbind (_layoutBranches layout)
      openedBranches = openBind bnd
      xs = map (getPatternMatch . _layoutMatch) openedBranches

      openIt (B pat body) = openBind (B (getPatternNames pat) body)

      openedXs = map openIt xs

      openExists = map openBind openedXs

  let localEnv0 = getBv bnd -- $ _layoutBranches layout
  let localEnv :: [ModedName a]
      localEnv = localEnv0 <> map getExists (concatMap (getBv) openedXs)

  -- openedBranches <- mapM (fmap snd . openPatternMatch . _layoutMatch) branches
  -- openExists <- mapM (fmap snd . unbind) openedBranches

  let heaplets = concatMap (_unLayoutBody . _ghostCondBody) openExists
  -- let localEnv = getLayoutParams layout
  --     heaplets = view (layoutBranches.traversed.unLayoutBody) layout
  pure . execModeCheck layoutEnv localEnv $
    mapM_ modeCheckHeaplet heaplets

modeCheckHeaplet :: forall a. (Alpha a, Typeable a, IsName a a, ModeCheckC a) => LayoutHeaplet a -> ModeCheck a ()
modeCheckHeaplet h@(LPointsTo ((n :+ _) :-> _)) = do
  getModeM (getName n) >>= \case
    Out -> pure ()
    In -> modeErrorM $
            text "Variable" <+> ppr n <+> text "has input mode in left-hand side of points-to: " $$ nest 4 (ppr h)

modeCheckHeaplet h@(LApply layoutName _patVar layoutVars) = do
  layout <- asks (lookupLayout . _mcLayoutEnv) <*> pure layoutName
  zipWithM_ modeMatch' (getLayoutParams layout) (map getName layoutVars)
  where
    modeMatch' x y =
      extendModeError
        ((text "In" <+> ppr h) <> text ":")
        (modeMatch x y)

class GetModeM a b | a -> b where
  getModeM :: a -> ModeCheck b Mode

instance GetModeM (Name a) a where
  getModeM x = do
    locals <- asks _mcLocalModes
    case lookupMode locals x of
      Nothing ->
          modeErrorM $
            text "Cannot find mode for the variable" <+> ppr x <+> text "in" <+> text (show locals)
      Just r -> pure r

instance GetModeM (Moded (Name a)) a where
  getModeM (Moded m _) = pure m

modeMatch :: (Ppr a, Ppr b, GetModeM a c, GetModeM b c) => a -> b -> ModeCheck c ()
modeMatch x y =
  liftA2 (,) (getModeM x) (getModeM y) >>= \case
    (In, Out) -> pure ()
    (Out, In) -> pure ()
    (modeX, modeY) ->
      modeErrorM $
        text "Cannot match modes:" $$
          nest 4 (text "Variable" <+> ppr x <+> text "has mode" <+> ppr modeX) $$
          nest 4 (text "Variable" <+> ppr y <+> text "has mode" <+> ppr modeY)

