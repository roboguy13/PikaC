{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module PikaC.Stage.ToPikaCore.Monad
  where

import qualified PikaC.Syntax.Pika.Expr as Pika
import qualified PikaC.Syntax.Pika.FnDef as Pika
import qualified PikaC.Syntax.PikaCore.Expr as PikaCore
import PikaC.Syntax.Pika.Layout
import PikaC.Syntax.Pika.Pattern
import PikaC.Syntax.Pika.Expr
import PikaC.Syntax.Type
import PikaC.Syntax.Heaplet
import PikaC.Utils
import PikaC.Ppr (ppr')
import PikaC.Stage.ToPikaCore.Utils (freshModed)

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Bind

import Data.List

import Control.Monad.Reader
import Control.Monad.State

import Control.Lens
import Control.Lens.TH

import GHC.Stack

import Debug.Trace

newtype LayoutVarSubst = LayoutVarSubst [(Pika.ExprName, (String, LayoutArg PikaCore.Expr))]
  deriving (Semigroup, Monoid, Show)

data PikaConvertEnv =
  PikaConvertEnv
  { _origLayoutEnv :: LayoutEnv Pika.Expr
  , _layoutEnv :: LayoutEnv PikaCore.Expr
  , _globalFns :: [Pika.FnDef]
  -- , _layoutVarSubst :: LayoutVarSubst
  }

newtype PikaConvertState =
  PikaConvertState
  { _pikaToPcExprNameMap :: [(Pika.ExprName, PikaCore.ExprName)] -- TODO: Should we just use LayoutVarSubst instead of this? Probably not, since this could have local variables other than pattern variables.
  }

makeLenses ''PikaConvertState

newtype PikaIntern m a = PikaIntern { getPikaIntern :: StateT PikaConvertState (FreshMT m) a }
  deriving (Functor, Applicative, Monad, MonadState PikaConvertState, Fresh)

runPikaIntern' :: Monad m => PikaIntern m a -> FreshMT m a
runPikaIntern' (PikaIntern m) =
  evalStateT m (PikaConvertState mempty)

type MonadPikaIntern m = (Fresh m, MonadState PikaConvertState m)

newtype PikaConvert m a = PikaConvert (ReaderT PikaConvertEnv (PikaIntern m) a)
  deriving (Functor, Applicative, Monad, MonadState PikaConvertState, MonadReader PikaConvertEnv, Fresh)

instance Monad m => MonadFail (PikaConvert m) where
  fail = error

piLift :: Monad m => FreshMT m a -> PikaIntern m a
piLift = PikaIntern . lift

pcLift :: Monad m => FreshMT m a -> PikaConvert m a
pcLift = PikaConvert . lift . piLift

runPikaConvert :: Monad m => LayoutEnv Pika.Expr -> LayoutEnv PikaCore.Expr -> [Pika.FnDef] -> PikaConvert m a -> m a
runPikaConvert origLayouts layouts globalFns =
  runFreshMT . runPikaConvert' origLayouts layouts globalFns

runPikaConvert' :: Monad m => LayoutEnv Pika.Expr -> LayoutEnv PikaCore.Expr -> [Pika.FnDef] -> PikaConvert m a -> FreshMT m a
runPikaConvert' origLayouts layouts globalFns =
  runPikaIntern' . runPikaConvert'' origLayouts layouts globalFns

runPikaConvert'' :: LayoutEnv Pika.Expr -> LayoutEnv PikaCore.Expr -> [Pika.FnDef] -> PikaConvert m a -> PikaIntern m a
runPikaConvert'' origLayouts layouts globalFns (PikaConvert m) = runReaderT m (PikaConvertEnv origLayouts layouts globalFns)

lookupLayoutM :: Monad m => String -> PikaConvert m (Layout PikaCore.Expr)
lookupLayoutM layoutName = do
  layouts <- asks _layoutEnv
  pure $ lookupLayout layouts layoutName

lookupFnDef :: (Monad m, HasCallStack) => String -> PikaConvert m Pika.FnDef
lookupFnDef name = go <$> asks _globalFns
  where
    go [] = error $ "lookupFnDef: Cannot find function " ++ name
    go (x:xs)
      | Pika.fnDefName x == name = x
      | otherwise = go xs

lookupFnTypeSig :: (Monad m, HasCallStack) => String -> PikaConvert m TypeSig
lookupFnTypeSig name = Pika.fnDefTypeSig <$> lookupFnDef name

lookupFnResultType :: (Monad m, HasCallStack) => String -> PikaConvert m Type
lookupFnResultType fnName = do
  sig <- lookupFnTypeSig fnName
  case _typeSigLayoutConstraints sig of
    (_:_) -> error $ "lookupFnResultType: Function " ++ fnName ++ " should not be layout polymorphic. Found type " ++ ppr' sig
    [] ->
      let (_, r) = splitFnType (_typeSigTy sig)
      in
      pure r

-- -- | Construct an ADT value using a pre-specified layout
-- newtype Construct a =
--   Construct
--     (HasCallStack =>
--           String ->   -- Constructor name
--           [Name a] -> -- Location parameters
--           [LayoutHeaplet a])

-- -- | Build a name mapping from an LApply given the global environment of layouts
-- makeConstruct :: forall a. HasApp a => [Layout a] -> (String, a, [Name a]) -> Construct a
-- makeConstruct layouts (layoutName, patVar, locVars) =
--   let layout = lookupLayout layouts layoutName
--   in
--   Construct $ \constructor newLocNames ->
--     let branch = lookupLayoutBranch' layout constructor
--     in
--     undefined

-- vsubstLookupM :: HasCallStack => Pika.ExprName -> PikaConvert (String, LayoutArg PikaCore.Expr)
-- vsubstLookupM e = do
--   LayoutVarSubst assocs <- asks _layoutVarSubst
--   case lookup e assocs of
--     Nothing -> error $ "vsubstLookupM: Cannot find variable " ++ show e ++ " in variable -> layout variable list conversion table"
--     Just r -> pure r

scoped :: MonadPikaIntern m => m a -> m a
scoped m = do
  state <- get
  r <- m
  put state
  pure r

-- TODO: Does this obey scope properly? Should we be using 'scoped' with
-- this function?
internExprName :: forall m. MonadPikaIntern m => Pika.ExprName -> m PikaCore.ExprName
internExprName n = pure (string2Name (name2String n))
-- internExprName n = pure (string2Name (name2String n))
  -- do
  --   assocs <- gets _pikaToPcExprNameMap
  --   case lookup n assocs of
  --     Just n' -> pure n'
  --     Nothing -> do
  --       n' <- fresh (string2Name (name2String n))
  --       pikaToPcExprNameMap %= ((n, n') :)
  --       pure n'

internExists :: forall m. MonadPikaIntern m => Exists Pika.Expr -> m (Exists PikaCore.Expr)
internExists (Exists n) = Exists <$> internModedExprName n

internModedExprName :: MonadPikaIntern m => ModedName Pika.Expr -> m (ModedName PikaCore.Expr)
internModedExprName (Moded mode v) =
  Moded mode <$> internExprName v

freshLayoutParams :: Fresh m => Layout a -> m [ModedName a]
freshLayoutParams layout =
  let B vs _ = _layoutBranches layout
  in
  mapM freshModed vs

-- applyLayout'' :: Layout PikaCore.Expr -> String -> [PType PikaCore.Expr] -> LayoutBody PikaCore.Expr
-- applyLayout'' layout c args =
--   let B vs _ = _layoutBranches layout
--   in
--   case applyLayout layout params c args of
--     Nothing -> error $ "applyLayout': Cannot find branch for constructor " ++ c ++ " in " ++ show layout
--     Just r -> r

type Bound = Bind [ModedName PikaCore.Expr]


-- -- | Freshen the layout parameters
-- freshSslAssertion :: Fresh m => LayoutArg PikaCore.Expr -> PikaCore.ExprAssertion -> m PikaCore.Expr
-- freshSslAssertion params body = do
--   params' <- mapM fresh params
--   let body' = go (zip params params') body
--
--   pure $ PikaCore.SslAssertion params' body'
--   where
--     go [] z = z
--     go ((p, p'):xs) z =
--       rename [(p, p')] (go xs z)

-- type Rename a = [(a, a)]

-- -- | For freshening layout pattern variables in function definitions
-- layoutParamRename :: forall m. Fresh m => Layout PikaCore.Expr -> m (Rename PikaCore.ExprName)
-- layoutParamRename layout =
--     let params = map modedNameName (_layoutSigParams (_layoutSig layout))
--                   `union` foldr (union . layoutBranchFVs) [] (_layoutBranches layout)
--     in
--     mapM go params
--   where
--     go :: PikaCore.ExprName -> m (PikaCore.ExprName, PikaCore.ExprName)
--     go x = do
--       x' <- fresh x
--       pure (x, x')

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

