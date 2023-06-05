-- TODO: Consider unifying this with Pika.Stage.ToPikaCore.Monad by
-- generalizing

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module PikaC.Backend.C.Monad
  where

import qualified PikaC.Syntax.PikaCore.Expr as PikaCore
import qualified PikaC.Syntax.PikaCore.FnDef as PikaCore
import PikaC.Syntax.PikaCore.FnDef (FnDef (..), FnDefBranch (..))
import PikaC.Syntax.Heaplet
import PikaC.Syntax.Pika.Layout

import PikaC.Stage

import PikaC.Utils

import qualified PikaC.Backend.C.Syntax as C
import PikaC.Backend.C.Syntax (CName, CExpr)

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Unsafe

import Control.Monad.Reader
import Control.Monad.State

import Control.Lens
import Control.Lens.TH

import GHC.Stack

import Data.List

import Debug.Trace

newtype GenCEnv =
  GenCEnv
    { _genCAllocs :: [ModedName' AllocAnnotated (PikaCore.Expr' AllocAnnotated)]
    }

-- type instance XLayout

type instance XModed AllocAnnotated = Int
type instance PikaCore.XV AllocAnnotated = Int

type AllocExpr = PikaCore.Expr' AllocAnnotated

-- instance Alpha (Moded' AllocAnnot

annotateAllocs ::
  [ModedName' AllocAnnotated (PikaCore.Expr' AllocAnnotated)] ->
  PikaCore.Expr' PC -> PikaCore.Expr' AllocAnnotated
annotateAllocs origAllocs =
  PikaCore.bindXV convertNameExpr convertNameAsn origAllocs
  where
    convertNameExpr ::
      PikaCore.Expr -> ModedName PikaCore.Expr -> ModedName' AllocAnnotated (PikaCore.Expr' AllocAnnotated)
    convertNameExpr e v =
      case find ((== modedNameName v) . allocName) (exprOutputAllocs e) of
        Nothing -> --error $ "annotateAllocs: cannot find " ++ show v
          annotateModed (string2Name . show) (\_ -> 0) v -- TODO: Does this work?
        Just r ->
          annotateModed (string2Name . show) (\_ -> allocSize r) v

    convertNameAsn :: 
      PikaCore.ExprAssertion -> ModedName PikaCore.Expr -> ModedName' AllocAnnotated (PikaCore.Expr' AllocAnnotated)
    convertNameAsn asn v =
      let v' = modedNameName v
          sz = lookupAllocation (findAllocations [v'] asn) v'
      in
      annotateModed (string2Name . show) (\_ -> sz) v

exprOutputAllocs :: PikaCore.Expr -> [Allocation PikaCore.Expr]
exprOutputAllocs = go
  where
    go (PikaCore.WithIn _ bnd) =
      let (vars, body) = unsafeUnbind bnd
      in
      go body
    go (PikaCore.SslAssertion bnd) =
      let (vars, asn) = unsafeUnbind bnd
      in
      findAllocations (map modedNameName vars) asn
    -- go (PikaCore.App _ sz _) = sz -- TODO: Should this have a special
    -- case?
    go _ = []

makeLenses ''GenCEnv

newtype GenC a = GenC { unGenC :: ReaderT GenCEnv FreshM a }
  deriving (Functor, Applicative, Monad, MonadReader GenCEnv, Fresh)

runGenC :: GenC a -> a
runGenC = runFreshM . runGenC'

runGenC' :: GenC a -> FreshM a
runGenC' (GenC m) = runReaderT m (GenCEnv mempty)

lookupAllocM ::
  Name (PikaCore.Expr' AllocAnnotated) ->
  GenC Int
lookupAllocM v = do
  allocs <- asks _genCAllocs
  case find ((== v) . modedNameName) allocs of
    Nothing ->
      pure 0 -- TODO: Is this correct?
      -- error $ "lookupAllocM: Cannot find " ++ show v
    Just (Moded' sz _ _) -> pure sz

-- enterFnDef ::
--   (FnDef -> GenC a) ->
--   ()
-- enterFnDef = undefined

enterBranch ::
  (FnDefBranch -> GenC a) ->
  FnDefBranch -> GenC a
enterBranch f branch = do
    -- TODO: Handle the LHS's of the points-tos in fnDefBranchInputAssertions?
    -- Also outputs?
  origAllocs <- asks _genCAllocs
  let inLhsVars = map (PikaCore.getV . locBase . pointsToLhs) $ concat (_fnDefBranchInputAssertions branch)
      newAllocs = map (allocToModed In) (_fnDefBranchInAllocs branch
                  ++ findAllocations inLhsVars (concat (_fnDefBranchInputAssertions branch)))
  local (genCAllocs %~ (newAllocs ++)) $ f branch

allocToModed :: Mode -> Allocation PikaCore.Expr -> ModedName' AllocAnnotated (PikaCore.Expr' AllocAnnotated)
allocToModed mode alloc = Moded' (allocSize alloc) mode (string2Name (show (allocName alloc)))

enterBranchBody ::
  (PikaCore.Expr' AllocAnnotated -> GenC a) ->
  PikaCore.Expr -> GenC a
enterBranchBody f e = do 
  origAllocs <- asks _genCAllocs
  f $ annotateAllocs origAllocs e

