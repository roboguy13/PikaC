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

-- convertName :: Name (PikaCore.Expr' s) -> C.CName
convertName :: Name a -> Name b
-- convertName = string2Name . name2String
convertName = string2Name . show

-- newtype GenCEnv =
--   GenCEnv
--     { _genCAllocs :: [ModedName' AllocAnnotated (PikaCore.Expr' AllocAnnotated)]
--     }

-- type instance XLayout

type instance XModed AllocAnnotated = Int
type instance PikaCore.XV AllocAnnotated = Int

type AllocExpr = PikaCore.Expr' AllocAnnotated

-- instance Alpha (Moded' AllocAnnot

annotateAllocs ::
  -- [ModedName' AllocAnnotated (PikaCore.Expr' AllocAnnotated)] ->
  PikaCore.Expr' PC -> FreshM (PikaCore.Expr' AllocAnnotated)
annotateAllocs =
  PikaCore.bindXV convertNameExpr convertNameAsn []
  where
    convertNameExpr ::
      PikaCore.Expr -> ModedName PikaCore.Expr -> FreshM (ModedName' AllocAnnotated (PikaCore.Expr' AllocAnnotated))
    convertNameExpr e v = do
      outAllocs <- exprOutputAllocs e
      case find ((== modedNameName v) . allocName) outAllocs of
        Nothing -> --error $ "annotateAllocs: cannot find " ++ show v
          pure $ annotateModed convertName (\_ -> 0) v -- TODO: Does this work?
        Just r ->
          pure $ annotateModed convertName (\_ -> allocSize r) v

    convertNameAsn :: 
      PikaCore.ExprAssertion -> ModedName PikaCore.Expr -> ModedName' AllocAnnotated (PikaCore.Expr' AllocAnnotated)
    convertNameAsn asn v =
      let v' = modedNameName v
          sz = lookupAllocation (findAllocations [v'] asn) v'
      in
      annotateModed convertName (\_ -> sz) v

exprOutputAllocs :: PikaCore.Expr -> FreshM [Allocation PikaCore.Expr]
exprOutputAllocs = go
  where
    go (PikaCore.WithIn _ bnd) = do
      (vars, body) <- unbind bnd
      -- let (vars, body) = unsafeUnbind bnd
      -- in
      go body
    go (PikaCore.SslAssertion bnd) = do
      (vars, asn) <- unbind bnd
      -- let (vars, asn) = unsafeUnbind bnd
      -- in
      pure $ findAllocations (map modedNameName vars) asn
    -- go (PikaCore.App _ sz _) = sz -- TODO: Should this have a special
    -- case?
    go _ = pure []

-- makeLenses ''GenCEnv

-- newtype GenC a = GenC { unGenC :: ReaderT GenCEnv FreshM a }
newtype GenC a = GenC { unGenC :: FreshM a }
  deriving (Functor, Applicative, Monad, Fresh)

runGenC :: GenC a -> a
runGenC = runFreshM . runGenC'

runGenC' :: GenC a -> FreshM a
runGenC' (GenC m) = m

-- lookupAllocM ::
--   Name (PikaCore.Expr' AllocAnnotated) ->
--   GenC Int
-- lookupAllocM v = do
--   allocs <- asks _genCAllocs
--   case find ((== v) . modedNameName) allocs of
--     Nothing ->
--       pure 0 -- TODO: Is this correct?
--       -- error $ "lookupAllocM: Cannot find " ++ show v
--     Just (Moded' sz _ _) -> pure sz

-- enterFnDef ::
--   (FnDef -> GenC a) ->
--   ()
-- enterFnDef = undefined

enterBranch ::
  (FnDefBranch -> GenC a) ->
  FnDefBranch -> GenC a
enterBranch f branch = f branch --do
  --   -- TODO: Handle the LHS's of the points-tos in fnDefBranchInputAssertions?
  --   -- Also outputs?
  -- origAllocs <- asks _genCAllocs
  -- let inLhsVars = map (PikaCore.getV . locBase . pointsToLhs) $ concat (_fnDefBranchInputAssertions branch)
  --     newAllocs = map (allocToModed In) (_fnDefBranchInAllocs branch
  --                 ++ findAllocations inLhsVars (concat (_fnDefBranchInputAssertions branch)))
  -- local (genCAllocs %~ (newAllocs ++)) $ f branch

allocToModed :: Mode -> Allocation PikaCore.Expr -> ModedName' AllocAnnotated (PikaCore.Expr' AllocAnnotated)
allocToModed mode alloc = Moded' (allocSize alloc) mode (convertName (allocName alloc))

enterBranchBody ::
  (PikaCore.Expr' AllocAnnotated -> GenC a) ->
  PikaCore.Expr -> GenC a
enterBranchBody f e = do 
  annotated <- GenC $ annotateAllocs e
  f annotated

