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

import Debug.Trace

newtype GenCEnv =
  GenCEnv
    { _genCAllocs :: [Allocation PikaCore.Expr]
    }

-- type instance XLayout

type instance XModed AllocAnnotated = Int

annotateModed :: Moded (Name (PikaCore.Expr' PC)) -> Int -> Moded' AllocAnnotated (Name (PikaCore.Expr' AllocAnnotated))
annotateModed (Moded' () m x) i = Moded' i m (string2Name (show x))

-- instance Alpha (Moded' AllocAnnot

annotateAllocs :: PikaCore.Expr' PC -> PikaCore.Expr' AllocAnnotated
annotateAllocs (PikaCore.V x) = PikaCore.V x
annotateAllocs (PikaCore.LayoutV xs) = PikaCore.LayoutV (map annotateAllocs xs)
annotateAllocs (PikaCore.IntLit i) = PikaCore.IntLit i
annotateAllocs (PikaCore.Add x y) =
  PikaCore.Add (annotateAllocs x) (annotateAllocs y)
annotateAllocs (PikaCore.Not x) =
  PikaCore.Not (annotateAllocs x)
annotateAllocs (PikaCore.Sub x y) =
  PikaCore.Sub (annotateAllocs x) (annotateAllocs y)
annotateAllocs (PikaCore.Equal x y) =
  PikaCore.Equal (annotateAllocs x) (annotateAllocs y)
annotateAllocs (PikaCore.And x y) =
  PikaCore.And (annotateAllocs x) (annotateAllocs y)
annotateAllocs (PikaCore.WithIn e bnd) =
  let (vars, body) = unsafeUnbind bnd
  in
  PikaCore.WithIn (annotateAllocs e)
    $ bind
        (zipWith annotateModed vars (exprOutputAllocs body))
        body
annotateAllocs (PikaCore.SslAssertion bnd) =
  let (vars, body) = unsafeUnbind bnd
      vars' = map modedNameName vars
  in
  PikaCore.SslAssertion
    $ bind
        (zipWith annotateModed vars (map allocSize (findAllocations vars' body)))
        body
annotateAllocs (PikaCore.App f sz args) =
  PikaCore.App f sz (map annotateAllocs args)

exprOutputAllocs :: PikaCore.Expr -> [Int]
exprOutputAllocs = go
  where
    go (PikaCore.WithIn _ bnd) =
      let (vars, body) = unsafeUnbind bnd
      in
      go body
    go (PikaCore.SslAssertion bnd) =
      let (vars, asn) = unsafeUnbind bnd
      in
      map allocSize $ findAllocations (map modedNameName vars) asn
    go (PikaCore.App _ sz _) = sz
    go _ = []

-- getTopLevelAllocs :: PikaCore.FnDefBranch -> [Allocation PikaCore.Expr]
-- getTopLevelAllocs branch =
--     fastNub $
--     flip execState (_fnDefBranchInAllocs branch) $
--     traverse go $ universe $ _fnDefBranchBody branch
--   where
--     go ::
--       PikaCore.Expr ->
--       State [Allocation PikaCore.Expr] ()
--     go (SslAssertion bnd) = undefined
--     go _ = pure ()
--
--     insertAlloc :: Allocation PikaCore.Expr -> State [Allocation PikaCore.Expr] ()
--     insertAlloc x = modify (x:)

  -- pure $
  --   _fnDefBranchInAllocs branch
  --     ++ undefined
  -- pure $
  --   findAllocations inParams $  undefined
--
-- copy :: PikaCore.ExprName -> CName -> GenC Command
-- copy = undefined

makeLenses ''GenCEnv

newtype GenC a = GenC { unGenC :: ReaderT GenCEnv FreshM a }
  deriving (Functor, Applicative, Monad, MonadReader GenCEnv, Fresh)

runGenC :: GenC a -> a
runGenC = runFreshM . runGenC'

runGenC' :: GenC a -> FreshM a
runGenC' (GenC m) = runReaderT m (GenCEnv mempty)

-- -- TODO: Does this obey scope properly?
-- internExprName :: HasCallStack => PikaCore.ExprName -> GenC CName
-- internExprName n =
--   if not (isFreeName n)
--     then error "internExprName: Bound variable"
--     else do
--       assocs <- gets _namePcToC
--       case lookup n assocs of
--         Just n' -> pure n'
--         Nothing -> do
--           n' <- fresh (string2Name (name2String n))
--           namePcToC %= ((n, n') :)
--           pure n'

