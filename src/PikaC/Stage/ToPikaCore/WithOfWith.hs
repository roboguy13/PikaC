-- | Translate
--
--     with { x ... } :=
--             with { y ... } := e_1
--             in e_2
--     in e_3
--
--   to
--
--     with {y ...} := e_1
--     in
--     with {x ...} := e_2
--     in e_3

{-# LANGUAGE LambdaCase #-}

module PikaC.Stage.ToPikaCore.WithOfWith
  (withOfWith
  -- ,withOfWithBranch
  )
  where

import PikaC.Syntax.PikaCore.Expr
import PikaC.Syntax.PikaCore.FnDef
import PikaC.Syntax.Pika.Layout
import PikaC.Syntax.Heaplet

import PikaC.Utils
import PikaC.Ppr

import PikaC.Stage.ToPikaCore.Utils
import PikaC.Stage.ToPikaCore.SimplifyM

import Data.Bifunctor

import Control.Lens

import Unbound.Generics.LocallyNameless

import Debug.Trace

-- TODO: Make sure names aren't captured
withOfWith :: Logger m => Expr -> SimplifyM m Expr
withOfWith =
  step "withOfWith" $ rewriteM withOfWithOne

withOfWithOne :: Fresh m => Expr -> m (Maybe Expr)
withOfWithOne orig@(WithIn (WithIn e1 bnd1) bnd2) = do
  (vars1, e2) <- unbind bnd1 
  (vars2, e3) <- unbind bnd2
  vars2' <- mapM (fresh . modedNameName) vars2

  vars1' <- mapM (fresh . modedNameName) vars1
  let modedVars1' = zipWith Moded (map getMode vars1) vars1'

      renameIt =
        substs (zip (map modedNameName vars2) (map V vars2') ++ zip (map modedNameName vars1) (map V vars1'))

  let e3' = renameIt e3
  let e2' = renameIt e2

  let modedVars2' = zipWith Moded (map getMode vars2) vars2'

  let r = WithIn e1
                  $ bind modedVars1'
                      $ WithIn e2' (bind modedVars2' e3')

  pure $ Just r

withOfWithOne _ = pure Nothing

