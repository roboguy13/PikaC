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
import PikaC.Stage.ToPikaCore.Utils
import PikaC.Stage.ToPikaCore.SimplifyM

import Data.Bifunctor

import Control.Lens

import Unbound.Generics.LocallyNameless

-- TODO: Make sure names aren't captured
withOfWith :: Logger m => Expr -> SimplifyM m Expr
withOfWith =
  step "withOfWith" $ rewriteM withOfWithOne

-- withOfWithBranch :: Fresh m => FnDefBranch -> m FnDefBranch
-- withOfWithBranch =
--   fnDefBranchBody %%~ rewriteM withOfWithOne

withOfWithOne :: Fresh m => Expr -> m (Maybe Expr)
withOfWithOne (WithIn (WithIn e1 bnd1) bnd2) = do
  (vars1, e2) <- unbind bnd1 
  (vars2, e3) <- unbind bnd2
  vars2' <- mapM (fresh . modedNameName) vars2
  let e3' = rename (zip (map modedNameName vars2) vars2') e3
        -- TODO: Try replacing rename with substs, for speed

  let modedVars2' = zipWith Moded (map getMode vars2) vars2'

  pure $ Just $ WithIn e1
                  $ bind vars1
                      $ WithIn e2 (bind modedVars2' e3')
  
  
  -- unbind bnd >>= \case
  --   (vars2, WithIn e1 bnd1) -> do
  --     (vars1, e3) <- unbind bnd1
  --     vars2' <- mapM (fresh . modedNameName) vars2
  --     let e3' = rename (zip (map modedNameName vars2) vars2') e3
  --
  --     let modedVars2' = zipWith Moded (map getMode vars2) vars2'
  --
  --     pure $ Just $ WithIn e1
  --                     $ bind vars1
  --                         $ WithIn e2 (bind modedVars2' e3')
  -- _ -> pure Nothing
-- withOfWithOne (WithIn (WithIn e1 vars1 e2) vars2 e3) = do
--   vars2' <- mapM fresh vars2
--   let e3' = rename (zip vars2 vars2') e3
--
--   pure $ Just $ WithIn e1 vars1
--               $ WithIn e2 vars2' e3'

withOfWithOne _ = pure Nothing

