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

module PikaC.Stage.ToPikaCore.WithOfWith
  (withOfWith
  ,withOfWithBranch
  )
  where

import PikaC.Syntax.PikaCore.Expr
import PikaC.Syntax.PikaCore.FnDef
import PikaC.Syntax.Heaplet

import Data.Bifunctor

import Control.Lens

import Unbound.Generics.LocallyNameless

-- TODO: Make sure names aren't captured
withOfWith :: Fresh m => FnDef -> m FnDef
withOfWith =
  (fnDefBranches.traversed) %%~ withOfWithBranch

withOfWithBranch :: Fresh m => FnDefBranch -> m FnDefBranch
withOfWithBranch =
  fnDefBranchBody %%~ rewriteM withOfWithOne

withOfWithOne :: Fresh m => Expr -> m (Maybe Expr)
withOfWithOne (WithIn (WithIn e1 vars1 e2) vars2 e3) = do
  vars1' <- mapM fresh vars1

  pure $ Just $ WithIn e1 vars1'
              $ WithIn e2 vars2 e3

withOfWithOne _ = pure Nothing

