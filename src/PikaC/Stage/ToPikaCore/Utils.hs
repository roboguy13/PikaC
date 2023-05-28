module PikaC.Stage.ToPikaCore.Utils
  where

import Unbound.Generics.LocallyNameless
import PikaC.Syntax.PikaCore.Expr
import PikaC.Syntax.PikaCore.FnDef
import PikaC.Ppr

import Control.Lens

import GHC.Stack

onFnDefBranch :: Fresh m => (Expr -> m Expr) -> FnDefBranch -> m FnDefBranch
onFnDefBranch f =
  fnDefBranchBody %%~ f

onFnDef :: Fresh m => (Expr -> m Expr) -> FnDef -> m FnDef
onFnDef f fnDef = do
  (inParams, bnd1) <- unbind (_fnDefBranches fnDef)
  (outParams, branches) <- unbind bnd1
  branches' <- mapM (onFnDefBranch f) branches
  pure $ fnDef
    { _fnDefBranches =
        bind inParams
          (bind outParams branches')
    }

getV :: HasCallStack => Expr -> Name Expr
getV (V x) = x
getV e = error $ "getV: " ++ ppr' e

