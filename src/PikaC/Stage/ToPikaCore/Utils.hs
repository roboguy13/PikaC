module PikaC.Stage.ToPikaCore.Utils
  where

import Unbound.Generics.LocallyNameless
import PikaC.Syntax.PikaCore.Expr
import PikaC.Syntax.PikaCore.FnDef
import PikaC.Syntax.Heaplet
import PikaC.Syntax.Pika.Layout
import PikaC.Ppr
import PikaC.Utils

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

-- argExpr :: Fresh m => [Name Expr] -> m Expr
-- argExpr [v] = V <$> fresh v
-- argExpr vs = LayoutV <$> mapM (fmap V . fresh) vs

argExpr :: [Name Expr] -> Expr
argExpr [v] = V v
argExpr vs = LayoutV (map V vs)

lhsNames :: ExprAssertion -> [ExprName]
lhsNames = fastNub . map (getV . locBase . pointsToLhs)

buildWithIns :: [(Expr, [ModedName Expr])] -> Expr -> Expr
buildWithIns [] e = e
buildWithIns ((rhs, vars) : xs) e =
  WithIn
    rhs
    (bind vars (buildWithIns xs e))

freshModed :: Fresh m => ModedName' s a -> m (ModedName' s a)
freshModed (Moded' s m v) = Moded' s m <$> fresh v

