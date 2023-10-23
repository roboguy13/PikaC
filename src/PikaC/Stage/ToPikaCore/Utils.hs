{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import Control.Monad.State

import GHC.Stack

data RewriteOne = Rewritten | NoRewrite

rewriteOne :: forall a m. (Monad m, Plated a) => (a -> m (Maybe a)) -> a -> m a
rewriteOne f x0 = evalStateT (rewriteM go x0) NoRewrite
  where
    go :: a -> StateT RewriteOne m (Maybe a)
    go x = do
      get >>= \case
        Rewritten -> pure Nothing
        NoRewrite ->
          lift (f x) >>= \case
            Just r -> put Rewritten *> pure (Just r)
            Nothing -> pure Nothing

onFnDefBranch :: Fresh m => (Expr -> m Expr) -> FnDefBranch -> m FnDefBranch
onFnDefBranch f =
  fnDefBranchBody %%~ f

onFnDef :: Fresh m => (Expr -> m Expr) -> FnDef -> m FnDef
onFnDef f fnDef = do
  (inParams, bnd1) <- unbind (_fnDefBranches fnDef)
  (outParams, (layouts, branches)) <- unbind bnd1
  branches' <- mapM (onFnDefBranch f) branches
  pure $ fnDef
    { _fnDefBranches =
        bind inParams
          (bind outParams (layouts, branches'))
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

