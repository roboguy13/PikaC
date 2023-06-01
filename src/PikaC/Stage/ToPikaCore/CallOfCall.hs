--
-- f (g e ...)
--
--      ==>
--
-- let {x ...} := g e
-- in f {x ...}
--

module PikaC.Stage.ToPikaCore.CallOfCall
  (callOfCall)
  where

import PikaC.Syntax.PikaCore.Expr
import PikaC.Syntax.PikaCore.FnDef
import PikaC.Syntax.Heaplet
import PikaC.Syntax.Pika.Layout

import PikaC.Utils

import PikaC.Stage.ToPikaCore.Utils
import PikaC.Stage.ToPikaCore.SimplifyM

import Control.Lens
import Control.Lens.Extras

import Control.Monad

import Unbound.Generics.LocallyNameless

callOfCall :: Logger m => FnDef -> SimplifyM m FnDef
callOfCall = step "callOfCall" $ onFnDef (rewriteM go)

go :: Fresh m => Expr -> m (Maybe Expr)
go (App f allocsF args)
  | any (is _App) args = do
      (withs, newArgs) <- getAndReplaceApps args
      pure $ Just $ mkWithIns withs (App f allocsF newArgs)
go _ = pure Nothing

mkWithIns :: [(Expr, [ModedName Expr])] -> Expr -> Expr
mkWithIns [] e = e
mkWithIns ((rhs, vars) : xs) e =
  WithIn
    rhs
    (bind vars (mkWithIns xs e))

getAndReplaceApps :: Fresh m =>
  [Expr] -> m ([(Expr, [ModedName Expr])], [Expr])
getAndReplaceApps [] = pure ([], [])
getAndReplaceApps (app@(App g allocsG args) : xs) = do
  intermediateVars <- replicateM (length allocsG) (fresh (string2Name "ww"))

  (restWiths, restArgs) <- getAndReplaceApps xs

  -- TODO: Use correct modes
  pure ((app, map (Moded Out) intermediateVars) : restWiths, LayoutV (map V intermediateVars) : restArgs)
getAndReplaceApps (e : xs) = do
  (withs, rest) <- getAndReplaceApps xs
  pure (withs, e : rest)

