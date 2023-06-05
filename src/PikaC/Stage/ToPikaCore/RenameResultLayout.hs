-- | Translate
--
--     f {x ...} ==> {r ...} :=
--       with {...} := ...
--       in
--       ...
--       layout {y ...} { ... }
--
--   to
--
--     f {x ...} ==> {r ...} :=
--       with {...} := ...
--       in
--       ...
--       layout {r ...} { ... }[y/r]

module PikaC.Stage.ToPikaCore.RenameResultLayout
  (renameResultLayout)
  where

import PikaC.Syntax.PikaCore.Expr
import PikaC.Syntax.PikaCore.FnDef
import PikaC.Syntax.Pika.Layout
import PikaC.Syntax.Heaplet

import PikaC.Stage.ToPikaCore.Utils
import PikaC.Stage.ToPikaCore.SimplifyM

import PikaC.Utils
import PikaC.Ppr

import Control.Lens

import Unbound.Generics.LocallyNameless

import Debug.Trace

renameResultLayout :: Logger m => FnDef -> SimplifyM m FnDef
renameResultLayout = step "renameResultLayout" $ \fnDef -> do
  (inputs, bnd1) <- unbind $ _fnDefBranches fnDef
  (outputs, branches) <- unbind bnd1
  branches' <- mapM (onFnDefBranch (go (map modedNameName outputs))) branches

  pure $ fnDef
    { _fnDefBranches =
        bind inputs
          (bind outputs branches')
    }

  -- onFnDef (go (map modedNameName outputs))
  --   $ fnDef { _fnDefBranches = bind inputs $ bind outputs branches }

  -- (fnDefBranches.traverse) %~ renameResultLayoutBranch

-- renameResultLayoutBranch :: FnDefBranch -> FnDefBranch
-- renameResultLayoutBranch branch =
--   let outputs = _fnDefOutputParams branch
--   in
--   branch & fnDefBranchBody %~ go outputs

go :: Fresh m => [ExprName] -> Expr -> m Expr
go newNames (WithIn e bnd) = do
  (vars, body) <- unbind bnd
  body' <- go newNames body
  pure $ WithIn e $ bind vars body'

-- TODO: Make sure modes match
go newNames e0@(SslAssertion bnd) = do
  (vars, body) <- unbind bnd
  let modedNewNames = zipWith Moded (map getMode vars) newNames :: [ModedName Expr]
      sb = zip (map modedNameName vars) (map V newNames)
  -- trace ("substitution: " ++ show sb ++ " in: " ++ show body) $
  pure $ SslAssertion $
      bind [] --modedNewNames
        (substs sb body)

-- go newNames (WithIn bnd vars body) =
--   WithIn bnd vars (go newNames body)

-- go newNames (SslAssertion names body) =
--   SslAssertion newNames (rename (zip names newNames) body)

go newNames e = pure e

