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
import PikaC.Syntax.Heaplet

import PikaC.Utils

import Control.Lens

import Unbound.Generics.LocallyNameless

renameResultLayout :: FnDef -> FnDef
renameResultLayout =
  (fnDefBranches.traverse) %~ renameResultLayoutBranch

renameResultLayoutBranch :: FnDefBranch -> FnDefBranch
renameResultLayoutBranch branch =
  let outputs = _fnDefOutputParams branch
  in
  branch & fnDefBranchBody %~ go outputs

go :: [ExprName] -> Expr -> Expr
go newNames (WithIn bnd vars body) =
  WithIn bnd vars (go newNames body)

go newNames (SslAssertion names body) =
  SslAssertion newNames (rename (zip names newNames) body)

go newNames e = e

