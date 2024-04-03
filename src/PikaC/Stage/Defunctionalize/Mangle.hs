--
-- Turn _ in names to __ to avoid collision with function names
-- generated during defunctionalization.
--

module PikaC.Stage.Defunctionalize.Mangle
  (mangleFnDef)
  where

import PikaC.Syntax.Pika.Expr
import PikaC.Syntax.Pika.FnDef
import PikaC.Utils

import Unbound.Generics.LocallyNameless

import Control.Lens

mangleFnDef :: (Alpha (f [FnDefBranch]), Subst Expr (f [FnDefBranch])) =>
  FnDef' f -> FnDef' f
mangleFnDef def =
  rename (zip exprFVs (map mangleName exprFVs)) def
  where
    exprFVs :: [ExprName]
    exprFVs = toListOf fv def

mangleName :: ExprName -> ExprName
mangleName = string2Name . mangleString . name2String

mangleString :: String -> String
mangleString "" = ""
mangleString ('_':cs) = "__" ++ mangleString cs
mangleString (c  :cs) = c     : mangleString cs

