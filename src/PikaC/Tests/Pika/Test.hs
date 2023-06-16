module PikaC.Tests.Pika.Test
  where

import           PikaC.Syntax.PikaCore.Expr
import           PikaC.Syntax.PikaCore.FnDef

import           PikaC.Syntax.Pika.Layout
import           PikaC.Syntax.Type

import           PikaC.Ppr

import           Unbound.Generics.LocallyNameless
import           Unbound.Generics.LocallyNameless.Bind

import qualified PikaC.Backend.C.Syntax as C
import           PikaC.Backend.C.CodeGen
import           PikaC.Backend.C.Monad

layoutPrinter :: Layout Expr -> C.CFunction
layoutPrinter = undefined

type Printer = FnName

runTest :: [Layout Expr] -> Type -> Expr -> GenC [C.Command]
runTest layouts ty e = do
  outVars <- mkOutVars layouts ty
  runTestWithPrinter outVars (getPrinter ty) e

runTestWithPrinter :: [C.CName] -> Printer -> Expr -> GenC [C.Command]
runTestWithPrinter outVars printer e = do
  body <- convertExpr outVars e
  pure $
    body ++
    [call printer (map C.V outVars)
    ]

call :: FnName -> [C.CExpr] -> C.Command
call (FnName n) args = C.Call n args []

mkOutVars :: [Layout Expr] -> Type -> GenC [C.CName]
mkOutVars _ IntType = (:[]) <$> fresh (string2Name "i")
mkOutVars _ BoolType = (:[]) <$> fresh (string2Name "b")
mkOutVars layouts (TyVar x) =
  let B vs _ = _layoutBranches $ lookupLayout layouts (name2String x)
  in
  mapM fresh (map (convertName . modedNameName) vs)

getPrinter :: Type -> Printer
getPrinter ty@(FnType {}) = error $ "getPrinter: " ++ ppr' ty
getPrinter IntType = FnName "_printInt"
getPrinter BoolType = FnName "_printBool"
getPrinter (TyVar x) = FnName $ "_print_" ++ name2String x

convertExpr :: [C.CName] -> Expr -> GenC [C.Command]
convertExpr outVars = enterBranchBody (convertBranchBody outVars)

