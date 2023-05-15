module PikaC.Backend.C.CodeGen
  where

import qualified PikaC.Syntax.PikaCore.Expr as PikaCore
import qualified PikaC.Backend.C.Syntax as C
import PikaC.Backend.C.Syntax (CExpr, Command, CFunction)
import PikaC.Syntax.Heaplet

import GHC.Stack

type Outputs = LayoutArg

codeGenBase :: HasCallStack =>
  PikaCore.Base a ->
  CExpr a
codeGenBase (PikaCore.V x) = C.V x
codeGenBase (PikaCore.LayoutV xs) = error "codeGenBase: LayoutV"
codeGenBase (PikaCore.IntLit i) = C.IntLit i
codeGenBase (PikaCore.BoolLit b) = C.BoolLit b
codeGenBase (PikaCore.Add x y) = C.Add (codeGenBase x) (codeGenBase y)
codeGenBase (PikaCore.Sub x y) = C.Sub (codeGenBase x) (codeGenBase y)
codeGenBase (PikaCore.Equal x y) = C.Equal (codeGenBase x) (codeGenBase y)
codeGenBase (PikaCore.Not x) = C.Not (codeGenBase x)
codeGenBase (PikaCore.And x y) = C.And (codeGenBase x) (codeGenBase y)

codeGenSimple :: HasCallStack => Outputs a -> PikaCore.SimpleExpr a -> [Command a]
codeGenSimple outputs (PikaCore.WithIn vars bnd body) =
  codeGenExpr vars bnd ++ codeGenSimple outputs body

codeGenExpr :: HasCallStack => Outputs a -> PikaCore.Expr a -> [Command a]
codeGenExpr = undefined

