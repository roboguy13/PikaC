module PikaC.Backend.C.CodeGen
  where

import qualified PikaC.Syntax.PikaCore.Expr as PikaCore
import PikaC.Syntax.PikaCore.Expr (PointsToExpr)
import qualified PikaC.Backend.C.Syntax as C
import PikaC.Backend.C.Syntax (CExpr, Command, CFunction)
import PikaC.Syntax.Heaplet

import PikaC.Preorder.Preorder

import Data.Foldable

import GHC.Stack

type Outputs = LayoutArg

codeGenBase :: HasCallStack =>
  PikaCore.Base a ->
  CExpr a
-- codeGenBase (PikaCore.V x) = C.V x
codeGenBase (PikaCore.LayoutV xs) = error "codeGenBase: LayoutV"
codeGenBase (PikaCore.IntLit i) = C.IntLit i
codeGenBase (PikaCore.BoolLit b) = C.BoolLit b
codeGenBase (PikaCore.Add x y) = C.Add (codeGenBase x) (codeGenBase y)
codeGenBase (PikaCore.Sub x y) = C.Sub (codeGenBase x) (codeGenBase y)
codeGenBase (PikaCore.Equal x y) = C.Equal (codeGenBase x) (codeGenBase y)
codeGenBase (PikaCore.Not x) = C.Not (codeGenBase x)
codeGenBase (PikaCore.And x y) = C.And (codeGenBase x) (codeGenBase y)

codeGenSimple :: (HasCallStack, Ord a) => Outputs a -> PikaCore.SimpleExpr a -> [Command a]
codeGenSimple outputs (PikaCore.WithIn vars bnd body) =
  codeGenExpr vars bnd ++ codeGenSimple outputs body

codeGenSimple outputs (PikaCore.SslAssertion params asn0) =
  let asn = fmap (renameLayoutArg params outputs) asn0
      sorted = topologicalSortPointsTo asn
  in
  map codeGenPointsTo sorted

topologicalSortPointsTo :: Ord a => [PointsToExpr a] -> [PointsToExpr a]
topologicalSortPointsTo = map to . topologicalSortBy isLe . map from
  where
    to (x, y) = x :-> y
    from (x :-> y) = (x, y)

    isLe (lhs1, rhs1) (lhs2, rhs2) =
      all (`notElem` lhs2) (toList rhs1)

codeGenPointsTo :: PointsToExpr a -> Command a
codeGenPointsTo (lhs :-> rhs) =
  C.Assign lhs (codeGenBase rhs)

codeGenExpr :: (HasCallStack, Ord a) => Outputs a -> PikaCore.Expr a -> [Command a]
codeGenExpr outputs (PikaCore.SimpleExpr e) =
  codeGenSimple outputs e
codeGenExpr outputs (PikaCore.App f xs) =
    -- TODO: Translate PikaCore function names into C function names
  [C.Call f (map C.V . toList $ (fold xs <> outputs))]

-- assignToOutputs :: Outputs a -> [PikaCore.Expr a] -> [Command a]
-- assignToOutputs (LayoutArg []) [] = []
-- assignToOutputs (LayoutArg (outputVar:vars)) (e:es) = undefined

