module PikaC.Backend.C.CodeGen
  where

import qualified PikaC.Syntax.PikaCore.Expr as PikaCore
import PikaC.Syntax.PikaCore.Expr (PointsToExpr)
import PikaC.Syntax.PikaCore.FnDef
import qualified PikaC.Backend.C.Syntax as C
import PikaC.Backend.C.Syntax (CExpr, Command, CFunction)
import PikaC.Backend.C.DataDeps
import PikaC.Syntax.Heaplet

import PikaC.Preorder.Preorder

import PikaC.Ppr

import Data.Foldable

import GHC.Stack

import Debug.Trace

type Outputs = LayoutArg

codeGenBase :: HasCallStack =>
  PikaCore.Base a ->
  CExpr a
codeGenBase (PikaCore.V x) = C.V (x :+ 0)
codeGenBase (PikaCore.LayoutV xs) = error "codeGenBase: LayoutV"
codeGenBase (PikaCore.IntLit i) = C.IntLit i
codeGenBase (PikaCore.BoolLit b) = C.BoolLit b
codeGenBase (PikaCore.Add x y) = C.Add (codeGenBase x) (codeGenBase y)
codeGenBase (PikaCore.Sub x y) = C.Sub (codeGenBase x) (codeGenBase y)
codeGenBase (PikaCore.Equal x y) = C.Equal (codeGenBase x) (codeGenBase y)
codeGenBase (PikaCore.Not x) = C.Not (codeGenBase x)
codeGenBase (PikaCore.And x y) = C.And (codeGenBase x) (codeGenBase y)

codeGenSimple :: (HasCallStack, Ord a, Show a) => Outputs a -> PikaCore.SimpleExpr a -> [Command a]
codeGenSimple outputs (PikaCore.BaseExpr (PikaCore.LayoutV xs)) =
  codeGenAsn $ connectLayoutArgs outputs xs

codeGenSimple outputs (PikaCore.WithIn vars bnd body) =
  codeGenExpr vars bnd ++ codeGenSimple outputs body

codeGenSimple outputs (PikaCore.SslAssertion params asn0) =
  let asn = fmap (renameLayoutArg params outputs) asn0
  in
  codeGenAsn asn

codeGenAsn :: (Show a, Ord a) => PikaCore.ExprAssertion a -> [Command a]
codeGenAsn asn =
  -- let sorted = topologicalSortPointsTo asn
  -- in
  map codeGenPointsTo asn --sorted

codeGenInputs :: PikaCore.ExprAssertion a -> [Command a]
codeGenInputs =
  map go
  where
    go (x :-> PikaCore.V y) = C.Let y x

codeGenPointsTo :: PointsToExpr a -> Command a
codeGenPointsTo (lhs :-> rhs) =
  C.Assign lhs (codeGenBase rhs)

codeGenExpr :: (HasCallStack, Ord a, Show a) => Outputs a -> PikaCore.Expr a -> [Command a]
codeGenExpr outputs (PikaCore.SimpleExpr e) =
  codeGenSimple outputs e
codeGenExpr outputs (PikaCore.App f xs) =
    -- TODO: Translate PikaCore function names into C function names
  [C.Call f (convert (fold xs)) (convert outputs)]
  where
    convert = map (C.V . (:+ 0)) . toList

codeGenAllocation :: Allocation a -> Command a
codeGenAllocation (Alloc x sz) =
  C.IntoMalloc x sz

setToZero :: [a] -> [Command a]
setToZero = map go
  where
    go x = C.Assign (x :+ 0) (C.IntLit 0)

-- sortedCodeGen :: (Ord a, Show a) => Outputs a -> PikaCore.Expr a -> [Command a]
-- sortedCodeGen outputs = topologicalSortCommands . codeGenExpr outputs

codeGenFnBranch :: (Ord a, Show a) => FnDef a -> FnDefBranch a -> [Command a] -> Command a
codeGenFnBranch fn branch elseCmd =
  let cond = codeGenBase (computeBranchCondition fn branch)
      names = getLayoutArg (fnDefOutputParams branch)
      allocs =
        findAllocations
          names
          (getPointsTo (fnDefBranchBody branch))
      zeros = findSetToZero names (getPointsTo (fnDefBranchBody branch))
  in
  C.IfThenElse
    cond
    (setToZero zeros
      <> map codeGenAllocation allocs
      <> concatMap codeGenInputs (fnDefBranchInputAssertions branch)
      <> codeGenExpr (fnDefOutputParams branch) (fnDefBranchBody branch)
    )
    elseCmd

codeGenFn :: (Ord a, Show a) => FnDef a -> C.CFunction a
codeGenFn fn =
  C.CFunction
    { C.cfunctionName = fnDefName fn
    , C.cfunctionParams = fnDefParams fn
    , C.cfunctionBody = go (fnDefBranches fn)
    }
  where
    go [] = [C.Nop]
    go (x:xs) =
      [codeGenFnBranch fn x (go xs)]

connectLayoutArgs :: LayoutArg a -> LayoutArg a -> PikaCore.ExprAssertion a
connectLayoutArgs (LayoutArg xs) (LayoutArg ys) = zipWith (:->) (map (:+ 0) xs) (map PikaCore.V ys)

example :: PikaCore.Expr String
example =
  PikaCore.SimpleExpr $
  PikaCore.WithIn (LayoutArg ["r", "y"]) (PikaCore.App "convertList2" [LayoutArg ["nxt"]])
    $ PikaCore.SslAssertion (LayoutArg ["r", "z"])
        [ ("r" :+ 0) :-> PikaCore.V "h"
        , ("r" :+ 1) :-> PikaCore.V "y"
        , ("r" :+ 2) :-> PikaCore.V "w"
        ]

exampleAsn :: PikaCore.ExprAssertion String
exampleAsn = 
        [ ("r" :+ 0) :-> PikaCore.V "h"
        , ("r" :+ 1) :-> PikaCore.V "y"
        , ("r" :+ 2) :-> PikaCore.V "w"
        ]

exampleFn :: FnDef String
exampleFn =
  FnDef
    { fnDefName = "convertList2"
    , fnDefParams = ["x", "w", "r"]
    , fnDefBranches =
        [FnDefBranch
          { fnDefOutputParams = LayoutArg ["r"]
          , fnDefBranchInputAssertions = []
          , fnDefBranchBody =
              PikaCore.SimpleExpr $
              PikaCore.SslAssertion (LayoutArg ["r"]) []
          }

        ,FnDefBranch
          { fnDefOutputParams = LayoutArg ["r"]
          , fnDefBranchInputAssertions =
              [[("x" :+ 0) :-> PikaCore.V "h"
               ,("x" :+ 1) :-> PikaCore.V "nxt"
               ]]
          , fnDefBranchBody =
              PikaCore.SimpleExpr $
              PikaCore.WithIn (LayoutArg ["r", "y"]) (PikaCore.App "convertList2" [LayoutArg ["nxt"]])
                $ PikaCore.SslAssertion (LayoutArg ["r", "z"])
                    [ ("r" :+ 0) :-> PikaCore.V "h"
                    , ("r" :+ 1) :-> PikaCore.V "y"
                    , ("r" :+ 2) :-> PikaCore.V "w"
                    ]
          }
        ]
    }

