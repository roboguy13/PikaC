{-# LANGUAGE ScopedTypeVariables #-}

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
import PikaC.Utils

import Data.Foldable
import Data.Maybe

import GHC.Stack

import Debug.Trace

import Unbound.Generics.LocallyNameless

type Outputs = LayoutArg

toCName :: PikaCore.ExprName -> LocName
toCName = string2Name . name2String

codeGenBase :: HasCallStack =>
  PikaCore.Base ->
  CExpr
codeGenBase (PikaCore.V x) = C.LocValue (toCName x :+ 0)
codeGenBase (PikaCore.LocV x) = C.LocValue (x :+ 0)
codeGenBase (PikaCore.LayoutV xs) = error "codeGenBase: LayoutV"
-- codeGenBase (PikaCore.LocValue x) = C.LocValue x
codeGenBase (PikaCore.IntLit i) = C.IntLit i
codeGenBase (PikaCore.BoolLit b) = C.BoolLit b
codeGenBase (PikaCore.Add x y) = C.Add (codeGenBase x) (codeGenBase y)
codeGenBase (PikaCore.Sub x y) = C.Sub (codeGenBase x) (codeGenBase y)
codeGenBase (PikaCore.Equal x y) = C.Equal (codeGenBase x) (codeGenBase y)
codeGenBase (PikaCore.Not x) = C.Not (codeGenBase x)
codeGenBase (PikaCore.And x y) = C.And (codeGenBase x) (codeGenBase y)

codeGenSimple :: HasCallStack => Outputs -> PikaCore.SimpleExpr -> [Command]
codeGenSimple outputs (PikaCore.BaseExpr (PikaCore.LayoutV xs)) =
  codeGenAsn $ connectLayoutArgs outputs xs

codeGenSimple outputs (PikaCore.WithIn vars bnd body) =
  codeGenExpr vars bnd ++ codeGenSimple outputs body

codeGenSimple outputs (PikaCore.SslAssertion params asn0) =
  let asn = fmap (rename (zip params outputs)) asn0
  in
  codeGenAsn asn

codeGenAsn :: PikaCore.ExprAssertion -> [Command]
codeGenAsn asn =
  -- let sorted = topologicalSortPointsTo asn
  -- in
  map codeGenPointsTo asn --sorted

codeGenInputs :: PikaCore.ExprAssertion -> [Command]
codeGenInputs =
  mapMaybe go
  where
    go :: PointsToExpr -> Maybe Command
    go (x :-> (PikaCore.V y)) = Just $ C.Let (toCName y) x
    go _ = Nothing

codeGenPointsTo :: PointsToExpr -> Command
codeGenPointsTo (lhs :-> rhs) =
  C.Assign lhs (codeGenBase rhs)

codeGenExpr :: HasCallStack => Outputs -> PikaCore.Expr -> [Command]
codeGenExpr outputs (PikaCore.SimpleExpr e) =
  codeGenSimple outputs e
codeGenExpr outputs (PikaCore.App f xs) =
    -- TODO: Translate PikaCore function names into C function names
  [C.Call f (convert (fold xs)) (convert outputs)]
  where
    convert :: Outputs -> [CExpr]
    convert = map C.V . toList

codeGenAllocation :: Allocation -> Command
codeGenAllocation (Alloc x sz) =
  C.IntoMalloc x sz

setToZero :: [LocName] -> [Command]
setToZero = map go
  where
    go x = C.Assign (x :+ 0) (C.IntLit 0)

-- sortedCodeGen :: (Ord a, Show a) => Outputs a -> PikaCore.Expr a -> [Command a]
-- sortedCodeGen outputs = topologicalSortCommands . codeGenExpr outputs

codeGenFnBranch :: FnDef -> FnDefBranch -> [Command] -> Command
codeGenFnBranch fn branch elseCmd =
  let cond = codeGenBase (computeBranchCondition fn branch)
      names = fnDefOutputParams branch
      allocs =
        findAllocations
          names
          (PikaCore.getPointsToExpr (fnDefBranchBody branch))
      zeros = findSetToZero names (PikaCore.getPointsToExpr (fnDefBranchBody branch))
  in
  C.IfThenElse
    cond
    (setToZero zeros
      <> map codeGenAllocation allocs
      <> concatMap codeGenInputs (fnDefBranchInputAssertions branch)
      <> codeGenExpr (fnDefOutputParams branch) (fnDefBranchBody branch)
    )
    elseCmd

codeGenFn :: FnDef -> C.CFunction
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

connectLayoutArgs :: LayoutArg -> LayoutArg -> PikaCore.ExprAssertion
connectLayoutArgs xs ys = zipWith (:->) (map (:+ 0) xs) (map PikaCore.LocV ys)

example :: PikaCore.Expr
example =
  PikaCore.SimpleExpr $
  PikaCore.WithIn (map s2n ["r", "y"]) (PikaCore.App "convertList2" [[s2n "nxt"]])
    $ PikaCore.SslAssertion (map s2n ["r", "z"])
        [ (s2n "r" :+ 0) :-> PikaCore.V (s2n "h")
        , (s2n "r" :+ 1) :-> PikaCore.V (s2n "y")
        , (s2n "r" :+ 2) :-> PikaCore.V (s2n "w")
        ]

exampleAsn :: PikaCore.ExprAssertion
exampleAsn = 
        [ (s2n "r" :+ 0) :-> PikaCore.V (s2n "h")
        , (s2n "r" :+ 1) :-> PikaCore.V (s2n "y")
        , (s2n "r" :+ 2) :-> PikaCore.V (s2n "w")
        ]

exampleFn :: FnDef
exampleFn =
  FnDef
    { fnDefName = "convertList2"
    , fnDefParams = map s2n ["x", "w", "r"]
    , fnDefBranches =
        [FnDefBranch
          { fnDefOutputParams = map s2n ["r"]
          , fnDefBranchInputAssertions = []
          , fnDefBranchBody =
              PikaCore.SimpleExpr $
              PikaCore.SslAssertion (map s2n ["r"]) []
          }

        ,FnDefBranch
          { fnDefOutputParams = map s2n ["r"]
          , fnDefBranchInputAssertions =
              [[(s2n "x" :+ 0) :-> PikaCore.V (s2n "h")
               ,(s2n "x" :+ 1) :-> PikaCore.V (s2n "nxt")
               ]]
          , fnDefBranchBody =
              PikaCore.SimpleExpr $
              PikaCore.WithIn (map s2n ["r", "y"]) (PikaCore.App "convertList2" [map s2n ["nxt"]])
                $ PikaCore.SslAssertion (map s2n ["r", "z"])
                    [ (s2n "r" :+ 0) :-> PikaCore.V (s2n "h")
                    , (s2n "r" :+ 1) :-> PikaCore.V (s2n "y")
                    , (s2n "r" :+ 2) :-> PikaCore.V (s2n "w")
                    ]
          }
        ]
    }

