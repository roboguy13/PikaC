module PikaC.Tests.Pika.Test
  where

import           PikaC.Syntax.PikaCore.Expr
import           PikaC.Syntax.PikaCore.FnDef

import           PikaC.Syntax.Pika.Layout
import           PikaC.Syntax.Pika.Pattern
import           PikaC.Syntax.Type
import           PikaC.Syntax.Heaplet

import           PikaC.Ppr

import           Unbound.Generics.LocallyNameless
import           Unbound.Generics.LocallyNameless.Unsafe
import           Unbound.Generics.LocallyNameless.Bind

import qualified PikaC.Backend.C.Syntax as C
import           PikaC.Backend.C.CodeGen
import           PikaC.Backend.C.Monad
import           PikaC.Backend.Utils

import           Control.Lens

import Debug.Trace

layoutPrinter :: Layout Expr -> C.CFunction
layoutPrinter layout =
  let (_, branches) = unsafeUnbind $ _layoutBranches layout
      fnBody = go branches
  in
  C.CFunction
  { C.cfunctionName = ourFnName
  , C.cfunctionParams = params
  , C.cfunctionBody =
      [C.Printf "(" []]
      ++ fnBody
      ++ [C.Printf ")" []]
  }
  where
    params = layoutParamNames layout
    ourFnName = unFnName $ getPrinter (TyVar (string2Name (_layoutName layout)))

    go :: [LayoutBranch Expr] -> [C.Command]
    go [] = []
    go (branch@(LayoutBranch (PatternMatch bnd1)) : rest) =
      let
        (patVars, existsBnd) = unsafeUnbind bnd1
        (existVars, matchBody) = unsafeUnbind existsBnd
        branchNames =
          map (getV . locBase . pointsToLhs) $ getPointsTos matchBody
        branchNamesC = map convertName branchNames

        allNames = getPatternNames patVars ++ map (modedNameName . getExists) existVars
      in
      [C.IfThenElse
        (computeBranchCondition params branchNamesC)
        (goBody (filter (isRecVar matchBody) allNames) matchBody)
        (go rest)
      ]
      where
        goBody :: [ExprName] -> LayoutBody Expr -> [C.Command]
        goBody recVars (LayoutBody []) = []
        goBody recVars (LayoutBody (LPointsTo p@(_ :-> V rhs) : rest)) =
            -- These are values at the current node
          let restCmds = goBody recVars (LayoutBody rest)
              cmdHere =
                  if rhs `elem` recVars
                    then []
                    else
                      [C.Printf "%d "
                        [C.V $ convertName rhs
                        ]
                      ]
          in
            [setupInput p $
               cmdHere ++ restCmds
            ]
        -- TODO: Handle layouts that apply *other* layouts.
        goBody recVars (LayoutBody (LApply layoutName patVar layoutVars : rest))
          | layoutName /= _layoutName layout = error "layoutPrinter: Cannot create a layout printer for a layout that applies another layout"
          | otherwise =
              C.Call ourFnName
                (map (C.V . convertName . getV) layoutVars)
                []
              : goBody recVars (LayoutBody rest)

-- | Is used as an argument to a layout application
isRecVar :: LayoutBody Expr -> ExprName -> Bool
isRecVar body name =
  let appArgs =  body ^.. (unLayoutBody.traversed._LApply._3)
  in
  any (V name `aeq`) $ concat appArgs

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

layoutParamNames :: Layout Expr -> [C.CName]
layoutParamNames layout =
  let (vs, _) = unsafeUnbind $ _layoutBranches layout
  in
  map (convertName . modedNameName) vs

mkOutVars :: [Layout Expr] -> Type -> GenC [C.CName]
mkOutVars _ IntType = (:[]) <$> fresh (string2Name "i")
mkOutVars _ BoolType = (:[]) <$> fresh (string2Name "b")
mkOutVars layouts (TyVar x) =
  let vs = layoutParamNames $ lookupLayout layouts (name2String x)
  in
  mapM fresh vs

getPrinter :: Type -> Printer
getPrinter ty@(FnType {}) = error $ "getPrinter: " ++ ppr' ty
getPrinter IntType = FnName "_printInt"
getPrinter BoolType = FnName "_printBool"
getPrinter (TyVar x) = FnName $ "_print_" ++ name2String x

convertExpr :: [C.CName] -> Expr -> GenC [C.Command]
convertExpr outVars = enterBranchBody (convertBranchBody outVars)

