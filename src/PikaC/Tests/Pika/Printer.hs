module PikaC.Tests.Pika.Printer
  (Printer
  ,getPrinter
  ,layoutPrinter
  ,layoutParamNames
  )
  where

import           PikaC.Syntax.PikaCore.Expr
import           PikaC.Syntax.Pika.Layout
import           PikaC.Syntax.Pika.Pattern
import           PikaC.Syntax.Type
import           PikaC.Syntax.Heaplet

import           PikaC.Ppr

import           PikaC.Backend.C.CodeGen
import           PikaC.Backend.Utils

import           Unbound.Generics.LocallyNameless
import           Unbound.Generics.LocallyNameless.Unsafe

import qualified PikaC.Backend.C.Syntax as C

import           Control.Lens

type Printer = FnName

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


layoutParamNames :: Layout Expr -> [C.CName]
layoutParamNames layout =
  let (vs, _) = unsafeUnbind $ _layoutBranches layout
  in
  map (convertName . modedNameName) vs

getPrinter :: Type -> Printer
getPrinter ty@(FnType {}) = error $ "getPrinter: " ++ ppr' ty
getPrinter IntType = FnName "_printInt"
getPrinter BoolType = FnName "_printBool"
getPrinter (TyVar x) = FnName $ "_print_" ++ name2String x

-- | Is used as an argument to a layout application
isRecVar :: LayoutBody Expr -> ExprName -> Bool
isRecVar body name =
  let appArgs =  body ^.. (unLayoutBody.traversed._LApply._3)
  in
  any (V name `aeq`) $ concat appArgs

