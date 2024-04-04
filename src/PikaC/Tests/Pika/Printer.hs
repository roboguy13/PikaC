module PikaC.Tests.Pika.Printer
  (pprLayoutPrinters
  ,Printer
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
import           PikaC.Backend.C.Monad (runGenC)
import           PikaC.Backend.Utils
import           PikaC.Utils

import           Unbound.Generics.LocallyNameless
import           Unbound.Generics.LocallyNameless.Unsafe

import qualified PikaC.Backend.C.Syntax as C

import           Control.Lens

type Printer = FnName

pprLayoutPrinters :: [Layout Expr] -> Doc
pprLayoutPrinters layouts =
  let printers = map layoutPrinter layouts
  in
  vcat (map C.declFunction printers) $$ vcat (map ppr printers)

layoutPrinter :: Layout Expr -> C.CFunction
layoutPrinter layout =
  let (_, bnd) = unsafeUnbind $ _layoutBranches layout
      (_, branches) = unsafeUnbind bnd
      fnBody = go branches
  in
  C.CFunction
  { C.cfunctionName = name2String ourFnName
  , C.cfunctionParams = params
  , C.cfunctionBody =
      [C.Printf "(" []]
      ++ fnBody
      ++ [C.Printf ")" []]
  }
  where
    params = layoutParamNames layout
    ourFnName = unFnName $ getPrinter (LayoutId (_layoutName layout))

    go :: [LayoutBranch Expr] -> [C.Command]
    go [] = []
    go (branch@(LayoutBranch (PatternMatch bnd1)) : rest) =
      let
        (patVars, existsBnd) = unsafeUnbind bnd1
        (existVars, matchBody0) = unsafeUnbind existsBnd
        matchBody = _ghostCondBody matchBody0
        branchNames =
          map (getV . locBase . pointsToLhs) $ getPointsTos matchBody
        branchNamesC = map convertName branchNames

        allNames = getPatternNames patVars ++ map (modedNameName . getExists) existVars
        cond = (computeBranchCondition params branchNamesC)
      in
      [C.IfThen
        cond
        (goBody (filter (isRecVar matchBody) allNames) matchBody)
      ] ++ [C.IfThen (C.Not cond) (go rest)]
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
            [runGenC $ setupInput p $
               cmdHere ++ restCmds
            ]
        -- TODO: Handle layouts that apply *other* layouts.
        goBody recVars (LayoutBody (LApply layoutName _ghosts patVar layoutVars : rest))
          | layoutName /= _layoutName layout =
              C.Call (name2String (unFnName (getPrinter (LayoutId layoutName))))
                (map (C.V . convertName . getV) layoutVars)
                []
              : goBody recVars (LayoutBody rest)
              
              --error "layoutPrinter: Cannot create a layout printer for a layout that applies another layout"
          | otherwise =
              C.Call (name2String ourFnName)
                (map (C.V . convertName . getV) layoutVars)
                []
              : goBody recVars (LayoutBody rest)


layoutParamNames :: Layout Expr -> [C.CName]
layoutParamNames layout =
  let (_, bnd) = unsafeUnbind $ _layoutBranches layout
      (vs, _) = unsafeUnbind bnd
  in
  map (convertName . modedNameName) vs

getPrinter :: Type -> Printer
getPrinter ty@(FnType {}) = error $ "getPrinter: " ++ ppr' ty
getPrinter IntType = FnName (s2n "_printInt")
getPrinter BoolType = FnName (s2n "_printBool")
getPrinter (LayoutId x) = FnName $ s2n $ "_print_" ++ x

-- | Is used as an argument to a layout application
isRecVar :: LayoutBody Expr -> ExprName -> Bool
isRecVar body name =
  let appArgs =  body ^.. (unLayoutBody.traversed._LApply._4)
  in
  any (V name `aeq`) $ concat appArgs

