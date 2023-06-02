{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module PikaC.Backend.C.CodeGen
  where

import qualified PikaC.Syntax.PikaCore.Expr as PikaCore
import qualified PikaC.Syntax.PikaCore.FnDef as PikaCore

import PikaC.Syntax.PikaCore.FnDef (FnDef (..), FnDefBranch (..))

import qualified PikaC.Backend.C.Syntax as C
import PikaC.Backend.C.Syntax (CExpr, CLoc, Command)

import PikaC.Backend.C.Monad

-- import PikaC.Backend.C.DataDeps
import PikaC.Syntax.Heaplet
import PikaC.Syntax.Pika.Layout

import PikaC.Ppr
import PikaC.Utils

import Data.Maybe

import Control.Applicative
import Control.Monad

import GHC.Stack

import Debug.Trace

import Unbound.Generics.LocallyNameless.Bind
import Unbound.Generics.LocallyNameless

import Control.Lens

import Control.Arrow

type Outputs = LayoutArg CExpr

codeGenFn :: PikaCore.FnDef -> C.CFunction
codeGenFn fnDef = runFreshM $ do
  let PikaCore.FnName fnName = _fnDefName fnDef

  (inVars0, bnd1) <- unbind $ _fnDefBranches fnDef
  (outVars0, branches0) <- unbind bnd1
  let inParams = map (convertName . modedNameName) inVars0
      outParams = map (convertName . modedNameName) outVars0

  -- inParams <- mapM fresh (map (convertName . modedNameName) inVars0)
  -- outParams <- mapM fresh (map (convertName . modedNameName) outVars0)

  let params = inParams ++ outParams

  -- let branches = instantiate (instantiate (_fnDefBranches fnDef) (map PikaCore.modedExpr inVars0)) (map PikaCore.modedExpr outVars0)

  -- let branches =
  --       substs
  --         (map PikaCore.modedExpr outVars0)
  --         (substs
  --           (map PikaCore.modedExpr inVars0)
  --           (_fnDefBranches fnDef))

  body <- mapM (convertBranch outParams) branches0

  -- trace ("params = " ++ show params)
  pure $ C.CFunction
    { C.cfunctionName = fnName
    , C.cfunctionParams = params
    , C.cfunctionBody = [flattenBranchCmds inParams (zip branches0 body)]
    }

-- genAsn ::
--   Bind [ModedName PikaCore.Expr] PikaCore.ExprAssertion ->
--   FreshM [C.Command]
-- genAsn bnd = do
--   (vars, asn) <- unbind bnd
--   let allocs = findAllocations (map modedNameName vars) asn
--
--   pure $ map codeGenAllocation allocs
--           ++ map (codeGenPointsTo . convertPointsTo) asn

flattenBranchCmds :: [C.CName] -> [(FnDefBranch, [C.Command])] -> C.Command
flattenBranchCmds _ [] = C.Nop
flattenBranchCmds allNames ((branch, cmds) : rest) =
  C.IfThenElse (C.computeBranchCondition allNames branchNames)
    cmds
    [flattenBranchCmds allNames rest]
  where
    branchNames =
      concatMap (map (convertName . PikaCore.getV . locBase . pointsToLhs)) $ _fnDefBranchInputAssertions branch

convertBranch :: [C.CName] -> PikaCore.FnDefBranch -> FreshM [C.Command]
convertBranch outVars branch = do
  body <- convertBranchBody outVars $ _fnDefBranchBody branch
  pure $
    -- codeGenAllocations (_fnDefBranchInAllocs branch)
    setupInputs (concat (_fnDefBranchInputAssertions branch)) body

setupInputs :: [PointsTo PikaCore.Expr] -> [C.Command] -> [C.Command]
setupInputs [] cmds = cmds
setupInputs (p:ps) cmds =
  [setupInput p (setupInputs ps cmds)]

-- | Take inputs heaplets like (x+2) :-> v and generate v = *(x+2)
setupInput :: PointsTo PikaCore.Expr -> [C.Command] -> C.Command
setupInput ((PikaCore.V lhs :+ i) :-> PikaCore.V rhs) body =
  C.Let (C.V (convertName lhs) :+ i)
    $ bind (convertName rhs) body
setupInput e _ = error $ "setupInput: " ++ ppr' e
  -- C.Let (convertName rhs) (C.V (convertName lhs) :+ i)

convertName :: PikaCore.ExprName -> C.CName
-- convertName = string2Name . name2String
convertName = string2Name . show

convertBranchBody :: [C.CName] -> PikaCore.Expr -> FreshM [C.Command]
convertBranchBody outVars = go
  where
    go :: PikaCore.Expr -> FreshM [C.Command]
    go (PikaCore.V x) = pure [assignVar (getOneVar outVars) x]
    go (PikaCore.LayoutV xs) = -- TODO: Make sure lengths match
        pure $ zipWith assignVar outVars (map PikaCore.getV xs)
    go (PikaCore.IntLit i) =
        pure [assignValue (getOneVar outVars) (C.IntLit i)]
    go (PikaCore.BoolLit b) =
        pure [assignValue (getOneVar outVars) (C.BoolLit b)]
    go (PikaCore.Add x y) = goBin C.Add x y
    go (PikaCore.Sub x y) = goBin C.Sub x y
    go (PikaCore.Equal x y) = goBin C.Equal x y
    go (PikaCore.Not x) =
      pure [assignValue (getOneVar outVars) (C.Not (convertBase x))]
    go (PikaCore.And x y) = goBin C.And x y
    go (PikaCore.App (PikaCore.FnName f) sizes xs)
      | not (isConstructor f) && all PikaCore.isBasic xs =
          pure
            [C.Call f
              (map convertBase xs)
              (map C.V outVars)
            ]

    go (PikaCore.App {}) = error "convertBranchBody: App should be bound by with-in if its arguments are not basic expressions"
    go (PikaCore.WithIn e bnd) = do
      (vars, body) <- unbind bnd
      let modes = map getMode vars
          unmodedVars = map modedNameName vars
      -- freshVars <- mapM fresh vars
      case e of
        PikaCore.V x ->
          let body' = substs (zip unmodedVars [PikaCore.V x]) body
          in
          go body'

        PikaCore.LayoutV xs ->
          let body' = substs (zip unmodedVars xs) body
          in
          go body'

        PikaCore.App (PikaCore.FnName f) szs args -> do
          body' <- go body
          let outs = map (C.V . convertName) unmodedVars
              allocs = zipWith Alloc unmodedVars szs

          pure $
            codeGenAllocations allocs $
            [C.Call f
              (getAppArgs args)
              outs
            ] ++ body'
        PikaCore.WithIn {} -> error "Nested with-in"
        PikaCore.SslAssertion bnd1 -> do
          (asnVars, heaplets) <- unbind bnd1

          case asnVars of
            [] -> do
              -- let heaplets' = substs (zip (toListOf fv heaplets) (map PikaCore.V unmodedVars)) heaplets
              let body' = substs (zip unmodedVars (map PikaCore.V (toListOf fv heaplets))) body
              go body'

            _ -> do
              let heaplets' = substs (zip (map modedNameName asnVars) (map PikaCore.V unmodedVars)) heaplets
                  allocs = findAllocations unmodedVars heaplets'
              body' <- go body

              pure $ codeGenAllocations allocs $
                map codeGenPointsTo heaplets'
                ++ body'
        _
          | [var] <- unmodedVars ->
              let body' = subst var e body
              in
              go body'
        _ -> error $ "Expected variable, variable list or function application in with-in. Found " ++ ppr' e

    go (PikaCore.SslAssertion bnd) = do
      (vars, body) <- unbind bnd
      let unmodedVars = map modedNameName vars
          modes = map getMode vars

          outVars' :: [PikaCore.Expr]
          outVars' =
            map (PikaCore.V . string2Name . show) outVars
            -- zipWith (\m v -> Moded m (PikaCore.V (string2Name (name2String v))))
            -- zipWith (\m v -> Moded m (PikaCore.V (string2Name (show v))))
            --   modes
            --   outVars
      pure (map codeGenPointsTo (substs (zip unmodedVars outVars') body))
    go e = error $ "convertBranchBody: " ++ ppr' e

    goBin f x y = 
        pure [assignValue (getOneVar outVars) (f (convertBase x) (convertBase y))]

getAppArgs :: [PikaCore.Expr] -> [C.CExpr]
getAppArgs = concatMap go
  where
    go (PikaCore.LayoutV xs) = map convertBase xs
    go e = [convertBase e]

getOneVar :: HasCallStack => [C.CName] -> C.CName
getOneVar [v] = v
getOneVar vs = error $ "Expected one variable, got " ++ show vs


assignVar :: C.CName -> PikaCore.ExprName -> C.Command
assignVar cv = assignValue cv . C.V . convertName

assignValue :: C.CName -> CExpr -> C.Command
assignValue cv = C.Assign (C.V cv :+ 0)

convertBase :: HasCallStack => PikaCore.Expr -> CExpr
convertBase (PikaCore.V x) = C.V $ convertName x
convertBase (PikaCore.IntLit i) = C.IntLit i
convertBase (PikaCore.BoolLit b) = C.BoolLit b
convertBase (PikaCore.Add x y) = C.Add (convertBase x) (convertBase y)
convertBase (PikaCore.Sub x y) = C.Sub (convertBase x) (convertBase y)
convertBase (PikaCore.Equal x y) = C.Equal (convertBase x) (convertBase y)
convertBase (PikaCore.And x y) = C.And (convertBase x) (convertBase y)
convertBase (PikaCore.Not x) = C.Not (convertBase x)
convertBase e = error $ "convertBase: " ++ ppr' e

codeGenPointsTo :: PointsTo PikaCore.Expr -> Command
codeGenPointsTo ((PikaCore.V lhs :+ i) :-> rhs) =
  C.Assign (C.V (convertName lhs) :+ i) (convertBase rhs)
codeGenPointsTo p = error $ "codeGenPointsTo: " ++ ppr' p

-- convertPointsTo :: PointsTo PikaCore.Expr -> PointsTo CExpr
-- convertPointsTo (lhs :-> rhs) =
--   convertLoc lhs :-> convertBase rhs

convertLoc :: Loc PikaCore.Expr -> Loc CExpr
convertLoc (x :+ i) = convertBase x :+ i

codeGenAllocations :: [Allocation PikaCore.Expr] -> [Command] -> [Command]
codeGenAllocations [] cmds = cmds
codeGenAllocations (a:as) cmds =
  [codeGenAllocation a (codeGenAllocations as cmds)]

codeGenAllocation :: Allocation PikaCore.Expr -> [Command] -> Command
codeGenAllocation (Alloc x sz) cmds =
  C.IntoMalloc sz
    $ bind (convertName x)
        cmds

