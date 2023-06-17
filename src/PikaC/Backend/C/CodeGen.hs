{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -Wall -fprint-potential-instances #-}

module PikaC.Backend.C.CodeGen
  where

import qualified PikaC.Syntax.PikaCore.Expr as PikaCore
import qualified PikaC.Syntax.PikaCore.FnDef as PikaCore

import PikaC.Syntax.PikaCore.FnDef (FnDef (..), FnDefBranch (..))

import PikaC.Stage.ToPikaCore.Utils

import qualified PikaC.Backend.C.Syntax as C
import PikaC.Backend.C.Syntax (CExpr, CLoc, Command)

import PikaC.Backend.C.Monad
import PikaC.Backend.Utils

import PikaC.Stage

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
codeGenFn fnDef = runGenC $ do
  let PikaCore.FnName fnName = _fnDefName fnDef

  (inVars0, bnd1) <- unbind $ _fnDefBranches fnDef
  (outVars0, branches0) <- unbind bnd1
  let inParams = map (convertName . modedNameName) inVars0
      outParams = map (convertName . modedNameName) outVars0

  let params = inParams ++ outParams

  -- inParamsC <- mapM fresh inParams
  outParamsC <- mapM fresh outParams

  body <- mapM (convertBranch outParams (_fnDefOutputSizes fnDef) outParamsC)
            -- $ rename (zip inParams derefedInParams)
                branches0

  pure $ C.CFunction
    { C.cfunctionName = fnName
    , C.cfunctionParams = inParams ++ outParamsC
    , C.cfunctionBody =
        -- map C.Decl outParams
        --   ++
        [flattenBranchCmds (zip outParams outParamsC) inParams outParams (zip branches0 body)]
    }

flattenBranchCmds ::
  [(C.CName, C.CName)] ->  -- (actual parameter, deref'd name)
  [C.CName] ->
  [C.CName] -> -- Output parameters
  [(FnDefBranch, [C.Command])] -> C.Command
flattenBranchCmds outNameMap _ outNames [] = C.Nop
flattenBranchCmds outNameMap allNames outNames ((branch, cmds) : rest) =
  let branchCond = computeBranchCondition allNames branchNames
      cmdsFvs = toListOf fv cmds
  in
  C.IfThenElse branchCond
    (cmds ++ mkOutputWrites outNameMap (filter (`elem` cmdsFvs) outNames))
    [flattenBranchCmds outNameMap allNames outNames rest]
  where
    branchNames =
      -- rename outNameMap $
      concatMap (map (convertName . PikaCore.getV . locBase . pointsToLhs)) $ _fnDefBranchInputAssertions branch

-- | Write to the actual output parameters
mkOutputWrites :: [(C.CName, C.CName)] -> [C.CName] -> [C.Command]
mkOutputWrites _ [] = []
mkOutputWrites outNameMap (n:ns) =
  let Just actualOutParam = lookup n outNameMap
  in
  C.Assign (C.V actualOutParam :+ 0) (C.V n)
    : mkOutputWrites outNameMap ns

-- -- | Derefence the one extra layer of indirection from the actual
-- -- parameters
-- mkDerefs :: [(C.CName, C.CName)] -> C.CExpr -> [C.Command] -> [C.Command]
-- mkDerefs derefed (C.Equal x (C.IntLit 0)) body = body
-- mkDerefs derefed (C.Not (C.Equal (C.V x) (C.IntLit 0))) body =
--   let Just x' = lookup x derefed
--   in
--   [C.Let (C.V x :+ 0)
--     $ bind x' body
--   ]
-- mkDerefs derefed (C.And x y) body =
--   mkDerefs derefed x (mkDerefs derefed y body)
-- mkDerefs _ _ body = body

convertBranch :: [C.CName] -> [Int] -> [C.CName] -> PikaCore.FnDefBranch -> GenC [C.Command]
convertBranch outVars outSizes actualOutVars = enterBranch $ \branch -> do
  body <-
    enterBranchBody (convertBranchBody outVars outSizes actualOutVars) $ _fnDefBranchBody branch
    -- codeGenAllocations (_fnDefBranchInAllocs branch)
  setupInputs (concat (_fnDefBranchInputAssertions branch)) body

setupInputs :: [PointsTo PikaCore.Expr] -> [C.Command] -> GenC [C.Command]
setupInputs [] cmds = pure cmds
setupInputs (p:ps) cmds = do
  cmd <- setupInput p =<< setupInputs ps cmds
  pure [cmd]

-- | Take inputs heaplets like (x+2) :-> v and generate v = *(x+2)
setupInput :: PointsTo PikaCore.Expr -> [C.Command] -> GenC C.Command
setupInput ((PikaCore.V lhs :+ i) :-> PikaCore.V rhs) body = do
  pure $ C.Let (C.V (convertName lhs) :+ i)
    (convertName rhs)
    body
setupInput e _ = error $ "setupInput: " ++ ppr' e
  -- C.Let (convertName rhs) (C.V (convertName lhs) :+ i)

convertBranchBody ::
  [C.CName] ->
  [Int] ->
  [C.CName] ->
  PikaCore.Expr' AllocAnnotated ->
  GenC [C.Command]
convertBranchBody outVars outSizes actualOutVars = go
  where
    go :: PikaCore.Expr' AllocAnnotated -> GenC [C.Command]
    go (PikaCore.V x) = copy (head outSizes) (getOneVar outVars) x
    go (PikaCore.LayoutV xs) = -- TODO: Make sure lengths match
        fmap concat $ sequenceA $ zipWith3 copy outSizes outVars (map PikaCore.getV xs)
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
          let allocs = zipWith Alloc (map convertName outVars) outSizes
          in
          codeGenAllocations allocs
            [C.Call f
              (map convertBase xs)
              (map C.V outVars)
            ]

    go (PikaCore.App {}) = error "convertBranchBody: App should be bound by with-in if its arguments are not basic expressions"
    go e0@(PikaCore.WithIn e bnd) = do
      (vars0, body0) <- unbind bnd
      vars <- mapM freshModed vars0
      let body = rename (zip (map modedNameName vars0) (map modedNameName vars)) body0
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

              case heaplets' of
                [] ->
                  go $ substs (zip (map modedNameName vars) (repeat (PikaCore.IntLit 0))) body
                _ -> do
                  body' <- go body

                  codeGenAllocations allocs $
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
      case body of
        -- [] -> pure $ map C.SetToNull outVars
        [] -> pure $ map C.Decl outVars
        _ -> do
          let unmodedVars = map modedNameName vars
              modes = map getMode vars
              allocs = zipWith Alloc (map PikaCore.getV outVars') $ map getModedAnnotation vars

              outVars' :: [AllocExpr]
              outVars' =
                map (PikaCore.V . convertName) outVars
                -- zipWith (\m v -> Moded m (PikaCore.V (string2Name (name2String v))))
                -- zipWith (\m v -> Moded m (PikaCore.V (string2Name (show v))))
                --   modes
                --   outVars
          codeGenAllocations allocs
            (map codeGenPointsTo (substs (zip unmodedVars outVars') body)
              ++ zipWith C.Assign (map ((:+ 0) . C.V) actualOutVars) (map convertBase outVars'))
    go e = error $ "convertBranchBody: " ++ ppr' e

    goBin f x y = 
        pure [assignValue (getOneVar outVars) (f (convertBase x) (convertBase y))]

getAppArgs :: [AllocExpr] -> [C.CExpr]
getAppArgs = concatMap go
  where
    go (PikaCore.LayoutV xs) = map convertBase xs
    go e = [convertBase e]

getOneVar :: HasCallStack => [C.CName] -> C.CName
getOneVar [v] = v
getOneVar vs = error $ "Expected one variable, got " ++ show vs


-- assignVar :: C.CName -> PikaCore.ExprName -> C.Command
-- assignVar cv = assignValue cv . C.V . convertName

assignValue :: C.CName -> CExpr -> C.Command
assignValue cv = C.Assign (C.V cv :+ 0)

convertBase :: HasCallStack => AllocExpr -> CExpr
convertBase (PikaCore.V x) = C.V $ convertName x
convertBase (PikaCore.IntLit i) = C.IntLit i
convertBase (PikaCore.BoolLit b) = C.BoolLit b
convertBase (PikaCore.Add x y) = C.Add (convertBase x) (convertBase y)
convertBase (PikaCore.Sub x y) = C.Sub (convertBase x) (convertBase y)
convertBase (PikaCore.Equal x y) = C.Equal (convertBase x) (convertBase y)
convertBase (PikaCore.And x y) = C.And (convertBase x) (convertBase y)
convertBase (PikaCore.Not x) = C.Not (convertBase x)
convertBase e = error $ "convertBase: " ++ ppr' e

copy ::
  Int ->
  C.CName ->
  Name (PikaCore.Expr' AllocAnnotated) ->
  GenC [Command]
copy sz cv v = do
  if sz == 0
    then pure [C.Assign (C.V (convertName v) :+ 0) (C.V cv)]
    else pure $ map go [0..sz-1]
  where
    go i =
      C.Assign (C.V (convertName v) :+ i) (C.Deref ((C.V cv) :+ i))

codeGenPointsTo :: PointsTo AllocExpr -> Command
codeGenPointsTo ((PikaCore.V lhs :+ i) :-> rhs) =
  C.Assign (C.V (convertName lhs) :+ i) (convertBase rhs)
codeGenPointsTo p = error $ "codeGenPointsTo: " ++ ppr' p

-- convertPointsTo :: PointsTo PikaCore.Expr -> PointsTo CExpr
-- convertPointsTo (lhs :-> rhs) =
--   convertLoc lhs :-> convertBase rhs

convertLoc :: Loc AllocExpr -> Loc CExpr
convertLoc (x :+ i) = convertBase x :+ i

codeGenAllocations :: [Allocation AllocExpr] -> [Command] -> GenC [Command]
codeGenAllocations [] cmds = pure cmds
codeGenAllocations (a:as) cmds = do
  cmd <- codeGenAllocation a =<< codeGenAllocations as cmds
  pure [cmd]

codeGenAllocation :: Allocation AllocExpr -> [Command] -> GenC Command
codeGenAllocation (Alloc x sz) cmds = do
  pure $ C.IntoMalloc sz
    (convertName x)
    cmds

