{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -Wall -fprint-potential-instances #-}

module PikaC.Backend.C.CodeGen
  where

import qualified PikaC.Syntax.PikaCore.Expr as PikaCore
import qualified PikaC.Syntax.PikaCore.FnDef as PikaCore

import PikaC.Syntax.PikaCore.FnDef (FnDef (..), FnDefBranch (..), inputNames, getInputAsns)

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

import Data.List

import Control.Arrow

type Outputs = LayoutArg CExpr

codeGenFn :: PikaCore.FnDef -> C.CFunction
codeGenFn fnDef = runGenC $ do
  let PikaCore.FnName fnName = _fnDefName fnDef

  (inVars0, bnd1) <- unbind $ _fnDefBranches fnDef
  (outVars0, (_layouts, branches0)) <- unbind bnd1
  let inParams = map (convertName . modedNameName) inVars0
      outParams = map (convertName . modedNameName) outVars0

  let params = inParams ++ outParams

  -- inParamsC <- mapM fresh inParams
  outParamsC <- mapM fresh outParams

  let body = map (convertBranch outParams (_fnDefOutputSizes fnDef) outParamsC) branches0
  bodyCmd <- flattenBranchCmds (zip outParams outParamsC) inParams outParams (zip branches0 body)

  pure $ C.CFunction
    { C.cfunctionName = fnName
    , C.cfunctionParams = inParams ++ outParamsC
    , C.cfunctionBody =
        map baseDecl outParams -- TODO: Only use base type parameters here
          ++
        bodyCmd
    }

flattenBranchCmds ::
  [(C.CName, C.CName)] ->  -- (actual parameter, deref'd name)
  [C.CName] ->
  [C.CName] -> -- Output parameters
  [(FnDefBranch, ([C.CName] -> [C.Command]) -> GenC [C.Command])] -> GenC [C.Command]
flattenBranchCmds outNameMap _ outNames [] = pure [C.Nop]
flattenBranchCmds outNameMap allNames outNames ((branch, cmdsFn) : rest) = do
  let 
      baseNames = map convertName (PikaCore.inputBaseNames (_fnDefBranchInputAssertions branch))
      branchCond =
          (computeBranchCondition (allNames \\ baseNames) branchNames)
      -- cmdsFvs = toListOf fv (cmdsFn [])
  cmds <-
    cmdsFn (\cmdsFvs -> mkOutputWrites outNameMap (filter (`elem` cmdsFvs) outNames))
  restCmds <- flattenBranchCmds outNameMap allNames outNames rest
  pure $ C.IfThen branchCond cmds
    : restCmds
    -- : [C.IfThen (C.Not branchCond) restCmds]
  where
    -- baseDecls =
    --   map (C.Decl . convertName) $ PikaCore.inputBaseNames $ _fnDefBranchInputAssertions branch
    branchNames =
      -- rename outNameMap $
      map (convertName . PikaCore.getV . locBase . pointsToLhs) $ concat $ getInputAsns $ _fnDefBranchInputAssertions branch

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

convertBranch :: [C.CName] -> [Int] -> [C.CName] -> PikaCore.FnDefBranch -> ([C.CName] -> [C.Command]) -> GenC [C.Command]
convertBranch outVars outSizes actualOutVars = flip $ \k -> enterBranch $ \branch -> do
  (cond, body) <-
    enterBranchBody' (convertBranchBody outVars outSizes actualOutVars) branch
    -- codeGenAllocations (_fnDefBranchInAllocs branch)
  (condName, condCmds) <- convertBaseIntoFresh cond
  let restCmds = k $ toListOf fv body
  let body' =
        case cond of
          PikaCore.BoolLit True -> body ++ restCmds
          _ -> condCmds ++ C.whenTrue (C.V condName) (body ++ restCmds)

  setupInputs (concat (getInputAsns (_fnDefBranchInputAssertions branch))) body'

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
    go (PikaCore.V x) =
      copy (head outSizes) (getOneVar outVars) x
    go (PikaCore.LayoutV xs) = -- TODO: Make sure lengths match
        fmap concat $ sequenceA $ zipWith3 copy outSizes outVars (map PikaCore.getV xs)
    go (PikaCore.IntLit i) =
      -- pure [convertBaseInto (getOneVar outVars) (C.IntLit i)]
        pure [assignValue (getOneVar outVars) (C.IntLit i)]
    go (PikaCore.BoolLit b) =
      -- pure [convertBaseInto (getOneVar outVars) (C.BoolLit b)]
        pure [assignValue (getOneVar outVars) (C.BoolLit b)]
    go (PikaCore.Mod x y) = goBin C.Mod x y
    go (PikaCore.Div x y) = goBin C.Div x y
    go (PikaCore.Add x y) = goBin C.Add x y
    go (PikaCore.Mul x y) = goBin C.Mul x y
    go (PikaCore.Sub x y) = goBin C.Sub x y
    go (PikaCore.Equal x y) = goBin C.Equal x y
    go (PikaCore.Not x) = do
      p <- fresh $ string2Name "p"
      xCmds <- convertBaseInto p x
      -- pure [convertBaseInto (getOneVar outVars) (C.Not (convertBase x))]
      pure $ [baseDecl p]
             ++ xCmds
             ++
             [baseReassign p
             ,assignValue (getOneVar outVars) (C.Not (C.V p))
             ]
    go (PikaCore.And x y) = goBin C.And x y
    go (PikaCore.App (PikaCore.FnName f) sizes xs)
      | not (isConstructor f) && all PikaCore.isBasic xs = do
          let allocs = zipWith Alloc (map convertName outVars) outSizes
          (argNames, argCmds) <- unzip <$> mapM convertBaseIntoFresh xs
            -- TODO: Handle nested calls here?
          codeGenAllocations allocs $
            concat argCmds ++
            [C.Call f
              (map C.V argNames)
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
            ]
            ++
              case allocs of
                [Alloc v 0] -> -- Result of call has base type
                  [C.ToInt $ convertName v]
                _ -> []
            ++ body'
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

    goBin = convertBaseBin (getOneVar outVars)

convertBaseBin outVar f x y = do
  p <- fresh (string2Name "p")
  q <- fresh (string2Name "q")
  xCmds <- convertBaseInto p x
  yCmds <- convertBaseInto q y
      -- eCmd = convertBaseInto (getOneVar outVars) (f (C.V p) (C.V q))
  pure $ [baseDecl p
       ,baseDecl q
       ]
       ++ xCmds
       ++ yCmds
       ++
       [baseReassign p
       ,baseReassign q
       ,assignValue outVar (f (C.V p) (C.V q))
       ]
        -- pure [assignValue (getOneVar outVars) (f (convertBase x) (convertBase y))]

baseDecl :: C.CName -> C.Command
baseDecl n = C.IntoMalloc 1 n []

-- TODO: Is there a cleaner way? Also, is this undefined behavior?
baseReassign :: C.CName -> C.Command
baseReassign n = C.ToInt n

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

convertBaseIntoFresh :: Fresh m => AllocExpr -> m (C.CName, [C.Command])
convertBaseIntoFresh (PikaCore.V x) = pure (convertName x, [])
convertBaseIntoFresh e = do
  v <- fresh $ string2Name "p"
  cmds <- convertBaseInto v e
  pure (v, baseDecl v : cmds ++ [baseReassign v])
  -- pure (v, [baseDecl v, convertBaseInto v e, baseReassign v])

convertBaseInto :: (Fresh m, HasCallStack) => C.CName -> AllocExpr -> m [C.Command]
convertBaseInto outVar (PikaCore.V x) =
  pure [C.SimpleAssign outVar (convertName x)]
convertBaseInto outVar (PikaCore.App (PikaCore.FnName f) _ xs) = do
    -- TODO: Handle nested calls here?
  (names, cmds) <- unzip <$> mapM convertBaseIntoFresh xs
  pure $ concat cmds ++ [C.Call f (map C.V names) [C.V outVar]]
-- convertBaseInto outVar (PikaCore.V x) = pure [assignValue outVar (C.V (convertName x))]
convertBaseInto outVar (PikaCore.LayoutV [PikaCore.V x]) = pure [assignValue outVar (C.V (convertName x))]
convertBaseInto outVar (PikaCore.IntLit i) = pure [assignValue outVar (C.IntLit i)]
convertBaseInto outVar (PikaCore.BoolLit b) = pure [assignValue outVar (C.BoolLit b)]
convertBaseInto outVar (PikaCore.Mod x y) = convertBaseBin outVar C.Mod x y
convertBaseInto outVar (PikaCore.Div x y) = convertBaseBin outVar C.Div x y
convertBaseInto outVar (PikaCore.Add x y) = convertBaseBin outVar C.Add x y
convertBaseInto outVar (PikaCore.Mul x y) = convertBaseBin outVar C.Mul x y
convertBaseInto outVar (PikaCore.Sub x y) = convertBaseBin outVar C.Sub x y
convertBaseInto outVar (PikaCore.Equal x y) = convertBaseBin outVar C.Equal x y
convertBaseInto outVar (PikaCore.And x y) = convertBaseBin outVar C.And x y
convertBaseInto outVar (PikaCore.Lt x y) = convertBaseBin outVar C.Lt x y
convertBaseInto outVar (PikaCore.Not x) = do
  (name, cmds) <- convertBaseIntoFresh x
  pure $ cmds ++ [assignValue outVar (C.Not (C.V name))]
convertBaseInto _ e = error $ "convertBaseInto: " ++ ppr' e
-- convertBaseInto outVar e = 
--   pure [assignValue outVar (convertBase e)]

convertBase :: HasCallStack => AllocExpr -> CExpr
convertBase (PikaCore.V x) = C.V $ convertName x
convertBase (PikaCore.IntLit i) = C.IntLit i
convertBase (PikaCore.BoolLit b) = C.BoolLit b
convertBase (PikaCore.Mod x y) = C.Mod (convertBase x) (convertBase y)
convertBase (PikaCore.Div x y) = C.Div (convertBase x) (convertBase y)
convertBase (PikaCore.Add x y) = C.Add (convertBase x) (convertBase y)
convertBase (PikaCore.Mul x y) = C.Mul (convertBase x) (convertBase y)
convertBase (PikaCore.Sub x y) = C.Sub (convertBase x) (convertBase y)
convertBase (PikaCore.Equal x y) = C.Equal (convertBase x) (convertBase y)
convertBase (PikaCore.And x y) = C.And (convertBase x) (convertBase y)
convertBase (PikaCore.Lt x y) = C.Lt (convertBase x) (convertBase y)
convertBase (PikaCore.Not x) = C.Not (convertBase x)
convertBase e = error $ "convertBase: " ++ ppr' e

-- asInt :: CExpr -> CExpr
-- asInt (C.V x) = C.AsInt $ C.V x
-- asInt (C.Add x y) = C.Add (asInt x) (asInt y)
-- asInt (C.Mul x y) = C.Mul (asInt x) (asInt y)
-- asInt (C.Sub x y) = C.Sub (asInt x) (asInt y)
-- asInt (C.Equal x y) = C.Equal (asInt x) (asInt y)
-- asInt (C.And x y) = C.And (asInt x) (asInt y)
-- asInt (C.Not x) = C.Not (asInt x)
-- asInt e = e
--
-- assign :: CLoc -> CExpr -> Command
-- assign lhs e
--   | isNested e = C.Assign lhs (asInt e)
--   | otherwise  = C.Assign lhs e

copy ::
  Int ->
  C.CName ->
  Name (PikaCore.Expr' AllocAnnotated) ->
  GenC [Command]
copy sz cv v =
  if sz == 0
    then pure [C.Assign (C.V cv :+ 0) (C.V (convertName v))]
    else pure $ map go [0..sz-1]
  where
    go i =
      C.Assign (C.V cv :+ i) (C.Deref ((C.V (convertName v)) :+ i))

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
  codeGenAllocation a =<< codeGenAllocations as cmds

codeGenAllocation :: Allocation AllocExpr -> [Command] -> GenC [Command]
codeGenAllocation (Alloc x sz) cmds = do
  if sz == 0
    then pure $ C.Decl (convertName x) : cmds
    else pure $ [C.IntoMalloc sz
            (convertName x)
            cmds]
  -- let sz = if sz0 == 0 then 1 else sz0 -- TODO: Find a better solution
  -- pure $ C.IntoMalloc sz
  --   (convertName x)
  --   cmds

