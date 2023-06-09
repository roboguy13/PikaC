module PikaC.Backend.SuSLik.CodeGen
  (codeGenSynth
  ,codeGenSynthSig
  ,codeGenFnSig
  ,codeGenLayout
  ,codeGenIndPred)
  where

import qualified PikaC.Syntax.PikaCore.Expr as PikaCore
import qualified PikaC.Syntax.PikaCore.FnDef as PikaCore
import PikaC.Syntax.PikaCore.FnDef (inputNames, getInputAsns)
import PikaC.Syntax.Heaplet
import PikaC.Syntax.Type
import PikaC.Syntax.Pika.Layout
import PikaC.Syntax.Pika.Pattern
import PikaC.Syntax.Pika.FnDef

import qualified PikaC.Stage.ToPikaCore as ToPikaCore
import PikaC.Stage.ToPikaCore.SimplifyM (runSimplifyQuiet, SimplifyFuel (..), fixedPoint)
import PikaC.Stage.ToPikaCore.Utils
import PikaC.Stage.ToPikaCore.BaseAppToWith
import PikaC.Stage.ToPikaCore.FloatWith

import PikaC.Ppr
import PikaC.Utils
import PikaC.Backend.Utils

import qualified PikaC.Backend.SuSLik.Syntax as SuSLik
import PikaC.Backend.SuSLik.Syntax (HeapletS (..), CompoundAsn (..), mkPurePart, mkSpatialPart, mkPredicateBranch, spatialPart, purePart, _RecApply, _ApplyS)

import Unbound.Generics.LocallyNameless

import Control.Lens
import Control.Monad

import Data.Equivalence.Monad

import Data.List

import Data.Maybe

import GHC.Stack

import Debug.Trace

codeGenSynth :: [Layout PikaCore.Expr] -> Synth -> ([SuSLik.InductivePredicate], SuSLik.FnSig)
codeGenSynth layouts synth =
  let sig = codeGenSynthSig layouts synth
  in
  (map (codeGenLayout True) layouts, sig)

unGhostApp :: Type -> (String, [String])
unGhostApp (GhostApp (LayoutId v) args) = (v, args)

codeGenSynthSig :: [Layout PikaCore.Expr] -> Synth -> SuSLik.FnSig
codeGenSynthSig layouts (Synth fn purePart args (GhostApp (LayoutId resultType) resultGhostArgs)) = runFreshM $ do

  let (argLayoutNames, argsGhosts) = unzip $ map unGhostApp args

  let argLayouts = map (lookupLayout layouts) argLayoutNames
      resultLayout = lookupLayout layouts resultType

  let getArgParams argLayout = do
        (_, argBnd) <- unbind $ _layoutBranches argLayout
        (argParams, _) <- unbind argBnd
        pure argParams

  argsParams <- mapM getArgParams argLayouts

  -- (_, argBnd) <- unbind $ _layoutBranches argLayout
  -- (argParams, _) <- unbind argBnd
  --
  (_, resultBnd) <- unbind $ _layoutBranches resultLayout
  (resultParams, _) <- unbind resultBnd

  let resultParams' = map (convertName . modedNameName) resultParams

  (_, precondOuts) <- mkOutPointsTos resultParams'
  (postCondOutVars, postCondOuts) <- mkOutPointsTos resultParams'

  let params = map (convertName . modedNameName) (concat argsParams) ++ map (convertName . modedNameName) resultParams

      mkAlloc argType argParams argGhostArgs =
        Just $ PikaCore.ArgLayout argType
          $ map modedNameName argParams ++ map string2Name argGhostArgs

      allocs = mkLayoutApps (zipWith3 mkAlloc argLayoutNames argsParams argsGhosts)

      -- allocs = mkLayoutApps
      --           [Just $
      --             PikaCore.ArgLayout
      --               (name2String argType)
      --               $ map modedNameName argParams ++ map string2Name argGhostArgs]

      outAllocs = -- allocs ++
        mkLayoutApps [Just $ PikaCore.ArgLayout resultType $ map (string2Name . show) postCondOutVars ++ map string2Name resultGhostArgs]

      spec = SuSLik.FnSpec
               { SuSLik._fnSpecPrecond = allocs ++ precondOuts
               , SuSLik._fnSpecPostcond = (purePart', postCondOuts ++ outAllocs)
               }

      purePart' = convertBase $ ToPikaCore.convertBasicExpr purePart

  pure $ SuSLik.FnSig
    { SuSLik._fnSigName = fn
    , SuSLik._fnSigArgTypes = replicate (length (concat argsParams) + length resultParams) (LayoutId "Unused")
    , SuSLik._fnSigResultType = LayoutId "Unused"
    , SuSLik._fnSigConds = (params, spec)
    }

codeGenFnSig :: PikaCore.FnDef -> SuSLik.FnSig
codeGenFnSig fnDef = runFreshM $ do
  let PikaCore.FnName fnName = PikaCore._fnDefName fnDef

  (inParams, bnd1) <- unbind $ PikaCore._fnDefBranches fnDef
  (outParams, (layouts, branches)) <- unbind bnd1

  let inParams' = map (convertName . modedNameName) inParams
  let outParams' = map (convertName . modedNameName) outParams

  let params = map (convertName . modedNameName) $ inParams ++ outParams
  let allocs = mkLayoutApps layouts

  (_, precondOuts) <- mkOutPointsTos outParams'
  (postCondOutVars, postCondOuts) <- mkOutPointsTos outParams'

  let spec = SuSLik.FnSpec
              { SuSLik._fnSpecPrecond = allocs ++ precondOuts
              , SuSLik._fnSpecPostcond =
                  (SuSLik.BoolLit True
                  ,[RecApply fnName (map mkVar (inParams' ++ postCondOutVars))]
                    ++ postCondOuts
                  )
              }
  let (argTypes, resultType) = splitFnType $ PikaCore._fnDefType fnDef

  pure $ SuSLik.FnSig
    { SuSLik._fnSigName = fnName
    , SuSLik._fnSigArgTypes = argTypes
    , SuSLik._fnSigResultType = resultType
    , SuSLik._fnSigConds = (params, spec)
    }

codeGenLayout :: Bool -> Layout PikaCore.Expr -> SuSLik.InductivePredicate
codeGenLayout useGhosts layout = runFreshM $ do
  (ghosts, bnd) <- unbind $ _layoutBranches layout
  (params, branches) <- unbind bnd
  let params' = map (convertName . modedNameName) params
  branches' <- mapM (codeGenLayoutBranch useGhosts params') branches

  let params'' =
        if useGhosts
          then params' ++ map (convertName . getGhostName) ghosts
          else params'

  pure $ SuSLik.InductivePredicate
    { SuSLik._indPredName = _layoutName layout
    , SuSLik._indPredArgTypes = map (const (LayoutId "Unused")) params
    , SuSLik._indPredResultType = LayoutId "Unused2" --resultType
    , SuSLik._indPredGhostTypes =
        if useGhosts
          then map getGhostType ghosts
          else []
    , SuSLik._indPredBody = (params'', branches')
        -- (unmodedInParams ++ unmodedOutParams, branches')
    }

codeGenLayoutBranch :: Fresh m => Bool -> [SuSLik.ExprName] -> LayoutBranch PikaCore.Expr -> m SuSLik.PredicateBranch
codeGenLayoutBranch useGhosts allNames (LayoutBranch (PatternMatch bnd)) = do
  (pat, bnd1) <- unbind bnd
  (existVars, (GhostCondition gCond body@(LayoutBody asn))) <- unbind bnd1
  -- (zeroes, asn) <-
    -- getZeroes outNames =<<
    -- toAssertion fnName outNames (PikaCore._fnDefBranchBody branch)
  let heaplets = asn
      branchNames =
        map convertName $ toListOf fv asn
      -- allNames' = 

  let branchAllocs = map (overAllocName convertName) $ findAllocations (map convertName' allNames) $ getPointsTos body
  -- let outAllocs = zipWith Alloc outNames outSizes
  pure
    $ SuSLik.PredicateBranch
    { SuSLik._predBranchPure =
        if useGhosts
          then fromMaybe (boolLit True) $ fmap convertBase gCond
          else boolLit True
    , SuSLik._predBranchCond = computeBranchCondition allNames branchNames
    , SuSLik._predBranchAssertion =
        -- bind asnVars $
          map convertAlloc branchAllocs ++
          convertLayoutBody useGhosts body
          -- map convertPointsTo (concat (getInputAsns inAsns))
    }
  where
    -- inAsns = PikaCore._fnDefBranchInputAssertions branch

convertLayoutBody :: Bool -> LayoutBody PikaCore.Expr -> [HeapletS]
convertLayoutBody useGhosts = map go . _unLayoutBody
  where
    go (LPointsTo p) =
      let PointsToS q = convertPointsTo p
      in
      -- ReadOnlyPointsToS q
      PointsToS q
    go (LApply n ghosts _ vs) =
      let args = if useGhosts
                 then vs ++ ghosts
                 else vs
      in
      RecApply n (map (mkVar . convertName . PikaCore.getV) args)

mkOutPointsTos :: Fresh m => [SuSLik.ExprName] -> m ([SuSLik.ExprName], [HeapletS])
mkOutPointsTos [] = pure ([], [])
mkOutPointsTos (x:xs) = do
  x' <- fresh x
  let p = bimap (x':) (PointsToS ((mkVar x :+ 0) :-> mkVar x') :)
  rest <- mkOutPointsTos xs
  pure (p rest)

mkLayoutApps :: [Maybe PikaCore.ArgLayout] -> [HeapletS]
mkLayoutApps = map go . catMaybes
  where
    go (PikaCore.ArgLayout n vs) =
      RecApply n (map (mkVar . convertName) vs)  -- Not actually recursive, but this generates the correct kind of predicate application

codeGenIndPred :: PikaCore.FnDef -> SuSLik.InductivePredicate
codeGenIndPred fnDef0 = runFreshM $ do
    -- Some preprocessing on the PikaCore:
  let fnDef =
        runSimplifyQuiet Unlimited
          (fixedPoint (onFnDef (floatWith <=< baseAppToWith)))
          fnDef0

  let PikaCore.FnName fnName = PikaCore._fnDefName fnDef

  (inParams, bnd1) <- unbind $ PikaCore._fnDefBranches fnDef
  (outParams, (_layouts, branches)) <- unbind bnd1

  let unmodedInParams = map (convertName . modedNameName) inParams
      unmodedOutParams = map (convertName . modedNameName) outParams

  let (argTypes, resultType) = splitFnType $ PikaCore._fnDefType fnDef
      nonBaseParams = map snd $ filter (not . isBaseType . fst) $ zip argTypes unmodedInParams

  let outSizes = PikaCore._fnDefOutputSizes fnDef

  branches' <- mapM (genBranch fnName outSizes nonBaseParams unmodedOutParams) branches


  pure 
    -- $ trace ("---\n" ++ ppr' fnDef)
    $ SuSLik.InductivePredicate
    { SuSLik._indPredName = fnName
    , SuSLik._indPredArgTypes = argTypes ++ [LayoutId "Unused"] -- TODO: We need a way to deal with layouts that have multiple parameters
    , SuSLik._indPredResultType = resultType
    , SuSLik._indPredGhostTypes = []
    , SuSLik._indPredBody =
        (unmodedInParams ++ unmodedOutParams, branches')
    }

genBranch :: Fresh m => String -> [Int] -> [SuSLik.ExprName] -> [SuSLik.ExprName] -> PikaCore.FnDefBranch -> m SuSLik.PredicateBranch
genBranch fnName outSizes allNames outNames branch = do
  asn <- toAssertion fnName outNames (PikaCore._fnDefBranchBody branch)
  (zeroes0, heaplets) <- getZeroes outNames (view spatialPart asn)
  let zeroes = map snd . filter ((> 0) . fst) $ zip outSizes zeroes0
  -- let heaplets = getSpatialPart asn

  -- let branchAllocs = map (overAllocName convertName) $ findAllocations (map (string2Name . name2String) allNames) $ concat $ getInputAsns $ PikaCore._fnDefBranchInputAssertions branch
  let branchAsn = map convertPointsTo (concat (getInputAsns inAsns)) ++ heaplets
      inputAsn = concat $ getInputAsns $ PikaCore._fnDefBranchInputAssertions branch
  let branchAllocs = map (overAllocName convertName) $ findAllocations (map convertName' allNames) (mapMaybe toPointsTo heaplets ++ inputAsn)
  let outAllocs = zipWith Alloc outNames outSizes
      asnFVs = toListOf fv branchAsn :: [Name SuSLik.Expr]
      namesUsedInApps = appParameters branchAsn

      branchPurePart = mkAnd (mkAnd (mkZeroes outSizes outNames asnFVs) $ foldr mkAnd (boolLit True) zeroes) (view purePart asn)
      notUsedInApp = (not . any (`elem` namesUsedInApps) . (`getEquals` branchPurePart) ) 

  pure $
    -- trace ("inputAsn = " ++ show inputAsn ++ ", branchAllocs = " ++ show branchAllocs) $
    -- trace ("branchNames = " ++ show branchNames) $
    -- trace ("allNames = " ++ show allNames) $
    -- trace ("branchAllocs = " ++ show branchAllocs) $
    SuSLik.PredicateBranch
    { SuSLik._predBranchPure = branchPurePart
    , SuSLik._predBranchCond =
        SuSLik.andS (computeBranchCondition allNames branchNames)
                    (convertBase (PikaCore._fnDefBranchCondition branch))
    , SuSLik._predBranchAssertion =
        -- bind asnVars $
          map convertAlloc (filter (liftA2 (&&) (notUsedInApp . allocName) (isUsedAlloc asnFVs)) (outAllocs ++ branchAllocs)) ++
          -- map convertAlloc (outAllocs ++ branchAllocs) ++
          mapMaybe (\(f, xs) -> mkRecApplyMaybe f (map SuSLik.V (filter notUsedInApp (map (SuSLik.getV . convertBase) xs)))) (PikaCore.getLayoutApps (PikaCore.getInputAsns' inAsns)) ++
          branchAsn
    }
  where
    inAsns = PikaCore._fnDefBranchInputAssertions branch
    branchNames =
      concatMap (map convertName . inputNames) inAsns

mkRecApplyMaybe :: String -> [SuSLik.Expr] -> Maybe HeapletS
mkRecApplyMaybe _ [] = Nothing
mkRecApplyMaybe f xs = Just $ RecApply f xs

toPointsTo :: HeapletS -> Maybe (PointsTo PikaCore.Expr)
toPointsTo (PointsToS p) = Just (mapPointsTo (mkVar . convertName' . SuSLik.getV) p)
toPointsTo (ReadOnlyPointsToS p) = Just (mapPointsTo (mkVar . convertName' . SuSLik.getV) p)
toPointsTo _ = Nothing

mkZeroes :: [Int] -> [Name SuSLik.Expr] -> [Name SuSLik.Expr] -> SuSLik.Expr
mkZeroes outSizes allNames0 branchNames =
  foldr mkAnd (SuSLik.BoolLit True)
    $ map go (allNames \\ branchNames)
  where
    allNames = map snd . filter ((> 0) . fst) $ zip outSizes allNames0
    go v = SuSLik.Equal (SuSLik.V v) (SuSLik.IntLit 0)

isUsedAlloc :: [Name SuSLik.Expr] -> Allocation SuSLik.Expr -> Bool
isUsedAlloc usedNames (Alloc n _) = n `elem` usedNames

convertAlloc :: Allocation SuSLik.Expr -> SuSLik.HeapletS
convertAlloc (Alloc n 0) = BlockS n 1
convertAlloc (Alloc n sz) = BlockS n sz

toAssertion :: Fresh m => String -> [SuSLik.ExprName] -> PikaCore.Expr -> m CompoundAsn
toAssertion = collectAssertions []

getZeroes :: Fresh m => [SuSLik.ExprName] -> SuSLik.Assertion -> m ([SuSLik.Expr], SuSLik.Assertion)
getZeroes outVars bnd = do
  let hs = bnd
  -- hs <- unbind bnd
  case hs of
    [] -> pure ([], [])
    (PointsToS ((SuSLik.V x :+ 0) :-> SuSLik.IntLit 0) : rest)
      | x `elem` outVars -> do
          (restExprs, restAsn) <- getZeroes outVars rest
          pure (mkEqual (mkVar x) (intLit 0) : restExprs
               ,restAsn
               )
    (h : rest) -> do
          (restExprs, restAsn) <- getZeroes outVars rest
          let restHs = restAsn
          pure (restExprs, h : restHs)

collectAssertions :: Fresh m =>
  [PikaCore.ExprName] ->   -- | Application result names, for use in determining temploc's
  String ->
  [SuSLik.ExprName] -> PikaCore.Expr ->
  m CompoundAsn
collectAssertions appOutNames fnName outVars e
  | PikaCore.isBasic e =
      case outVars of
        [v] ->
          convertBaseAsn v e
          -- pure $ mkSpatialPart [PointsToS ((SuSLik.V v :+ 0) :-> convertBase e)]
        _ -> error "Expected exactly one output parameter when translating a top-level basic expression"
-- collectAssertions fnName outVars (PikaCore.WithIn (App f [0] args) bnd) = do
--   ([v], body) <- unbind bnd
--   let unmodedV = modedNameName v
--
--   let argAsns = map convertBase args
--
--   let asn = (Equal (V v) (convertBase app)) :& 

collectAssertions appOutNames0 fnName outVars (PikaCore.WithIn e bnd) = do
  (vars, body) <- unbind bnd
  let unmodedVars = map modedNameName vars

      appOutNames = case e of
                      PikaCore.App {} -> unmodedVars ++ appOutNames0
                      _ -> appOutNames0

  eAsns0 <- collectAssertions appOutNames0 fnName (map convertModedName vars) e

  bodyAsns <- collectAssertions appOutNames fnName outVars body

  let appOutUsages =
        filter (`elem` appOutNames) (toListOf (traversed . PikaCore._V) (children body))
          ++
        filter (`elem` appOutNames) (toListOf (traversed . PikaCore._LayoutV . traversed . PikaCore._V) (children body))
      tempLocs = map (TempLoc . convertName) appOutUsages
      tempLocAsn =
        case body of
          PikaCore.App {} -> mkSpatialPart tempLocs
          _ -> mempty

  case view spatialPart eAsns0 of
    [] ->
      pure $
        tempLocAsn
          <>
        (bodyAsns &
          spatialPart %~ substs (zip (map convertName unmodedVars) (repeat (SuSLik.IntLit 0))))
    _ ->
      pure (tempLocAsn <> eAsns0 <> bodyAsns)
collectAssertions appOutNames fnName outVars (PikaCore.App (PikaCore.FnName f) _sizes args) =
    -- TODO: Implement a sanity check that checks the length of outVars
    -- against sizes?
  let app = if f == fnName then RecApply else ApplyS
  in
  pure -- $ bind []
    $ mkSpatialPart
      [app f (map convertBase args ++ map SuSLik.V outVars)
      ]
collectAssertions appOutNames fnName outVars (PikaCore.SslAssertion bnd) = do
  (vars, asn) <- unbind bnd
  let unmodedVars = map (convertName . modedNameName) vars
  let asn' = map convertPointsTo asn
  let asn'' = rename (zip unmodedVars outVars) asn'
  pure $ mkSpatialPart asn'' -- TODO: Bind existentials
collectAssertions appOutNames fnName _ e = error $ "collectAssertions: " ++ ppr' e

convertPointsTo :: PointsTo PikaCore.Expr -> HeapletS
convertPointsTo (x :-> y) =
  PointsToS (fmap convertBase x :-> convertBase y)

joinAsnBind :: Fresh m =>
  Bind [SuSLik.ExistVar] (Bind [SuSLik.ExistVar] [HeapletS])
  -> m SuSLik.Assertion
joinAsnBind bnd1 = do
  (vars1, bnd2) <- unbind bnd1
  (vars2, body) <- unbind bnd2
  vars2' <- mapM (fresh . SuSLik.getExistVar) vars2
  let body' = rename (zip (map SuSLik.getExistVar vars2) vars2') body
  pure $ -- bind (vars1 ++ map SuSLik.ExistVar vars2')
    body'

sequenceAssertions :: Fresh m =>
  [SuSLik.Assertion] -> m [HeapletS]
sequenceAssertions [] = pure []
sequenceAssertions (x:xs) = do
  let hs = x
  hs' <- sequenceAssertions xs
  pure -- $ bind (vars ++ vars')
    (hs ++ hs')

-- extendAssertionBind :: [ExprN

convertBaseAsn :: Fresh m => SuSLik.ExprName -> PikaCore.Expr -> m CompoundAsn
-- convertBaseAsn outVar e
--   | Just r <- convertBaseMaybe e =
--       pure $ mkPurePart (SuSLik.Equal (SuSLik.V outVar) r)
convertBaseAsn outVar (PikaCore.V x) =
  pure $ mkPurePart (SuSLik.Equal (SuSLik.V outVar) (SuSLik.V (convertName x)))
convertBaseAsn outVar (PikaCore.LayoutV [x]) = convertBaseAsn outVar x
convertBaseAsn outVar (PikaCore.IntLit i) =
  pure $ mkPurePart (SuSLik.Equal (SuSLik.V outVar) (SuSLik.IntLit i))
convertBaseAsn outVar (PikaCore.BoolLit b) =
  pure $ mkPurePart (SuSLik.Equal (SuSLik.V outVar) (SuSLik.BoolLit b))
convertBaseAsn outVar (PikaCore.Add x y) = convertBin SuSLik.Add outVar x y
convertBaseAsn outVar (PikaCore.Mul x y) = convertBin SuSLik.Mul outVar x y
convertBaseAsn outVar (PikaCore.Sub x y) = convertBin SuSLik.Sub outVar x y
convertBaseAsn outVar (PikaCore.Equal x y) = convertBin mkEqual outVar x y
convertBaseAsn outVar (PikaCore.And x y) = convertBin SuSLik.Mul outVar x y
convertBaseAsn outVar (PikaCore.Lt x y) = convertBin SuSLik.Lt outVar x y
convertBaseAsn outVar (PikaCore.Le x y) = convertBin SuSLik.Le outVar x y
convertBaseAsn outVar (PikaCore.App (PikaCore.FnName f) [0] xs) = do
    -- TODO: Check to see if this is recursive call?
  vars <- mapM (fresh . string2Name . const "zz") xs
  xsAsns <- zipWithM convertBaseAsn vars xs
  let resultSpatial = [SuSLik.RecApply f (map SuSLik.V vars ++ [SuSLik.V outVar])]
  pure $ mconcat xsAsns <> mkSpatialPart resultSpatial
convertBaseAsn outVar e =
    error $ "convertBaseAsn: " ++ ppr' e

convertBin :: Fresh m =>
  (SuSLik.Expr -> SuSLik.Expr -> SuSLik.Expr) ->
  SuSLik.ExprName ->
  PikaCore.Expr ->
  PikaCore.Expr ->
  m CompoundAsn
convertBin f outVar x y = do
  varX <- fresh (string2Name "xx" :: SuSLik.ExprName)
  varY <- fresh (string2Name "yy" :: SuSLik.ExprName)
  asnX <- convertBaseAsn varX x
  asnY <- convertBaseAsn varY y

  let newEq = mkEqual (SuSLik.V outVar) (f (SuSLik.V varX) (SuSLik.V varY))
  pure $ asnX <> asnY <> mkPurePart newEq

convertBaseMaybe :: PikaCore.Expr -> Maybe SuSLik.Expr
convertBaseMaybe (PikaCore.V x) = Just $ SuSLik.V $ convertName x
convertBaseMaybe (PikaCore.LayoutV [x]) = convertBaseMaybe x
convertBaseMaybe (PikaCore.LayoutV []) = Just $ SuSLik.IntLit 0 -- TODO: Is this correct?
convertBaseMaybe (PikaCore.IntLit i) = Just $ SuSLik.IntLit i
convertBaseMaybe (PikaCore.BoolLit b) = Just $ SuSLik.BoolLit b
convertBaseMaybe (PikaCore.Add x y) = liftA2 SuSLik.Add (convertBaseMaybe x) (convertBaseMaybe y)
convertBaseMaybe (PikaCore.Mul x y) = liftA2 SuSLik.Mul (convertBaseMaybe x) (convertBaseMaybe y)
convertBaseMaybe (PikaCore.Sub x y) = liftA2 SuSLik.Sub (convertBaseMaybe x) (convertBaseMaybe y)
convertBaseMaybe (PikaCore.Equal x y) = liftA2 mkEqual (convertBaseMaybe x) (convertBaseMaybe y)
convertBaseMaybe (PikaCore.And x y) = liftA2 mkAnd (convertBaseMaybe x) (convertBaseMaybe y)
convertBaseMaybe (PikaCore.Not x) = fmap SuSLik.Not (convertBaseMaybe x)
convertBaseMaybe (PikaCore.Lt x y) = liftA2 SuSLik.Lt (convertBaseMaybe x) (convertBaseMaybe y)
convertBaseMaybe (PikaCore.Le x y) = liftA2 SuSLik.Le (convertBaseMaybe x) (convertBaseMaybe y)
convertBaseMaybe PikaCore.EmptySet = Just SuSLik.EmptySet
convertBaseMaybe (PikaCore.SingletonSet x) = fmap SuSLik.SingletonSet $ convertBaseMaybe x
convertBaseMaybe (PikaCore.SetUnion x y) = liftA2 SuSLik.SetUnion (convertBaseMaybe x) (convertBaseMaybe y)
convertBaseMaybe _ = Nothing

convertBase :: HasCallStack => PikaCore.Expr -> SuSLik.Expr
convertBase e =
  case convertBaseMaybe e of
    Just r -> r
    Nothing -> error $ "convertBase: " ++ ppr' e

-- | Get parameters of predicate applications so that we don't apply a layout
-- predicate to them
appParameters :: SuSLik.Assertion -> [SuSLik.ExprName]
appParameters =
  liftA2 (++)
    (toListOf (traversed . _RecApply . _2 . traversed . SuSLik._V))
    (toListOf (traversed . _ApplyS . _2 . traversed . SuSLik._V))

getEquals :: SuSLik.ExprName -> SuSLik.Expr -> [SuSLik.ExprName]
getEquals v e = v : eqNames
  where
    eqNames :: [SuSLik.ExprName]
    eqNames = runEquivM (:[]) (++) $ do
      go e
      desc =<< getClass v

    go (SuSLik.And x y) = go x *> go y
    go (SuSLik.Equal (SuSLik.V x) (SuSLik.V y)) = equate x y
    go _ = pure ()

-- -- Get the x's such that there's
-- --      with {x} := f e ...
-- --      in
-- --      ...
-- getAppResultVars :: PikaCore.Expr -> [PikaCore.ExprName]
-- getAppResultVars = concatMap go . universe
--   where
--     go (WithIn (App {}) bnd) = undefined

-- genTempLocs :: 

splitAssertions :: Fresh m =>
  [SuSLik.Assertion] -> m ([HeapletS])
splitAssertions [] = pure []
splitAssertions (bnd:asns) = do
  let asn = bnd
  -- (restVars, restHeaplets) <- splitAssertions asns
  restHeaplets <- splitAssertions asns
  pure
    ( --existVars ++ restVars,
    asn ++ restHeaplets)

catAssertions :: Fresh m => [SuSLik.Assertion] -> m SuSLik.Assertion
catAssertions xs = do
  heaplets <- splitAssertions xs
  pure heaplets

convertName' :: Name a -> Name b
convertName' n = --string2Name . name2String
  makeName (name2String n) (name2Integer n)

convertName :: PikaCore.ExprName -> SuSLik.ExprName
convertName = convertName'

convertModedName :: ModedName PikaCore.Expr -> SuSLik.ExprName
convertModedName (Moded _ n) = convertName n

