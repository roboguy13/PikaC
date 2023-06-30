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

import PikaC.Ppr
import PikaC.Utils
import PikaC.Backend.Utils

import qualified PikaC.Backend.SuSLik.Syntax as SuSLik
import PikaC.Backend.SuSLik.Syntax (HeapletS (..))

import Unbound.Generics.LocallyNameless

import Control.Lens
import Control.Monad

import Data.List

import Data.Maybe

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
codeGenIndPred fnDef = runFreshM $ do
  let PikaCore.FnName fnName = PikaCore._fnDefName fnDef

  (inParams, bnd1) <- unbind $ PikaCore._fnDefBranches fnDef
  (outParams, (_layouts, branches)) <- unbind bnd1

  let unmodedInParams = map (convertName . modedNameName) inParams
      unmodedOutParams = map (convertName . modedNameName) outParams

  let (argTypes, resultType) = splitFnType $ PikaCore._fnDefType fnDef
      nonBaseParams = map snd $ filter (not . isBaseType . fst) $ zip argTypes unmodedInParams

  let outSizes = PikaCore._fnDefOutputSizes fnDef

  branches' <- mapM (genBranch fnName outSizes nonBaseParams unmodedOutParams) branches


  pure $ SuSLik.InductivePredicate
    { SuSLik._indPredName = fnName
    , SuSLik._indPredArgTypes = argTypes ++ [LayoutId "Unused"] -- TODO: We need a way to deal with layouts that have multiple parameters
    , SuSLik._indPredResultType = resultType
    , SuSLik._indPredGhostTypes = []
    , SuSLik._indPredBody =
        (unmodedInParams ++ unmodedOutParams, branches')
    }

genBranch :: Fresh m => String -> [Int] -> [SuSLik.ExprName] -> [SuSLik.ExprName] -> PikaCore.FnDefBranch -> m SuSLik.PredicateBranch
genBranch fnName outSizes allNames outNames branch = do
  (zeroes, asn) <-
    getZeroes outNames =<<
    toAssertion fnName outNames (PikaCore._fnDefBranchBody branch)
  let heaplets = asn

  let branchAllocs = map (overAllocName convertName) $ findAllocations (map (string2Name . name2String) allNames) $ concat $ getInputAsns $ PikaCore._fnDefBranchInputAssertions branch
  let outAllocs = zipWith Alloc outNames outSizes
      branchAsn = map convertPointsTo (concat (getInputAsns inAsns)) ++ heaplets
      asnFVs = toListOf fv branchAsn :: [Name SuSLik.Expr]

  pure $
    -- trace ("branchNames = " ++ show branchNames) $
    -- trace ("allNames = " ++ show allNames) $
    -- trace ("branchAllocs = " ++ show branchAllocs) $
    SuSLik.PredicateBranch
    { SuSLik._predBranchPure = mkAnd (mkZeroes outNames asnFVs) $ foldr mkAnd (boolLit True) zeroes
    , SuSLik._predBranchCond =
        SuSLik.andS (computeBranchCondition allNames branchNames)
                    (convertBase (PikaCore._fnDefBranchCondition branch))
    , SuSLik._predBranchAssertion =
        -- bind asnVars $
          map convertAlloc (filter (isUsedAlloc asnFVs) (outAllocs ++ branchAllocs)) ++
          branchAsn
    }
  where
    inAsns = PikaCore._fnDefBranchInputAssertions branch
    branchNames =
      concatMap (map convertName . inputNames) inAsns

mkZeroes :: [Name SuSLik.Expr] -> [Name SuSLik.Expr] -> SuSLik.Expr
mkZeroes allNames branchNames = foldr SuSLik.And (SuSLik.BoolLit True) $ map go (allNames \\ branchNames)
  where
    go v = SuSLik.Equal (SuSLik.V v) (SuSLik.IntLit 0)

isUsedAlloc :: [Name SuSLik.Expr] -> Allocation SuSLik.Expr -> Bool
isUsedAlloc usedNames (Alloc n _) = n `elem` usedNames

convertAlloc :: Allocation SuSLik.Expr -> SuSLik.HeapletS
convertAlloc (Alloc n 0) = BlockS n 1
convertAlloc (Alloc n sz) = BlockS n sz

toAssertion :: Fresh m => String -> [SuSLik.ExprName] -> PikaCore.Expr -> m SuSLik.Assertion
toAssertion = collectAssertions

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
  String ->
  [SuSLik.ExprName] -> PikaCore.Expr ->
  m (--Bind [SuSLik.ExistVar]
        [HeapletS])
collectAssertions fnName outVars e
  | PikaCore.isBasic e =
      case outVars of
        [v] ->
          pure [PointsToS ((SuSLik.V v :+ 0) :-> convertBase e)]
        _ -> error "Expected exactly one output parameter when translating a top-level basic expression"
collectAssertions fnName outVars (PikaCore.WithIn e bnd) = do
  (vars, body) <- unbind bnd
  let unmodedVars = map modedNameName vars
  eAsns0 <- collectAssertions fnName (map convertModedName vars) e

  bodyAsns <- collectAssertions fnName outVars body

  case eAsns0 of
    [] ->
      pure $
        -- bind bodyVars $
        substs (zip (map convertName unmodedVars) (repeat (SuSLik.IntLit 0))) bodyAsns
    _ ->
      pure $
        -- bind (eVars ++ bodyVars)
          (eAsns0 ++ bodyAsns)
collectAssertions fnName outVars (PikaCore.App (PikaCore.FnName f) _sizes args) =
    -- TODO: Implement a sanity check that checks the length of outVars
    -- against sizes?
  let app = if f == fnName then RecApply else ApplyS
  in
  pure -- $ bind []
      [app f (map convertBase args ++ map SuSLik.V outVars)
      ]
collectAssertions fnName outVars (PikaCore.SslAssertion bnd) = do
  (vars, asn) <- unbind bnd
  let unmodedVars = map (convertName . modedNameName) vars
  let asn' = map convertPointsTo asn
  let asn'' = rename (zip unmodedVars outVars) asn'
  pure $ asn'' -- TODO: Bind existentials
collectAssertions fnName _ e = error $ "collectAssertions: " ++ ppr' e

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

convertBase :: PikaCore.Expr -> SuSLik.Expr
convertBase (PikaCore.V x) = SuSLik.V $ convertName x
convertBase (PikaCore.LayoutV [x]) = convertBase x
convertBase (PikaCore.LayoutV []) = SuSLik.IntLit 0 -- TODO: Is this correct?
convertBase (PikaCore.IntLit i) = SuSLik.IntLit i
convertBase (PikaCore.BoolLit b) = SuSLik.BoolLit b
convertBase (PikaCore.Add x y) = SuSLik.Add (convertBase x) (convertBase y)
convertBase (PikaCore.Mul x y) = SuSLik.Mul (convertBase x) (convertBase y)
convertBase (PikaCore.Sub x y) = SuSLik.Sub (convertBase x) (convertBase y)
convertBase (PikaCore.Equal x y) = SuSLik.Equal (convertBase x) (convertBase y)
convertBase (PikaCore.And x y) = SuSLik.And (convertBase x) (convertBase y)
convertBase (PikaCore.Not x) = SuSLik.Not (convertBase x)
convertBase (PikaCore.Lt x y) = SuSLik.Lt (convertBase x) (convertBase y)
convertBase (PikaCore.Le x y) = SuSLik.Le (convertBase x) (convertBase y)
convertBase PikaCore.EmptySet = SuSLik.EmptySet
convertBase (PikaCore.SingletonSet x) = SuSLik.SingletonSet $ convertBase x
convertBase (PikaCore.SetUnion x y) = SuSLik.SetUnion (convertBase x) (convertBase y)
convertBase e = error $ "convertBase: " ++ ppr' e

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

