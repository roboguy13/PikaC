module PikaC.Backend.SuSLik.CodeGen
  (codeGenIndPred)
  where

import qualified PikaC.Syntax.PikaCore.Expr as PikaCore
import qualified PikaC.Syntax.PikaCore.FnDef as PikaCore
import PikaC.Syntax.PikaCore.FnDef (inputNames, getInputAsns)
import PikaC.Syntax.Heaplet
import PikaC.Syntax.Type
import PikaC.Syntax.Pika.Layout

import PikaC.Ppr
import PikaC.Utils
import PikaC.Backend.Utils

import qualified PikaC.Backend.SuSLik.Syntax as SuSLik
import PikaC.Backend.SuSLik.Syntax (HeapletS (..))

import Unbound.Generics.LocallyNameless

import Control.Lens
import Control.Monad

import Debug.Trace

codeGenIndPred :: PikaCore.FnDef -> SuSLik.InductivePredicate
codeGenIndPred fnDef = runFreshM $ do
  let PikaCore.FnName fnName = PikaCore._fnDefName fnDef

  (inParams, bnd1) <- unbind $ PikaCore._fnDefBranches fnDef
  (outParams, branches) <- unbind bnd1

  let unmodedInParams = map (convertName . modedNameName) inParams
      unmodedOutParams = map (convertName . modedNameName) outParams

  let outSizes = PikaCore._fnDefOutputSizes fnDef

  branches <- mapM (genBranch fnName outSizes unmodedInParams unmodedOutParams) branches

  let (argTypes, resultType) = splitFnType $ PikaCore._fnDefType fnDef

  pure $ SuSLik.InductivePredicate
    { SuSLik._indPredName = fnName
    , SuSLik._indPredArgTypes = argTypes -- TODO: We need a way to deal with layouts that have multiple parameters
    , SuSLik._indPredResultType = resultType
    , SuSLik._indPredBody =
        (unmodedInParams ++ unmodedOutParams, branches)
    }

genBranch :: Fresh m => String -> [Int] -> [SuSLik.ExprName] -> [SuSLik.ExprName] -> PikaCore.FnDefBranch -> m SuSLik.PredicateBranch
genBranch fnName outSizes allNames outNames branch = do
  (zeroes, asn) <-
    getZeroes outNames =<<
    toAssertion fnName outNames (PikaCore._fnDefBranchBody branch)
  let heaplets = asn

  let branchAllocs = map (overAllocName convertName) $ findAllocations (map (string2Name . name2String) allNames) $ concat $ getInputAsns $ PikaCore._fnDefBranchInputAssertions branch
  let outAllocs = zipWith Alloc outNames outSizes

  pure $ SuSLik.PredicateBranch
    { SuSLik._predBranchPure = foldr mkAnd (boolLit True) zeroes
    , SuSLik._predBranchCond = computeBranchCondition allNames branchNames
    , SuSLik._predBranchAssertion =
        -- bind asnVars $
          map convertAlloc (outAllocs ++ branchAllocs) ++
          map convertPointsTo (concat (getInputAsns inAsns)) ++ heaplets
    }
  where
    inAsns = PikaCore._fnDefBranchInputAssertions branch
    branchNames =
      concatMap (map convertName . inputNames) inAsns

convertAlloc :: Allocation SuSLik.Expr -> SuSLik.HeapletS
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
  pure $
    -- bind []
      [ApplyS f (map convertBase args ++ map SuSLik.V outVars)
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

convertName :: PikaCore.ExprName -> SuSLik.ExprName
convertName = string2Name . show

convertModedName :: ModedName PikaCore.Expr -> SuSLik.ExprName
convertModedName (Moded _ n) = convertName n

