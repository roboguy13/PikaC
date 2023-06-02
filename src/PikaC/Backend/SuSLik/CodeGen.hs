module PikaC.Backend.SuSLik.CodeGen
  (codeGenIndPred)
  where

import qualified PikaC.Syntax.PikaCore.Expr as PikaCore
import qualified PikaC.Syntax.PikaCore.FnDef as PikaCore
import PikaC.Syntax.Heaplet
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

  branches <- mapM (genBranch unmodedInParams unmodedOutParams) branches

  pure $ SuSLik.InductivePredicate
    { SuSLik._indPredName = fnName
    , SuSLik._indPredBody =
        bind (unmodedInParams ++ unmodedOutParams)
          branches
    }

genBranch :: Fresh m => [SuSLik.ExprName] -> [SuSLik.ExprName] -> PikaCore.FnDefBranch -> m SuSLik.PredicateBranch
genBranch allNames outNames branch = do
  asn <- toAssertion outNames $ PikaCore._fnDefBranchBody branch
  (asnVars, heaplets) <- unbind asn

  pure $ SuSLik.PredicateBranch
    { SuSLik._predBranchPure = SuSLik.BoolLit True -- TODO: Set names to zero here when needed
    , SuSLik._predBranchCond = computeBranchCondition allNames branchNames
    , SuSLik._predBranchAssertion =
        bind asnVars $
          map convertPointsTo (concat inAsns) ++ heaplets
    }
  where
    inAsns = PikaCore._fnDefBranchInputAssertions branch
    branchNames =
      concatMap (map (convertName . PikaCore.getV . locBase . pointsToLhs)) inAsns


toAssertion :: Fresh m => [SuSLik.ExprName] -> PikaCore.Expr -> m SuSLik.Assertion
toAssertion = --catAssertions <=< collectAssertions outVars
  collectAssertions

collectAssertions :: Fresh m =>
  [SuSLik.ExprName] -> PikaCore.Expr ->
  m (Bind [SuSLik.ExistVar]
        [HeapletS])
collectAssertions outVars e
  | PikaCore.isBasic e =
      case outVars of
        [v] ->
          pure $ bind [] [PointsToS ((SuSLik.V v :+ 0) :-> convertBase e)]
        _ -> error "Expected exactly one output parameter when translating a top-level basic expression"
collectAssertions outVars (PikaCore.WithIn e bnd) = do
  (vars, body) <- unbind bnd
  let unmodedVars = map modedNameName vars
  (eVars, eAsns0) <- unbind =<< collectAssertions (map convertModedName vars) e

  (bodyVars, bodyAsns) <- unbind =<< collectAssertions outVars body
  pure $
    bind (eVars ++ bodyVars)
      (eAsns0 ++ bodyAsns)

  -- let boundVars = map (SuSLik.ExistVar . convertName) unmodedVars
  -- let bound = bind boundVars bodyAsns
  -- (joinedVars, joinedAsns) <- unbind =<< joinAsnBind bound
  -- _

  -- let eAsns1 = bind unmodedVars eAsns0
  --     -- eAsns = instantiate eAsns1 (map (SuSLik.V . SuSLik.getExistVar) boundVars)
  --     eAsns = instantiate eAsns1 (map (SuSLik.V . SuSLik.getExistVar) joinedVars)
  --
  -- trace ("the vars = " ++ show (joinedVars ) ++ "bound = " ++ show bound ++ "; joinedAsns = " ++ show joinedAsns ++ "; eAsns = " ++ show eAsns ++ "; eAsns1 = " ++ show eAsns1) $
  --   pure $ bind (joinedVars ++ eVars)
  --             (eAsns ++ joinedAsns)
    -- TODO: How should we bind the existentials ('vars') here?
  -- pure (eAsns ++ bodyAsns)
collectAssertions outVars (PikaCore.App (PikaCore.FnName f) _sizes args) =
    -- TODO: Implement a sanity check that checks the length of outVars
    -- against sizes?
  pure $
    bind []
      [ApplyS f (map convertBase args ++ map SuSLik.V outVars)
      ]
collectAssertions outVars (PikaCore.SslAssertion bnd) = do
  (vars, asn) <- unbind bnd
  let unmodedVars = map (convertName . modedNameName) vars
      asn' = map convertPointsTo asn
      asn'' = rename (zip unmodedVars outVars) asn'
  pure $ bind [] asn'' -- TODO: Bind existentials
collectAssertions _ e = error $ "collectAssertions: " ++ ppr' e

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
  pure $ bind (vars1 ++ map SuSLik.ExistVar vars2') body'

sequenceAssertions :: Fresh m =>
  [SuSLik.Assertion] -> m (Bind [SuSLik.ExistVar] [HeapletS])
sequenceAssertions [] = pure $ bind [] []
sequenceAssertions (x:xs) = do
  (vars, hs) <- unbind x
  (vars', hs') <- unbind =<< sequenceAssertions xs
  pure $ bind (vars ++ vars') (hs ++ hs')

-- extendAssertionBind :: [ExprN

convertBase :: PikaCore.Expr -> SuSLik.Expr
convertBase (PikaCore.V x) = SuSLik.V $ convertName x
convertBase (PikaCore.LayoutV []) = SuSLik.IntLit 0 -- TODO: Is this correct?
convertBase (PikaCore.IntLit i) = SuSLik.IntLit i
convertBase (PikaCore.BoolLit b) = SuSLik.BoolLit b
convertBase (PikaCore.Add x y) = SuSLik.Add (convertBase x) (convertBase y)
convertBase (PikaCore.Sub x y) = SuSLik.Sub (convertBase x) (convertBase y)
convertBase (PikaCore.Equal x y) = SuSLik.Equal (convertBase x) (convertBase y)
convertBase (PikaCore.And x y) = SuSLik.And (convertBase x) (convertBase y)
convertBase (PikaCore.Not x) = SuSLik.Not (convertBase x)
convertBase e = error $ "convertBase: " ++ ppr' e

splitAssertions :: Fresh m =>
  [SuSLik.Assertion] -> m ([SuSLik.ExistVar], [HeapletS])
splitAssertions [] = pure ([], [])
splitAssertions (bnd:asns) = do
  (existVars, asn) <- unbind bnd
  (restVars, restHeaplets) <- splitAssertions asns
  pure (existVars ++ restVars, asn ++ restHeaplets)

catAssertions :: Fresh m => [SuSLik.Assertion] -> m SuSLik.Assertion
catAssertions xs = do
  (vars, heaplets) <- splitAssertions xs
  pure $ bind vars heaplets

convertName :: PikaCore.ExprName -> SuSLik.ExprName
convertName = string2Name . show

convertModedName :: ModedName PikaCore.Expr -> SuSLik.ExprName
convertModedName (Moded _ n) = convertName n

