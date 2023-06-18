--
-- An example:
--   If we already have {y :-> a, y :-> b, ...}
--   then we can replace
--      layout {x} {x :-> a, (x+1) :-> b, ...}
--   with
--      layout {} {y :-> a, (y+1) :-> b, ...}
--

{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wall #-}

module PikaC.Stage.ToPikaCore.ReuseExistingPtrs
  (reuseExistingPtrs)
  where

import PikaC.Syntax.PikaCore.Expr
import PikaC.Syntax.PikaCore.FnDef
import PikaC.Syntax.Heaplet
import PikaC.Syntax.Pika.Layout

import PikaC.Stage.ToPikaCore.Utils
import PikaC.Stage.ToPikaCore.SimplifyM
import PikaC.Utils

import Data.List

import Control.Applicative
import Control.Monad

import Unbound.Generics.LocallyNameless

reuseExistingPtrs :: Logger m => FnDef -> SimplifyM m FnDef
reuseExistingPtrs = step "reuseExistingPtrs" $ \fnDef -> do
  (inVars, bnd1) <- unbind $ _fnDefBranches fnDef
  (outVars, branches) <- unbind bnd1

  branches' <- bind inVars <$> (bind outVars <$> mapM goBranch branches)
  pure $ fnDef { _fnDefBranches = branches' }

goBranch :: Fresh m => FnDefBranch -> m FnDefBranch
goBranch branch = do
  let e = _fnDefBranchBody branch
      asns = _fnDefBranchInputAssertions branch

  e' <- go (getInputAsns asns) e
  pure $ branch { _fnDefBranchBody = e' }

go :: Fresh m => [ExprAssertion] -> Expr -> m Expr
go asns e0@(V {}) = pure e0
go asns e0@(LayoutV {}) = pure e0
go asns e0@(IntLit {}) = pure e0
go asns e0@(BoolLit {}) = pure e0
go asns (Add x y) = liftA2 Add (go asns x) (go asns y)
go asns (Sub x y) = liftA2 Sub (go asns x) (go asns y)
go asns (Equal x y) = liftA2 Equal (go asns x) (go asns y)
go asns (Not x) = fmap Not (go asns x)
go asns (And x y) = liftA2 And (go asns x) (go asns y)
go asns (App f sz xs) = App f sz <$> mapM (go asns) xs
go asns (WithIn (SslAssertion asnBnd) bodyBnd) = do
  (asnVars, asn) <- unbind asnBnd
  (bodyVars, body) <- unbind bodyBnd

  let (asnVars', asn') = getNewAsn asnVars asns asn

  WithIn (SslAssertion (bind asnVars' asn'))
    <$> (bind bodyVars
          <$> go (asn' : asns) body)

go asns (WithIn e bodyBnd) = do
  (bodyVars, body) <- unbind bodyBnd
  WithIn <$> go asns e <*> (bind bodyVars <$> go asns body)

go asns (SslAssertion bnd) = do
  (vars, asn) <- unbind bnd
  let (vars', asn') = getNewAsn vars asns asn
  pure $ SslAssertion (bind vars' asn')

getNewAsn :: [ModedName a] -> [ExprAssertion] -> ExprAssertion -> ([ModedName a], ExprAssertion)
getNewAsn [] _ asn = ([], asn)
getNewAsn names candidateAsns asn =
  case asum (map (isApplicableAsn asn) candidateAsns) of
    Nothing -> (names, asn)
    Just asn' -> ([], asn')

allPossibleRenames :: ExprAssertion -> ExprAssertion -> [[(ExprName, ExprName)]]
allPossibleRenames asn candidateAsn = map go (permutations (lhsNames asn))
  where
    go :: [ExprName] -> [(ExprName, ExprName)]
    go [] = []
    go (x:xs) = do
      y <- lhsNames candidateAsn
      (x, y) : go xs

performPossibleRenames :: ExprAssertion -> ExprAssertion -> [ExprAssertion]
performPossibleRenames asn candidateAsn = do
  sb <- allPossibleRenames asn candidateAsn
  pure $ rename sb asn

-- | Gives back the renamed assertion if the candidate is applicable
isApplicableAsn :: ExprAssertion -> ExprAssertion -> Maybe ExprAssertion
isApplicableAsn asn candidateAsn = asum $ do
  renamedAsn <- performPossibleRenames asn candidateAsn

  let renamedAsnTuples = map pointsToTuple renamedAsn
      candidateAsnTuples = map pointsToTuple candidateAsn

  if all (\z -> any (tupleEq z) candidateAsnTuples) renamedAsnTuples
    then pure $ Just renamedAsn
    else pure Nothing

pointsToTuple :: PointsTo Expr -> ((ExprName, Int), Expr)
pointsToTuple ((V x :+ i) :-> rhs) = ((x, i), rhs)

tupleEq :: ((ExprName, Int), Expr) -> ((ExprName, Int), Expr) -> Bool
tupleEq ((a, i), e) ((b, j), e') =
  a == b && i == j && aeq e e'

