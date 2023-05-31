{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module PikaC.Syntax.PikaCore.FnDef
  where

import PikaC.Syntax.PikaCore.Expr
import PikaC.Syntax.Pika.Layout
import PikaC.Syntax.Heaplet
import PikaC.Ppr
import PikaC.Utils

import Control.Lens.TH

import Debug.Trace

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Bind

import GHC.Generics

import Data.GenValidity
import Data.Validity

import Control.Lens hiding (elements)
import Data.List

import Unbound.Generics.LocallyNameless.Unsafe -- Just for implementing QuickCheck shrinking

import Test.QuickCheck
import Control.Monad

data FnDef =
  FnDef
  { _fnDefName :: FnName
  , _fnDefBranches ::
      Bind [ModedName Expr]    -- Input parameters
        (Bind [ModedName Expr] -- Output parameters
           [FnDefBranch])
  -- , _fnDefParams :: [ModedName Expr]
  }
  deriving (Show, Generic)

data FnDefBranch =
  FnDefBranch
  -- { _fnDefOutputParams :: [ModedName Expr]
  { _fnDefBranchInputAssertions :: [ExprAssertion]
  , _fnDefBranchInAllocs :: [Allocation Expr]
  , _fnDefBranchBody :: Expr
  }
  deriving (Show, Generic)

makeLenses ''FnDef
makeLenses ''FnDefBranch

instance Alpha FnDefBranch
instance Alpha FnDef

-- fnDefInputNames :: FnDef -> [ExprName]
-- fnDefInputNames = concatMap fnDefBranchInputNames . _fnDefBranches

-- fnDefBranchInputNames :: FnDefBranch -> [ExprName]
-- fnDefBranchInputNames =
--   concatMap pointsToNames . _fnDefBranchInputAssertions

instance Subst Expr FnDefBranch

instance Subst (Moded Expr) FnDefBranch

instance Ppr FnDef where
  ppr def = runFreshM $ do
      let bnd1@(B vs _) = openBind (_fnDefBranches def)
      vs' <- mapM fresh (concatMap getNames vs)
      let modedVs' = zipWith Moded (map getMode vs) vs'
      let branches = instantiate bnd1 (map V vs')
      go modedVs' branches
    where
      go :: [ModedName Expr] -> [FnDefBranch] -> FreshM Doc
      go _ [] = pure mempty
      go outParams (x:xs) = do
        xDoc <- pprBranch outParams (ppr (_fnDefName def)) x
        restDoc <- go outParams xs
        pure (xDoc $$ restDoc)
      -- go :: Bind [ModedName Expr] [FnDefBranch] -> FreshM Doc
      -- go bnd =
      --   let (B bndVars _) = bnd
      --       modes = map getMode bndVars
      --   in
      --   freshOpen @_ @Name bnd >>= \case
      --     (_, []) -> pure mempty
      --     (vs, branch : rest) -> do
      --       let modedVs = zipWith Moded modes vs
      --       doc <- pprBranch (ppr (_fnDefName def)) (bind modedVs branch)
      --       rest <- go $ bind modedVs rest
      --       pure (doc $$ rest)
      -- -- go (B _ []) = mempty
      -- -- go (B vs (branch : rest)) = pprBranch (ppr (_fnDefName def)) (B vs branch) $$ go (B vs rest)

-- instance Ppr FnDefBranch where
--   ppr = pprBranch mempty

-- instance Show FnDef where
--   show = ppr'
--
-- instance Show FnDefBranch where
--   show = render . runFreshM . pprBranch [] mempty

pprBranch :: [ModedName Expr] -> Doc -> FnDefBranch -> FreshM Doc
pprBranch outParams doc branch = do
    -- (outParams, branch) <- freshOpen @_ @Name branchBind
    exprDoc <- pprExpr (_fnDefBranchBody branch)
    -- let B outParams branch = branchBind
    -- in
    pure $ (doc <+>
      sep
        [ hsep [hsep $ punctuate (text " ") (map ppr (_fnDefBranchInputAssertions branch))
                , hsep $ map ppr (_fnDefBranchInAllocs branch)
                , text "==>"
                , text "{" <+> hsep (punctuate (text ",") (map ppr outParams)) <+> text "}"
                , text ":="
                ]
        ])
          $$ nest 1 (exprDoc <> text ";")

and' :: Expr -> Expr -> Expr
and' x (BoolLit True) = x
and' x y = And x y

not' :: Expr -> Expr
not' (Not x) = x
not' x = Not x

--
-- Property testing --
--

instance Arbitrary FnDef where
  arbitrary = genValidFnDef
  shrink = filter isValid . genericShrink

instance Arbitrary FnDefBranch where
  arbitrary = genValidBranch [] [] -- NOTE: Only closed FnDefBranch's
  shrink = genericShrink

--   shrink fnDef = do
--     let (inVars, bnd1) = unsafeUnbind $ _fnDefBranches fnDef
--         (outVars, branches) = unsafeUnbind bnd1
--     newBranches <- transpose $ map shrinkBranch branches -- TODO: Is this right way around?
--     
--     setBranches fnDef inVars outVars <$> qcSubseqs newBranches
--
-- setBranches :: FnDef -> [Moded ExprName] -> [Moded ExprName] -> [FnDefBranch] -> FnDef
-- setBranches fnDef inVars outVars branches = fnDef { _fnDefBranches = bind inVars $ bind outVars branches }
--
-- shrinkBranch :: FnDefBranch -> [FnDefBranch]
-- shrinkBranch branch = do
--   e <- shrink $ _fnDefBranchBody branch
--   pure $ branch
--     { _fnDefBranchBody = e
--     }

-- -- | Generate only well-scoped @FnDef@s
-- instance GenValid FnDef where
--   genValid = genValidFnDef

-- | Function definitions should be well-scoped
instance Validity FnDef where
  validate (FnDef _ (B inVars (B outVars branches))) =
    let vars = inVars ++ outVars
    in
    mconcat $ map (validBranch (map modedNameName outVars) vars) branches
    -- check (isClosed @_ @Expr branches) "No free variables"

-- TODO: Make sure we are properly accounting for names "bound" by input assertions
validBranch :: [Name Expr] -> [ModedName Expr] -> FnDefBranch -> Validation
validBranch outVars modedBvs branch =
    check (all (`elem` bvs) bodyFvs) "Well-scoped"
    <> check (not (null (_fnDefBranchInputAssertions branch))) "Has at least one parameter"
    <> check (any (not . null) (_fnDefBranchInputAssertions branch)) "At least one non-empty parameter"
    <> check (null (inAsnVars `intersect` outVars)) "Input variables should not include output variables"
    <> annotate (_fnDefBranchBody branch) "Branch body expression is ok"
  where
    bodyFvs = toListOf @(Name Expr) fv (_fnDefBranchBody branch)

    inAsnVars = toListOf fv (_fnDefBranchInputAssertions branch)

    bvs = map modedNameName modedBvs `union` inAsnVars

genValidFnDef :: Gen FnDef
genValidFnDef = do
  i <- choose (1, 4)
  j <- choose (1, 2)
  inParams <- (:[]) . nub <$> replicateM i arbitraryAlpha
  outParams <- (:[]) . nub <$> replicateM j arbitraryAlpha

  when (any (`elem` outParams) inParams) discard -- Make sure inParams and outParams are disjoint

  k <- choose (1, 4)

  let modedInParams = map (Moded In . string2Name) inParams
      modedOutParams = map (Moded Out . string2Name) outParams

  let params = modedInParams ++ modedOutParams

  FnDef "testFn"
    <$>
      (bind modedInParams
        <$> (bind modedOutParams
              <$> replicateM k (genValidBranch (map string2Name outParams) params)))

genValidBranch :: [Name Expr] -> [ModedName Expr] -> Gen FnDefBranch
genValidBranch outVars = sized . genValidBranch' outVars

genValidBranch' :: [Name Expr] -> [ModedName Expr] -> Int -> Gen FnDefBranch
genValidBranch' outVars modedBvs size = do
    i <- choose (1, 3)
    inAsns <- replicateM i (genValidAssertion bvs (const $ genAsnVar (asnName : bvs)) (size `div` 2)) `suchThat` any (not . null)

    when (not (null (toListOf fv inAsns `intersect` outVars))) discard

    -- TODO: Figure this out:
    -- e <- genValidExpr' (asnName : bvs) (size `div` 2)
    e <- genValidExpr' bvs (size `div` 2)
    allocs <- genValidAllocations $ concat inAsns
    pure $ FnDefBranch inAsns allocs e
  where
    bvs = map modedNameName modedBvs
    asnName = newName bvs

    genAsnVar names = do
      name <- elements names
      pure $ V name

