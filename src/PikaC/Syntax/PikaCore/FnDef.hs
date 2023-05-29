{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

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

import Control.Lens
import Data.List

import Test.QuickCheck
import Control.Monad

data FnDef =
  FnDef
  { _fnDefName :: String
  , _fnDefBranches ::
      Bind [ModedName Expr]    -- Input parameters
        (Bind [ModedName Expr] -- Output parameters
           [FnDefBranch])
  -- , _fnDefParams :: [ModedName Expr]
  }
  deriving (Generic)

data FnDefBranch =
  FnDefBranch
  -- { _fnDefOutputParams :: [ModedName Expr]
  { _fnDefBranchInputAssertions :: [ExprAssertion]
  , _fnDefBranchBody :: Expr
  }
  deriving (Generic)

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

instance Show FnDef where
  show = ppr'

instance Show FnDefBranch where
  show = render . runFreshM . pprBranch [] mempty

pprBranch :: [ModedName Expr] -> Doc -> FnDefBranch -> FreshM Doc
pprBranch outParams doc branch = do
    -- (outParams, branch) <- freshOpen @_ @Name branchBind
    exprDoc <- pprExpr (_fnDefBranchBody branch)
    -- let B outParams branch = branchBind
    -- in
    pure $ (doc <+>
      sep
        [ hsep [hsep $ punctuate (text " ") (map ppr (_fnDefBranchInputAssertions branch))
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

-- -- | Generate only well-scoped @FnDef@s
-- instance GenValid FnDef where
--   genValid = genValidFnDef

-- | Function definitions should be well-scoped
instance Validity FnDef where
  validate (FnDef _ (B inVars (B outVars branches))) =
    let vars = inVars ++ outVars
    in
    mconcat $ map (validBranch vars) branches
    -- check (isClosed @_ @Expr branches) "No free variables"

-- TODO: Make sure we are properly accounting for names "bound" by input assertions
validBranch :: [ModedName Expr] -> FnDefBranch -> Validation
validBranch modedBvs branch =
    check (all (`elem` bvs) bodyFvs) "Well-scoped"
  where
    bodyFvs = toListOf @(Name Expr) fv (_fnDefBranchBody branch)

    bvs = map modedNameName modedBvs `union` toListOf fv (_fnDefBranchInputAssertions branch)

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
              <$> replicateM k (genValidBranch params)))

genValidBranch :: [ModedName Expr] -> Gen FnDefBranch
genValidBranch = sized . genValidBranch'

genValidBranch' :: [ModedName Expr] -> Int -> Gen FnDefBranch
genValidBranch' modedBvs size = do
    i <- choose (0, 3)
    inAsns <- replicateM i (genValidAssertion bvs (genValidExpr' (asnName : bvs)) (size `div` 2))
    -- TODO: Figure this out:
    -- e <- genValidExpr' (asnName : bvs) (size `div` 2)
    e <- genValidExpr' (bvs) (size `div` 2)
    pure $ FnDefBranch inAsns e
  where
    bvs = map modedNameName modedBvs
    asnName = newName bvs

