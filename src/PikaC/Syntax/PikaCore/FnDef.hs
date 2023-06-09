{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module PikaC.Syntax.PikaCore.FnDef
  where

import PikaC.Syntax.PikaCore.Expr
import PikaC.Syntax.Pika.Layout
import PikaC.Syntax.Heaplet
import PikaC.Syntax.Type
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
import Data.Maybe

import Unbound.Generics.LocallyNameless.Unsafe -- Just for implementing QuickCheck shrinking

import Test.QuickCheck
import Control.Monad

data ArgLayout =
  ArgLayout
  { _argLayoutName :: String
  , _argLayoutParams :: [ExprName]
  }
  deriving (Show, Generic)

data FnDef =
  FnDef
  { _fnDefName :: FnName
  , _fnDefOutputSizes :: [Int] -- Allocation sizes for output parameters
  , _fnDefType :: Type
  , _fnDefBranches ::
      Bind [ModedName Expr]    -- Input parameters
        (Bind [ModedName Expr] -- Output parameters
           ([Maybe ArgLayout]  -- 'Nothing' means a base type
           ,[FnDefBranch]))
  -- , _fnDefParams :: [ModedName Expr]
  }
  deriving (Show, Generic)

data InputAssertion = ExprAssertion ExprAssertion | LayoutApp String [Expr]
  deriving (Show, Generic)

getLayoutApps :: [InputAssertion] -> [(String, [Expr])]
getLayoutApps = mapMaybe $ \case
  LayoutApp f xs -> Just (f, xs)
  _ -> Nothing

data Input = InputAsn InputAssertion | InputVar ExprName
  deriving (Show, Generic)

getInputAsns' :: [Input] -> [InputAssertion]
getInputAsns' = mapMaybe $ \case
  InputAsn asn -> Just asn
  _ -> Nothing

getInputAsns :: [Input] -> [ExprAssertion]
getInputAsns [] = []
getInputAsns (InputAsn (ExprAssertion x) : xs) = x : getInputAsns xs
getInputAsns (InputAsn (LayoutApp {}) : xs) = getInputAsns xs
getInputAsns (InputVar {} : xs) = getInputAsns xs

data FnDefBranch =
  FnDefBranch
  { _fnDefBranchInputAssertions :: [Input]
  , _fnDefBranchInAllocs :: [Allocation Expr]
  , _fnDefBranchCondition :: Expr -- Guard condition
  , _fnDefBranchBody :: Expr
  }
  deriving (Show, Generic)

makeLenses ''FnDef
makeLenses ''FnDefBranch

instance Alpha FnDefBranch
instance Alpha FnDef
instance Alpha ArgLayout

-- fnDefInputNames :: FnDef -> [ExprName]
-- fnDefInputNames = concatMap fnDefBranchInputNames . _fnDefBranches

-- fnDefBranchInputNames :: FnDefBranch -> [ExprName]
-- fnDefBranchInputNames =
--   concatMap pointsToNames . _fnDefBranchInputAssertions

instance Subst Expr FnDefBranch

instance Subst Expr ArgLayout

instance WellScoped (Name Expr) Type
instance WellScoped (Name Expr) (Bind (TypeName, Embed AdtName) Type)
instance WellScoped (Name Expr) (Embed AdtName)
instance WellScoped (Name Expr) (Name Type)

inputNames :: Input -> [ExprName]
inputNames (InputAsn (ExprAssertion asn)) = map (getV . locBase . pointsToLhs) asn
inputNames (InputAsn (LayoutApp _ args)) = map getV args
inputNames (InputVar v) = [v]

inputBaseNames :: [Input] -> [ExprName]
inputBaseNames [] = []
inputBaseNames (InputAsn {} : rest) = inputBaseNames rest
inputBaseNames (InputVar v : rest) = v : inputBaseNames rest

instance Ppr Input where
  ppr (InputAsn asn) = ppr asn
  ppr (InputVar v) = ppr v

instance Alpha Input

instance Alpha InputAssertion

instance Subst a Expr => Subst a InputAssertion

instance Subst a Expr => Subst a Input

instance Ppr InputAssertion where
  ppr (ExprAssertion asn) = ppr asn
  ppr (LayoutApp f xs) = text "[[" <> text f <> text "(" <> hsep (punctuate (text ",") (map ppr xs)) <> text ")" <> text "]]"

instance Ppr FnDef where
  ppr def = runFreshM $ do
      let bnd1@(B vs _) = openBind (_fnDefBranches def)
      vs' <- mapM fresh (concatMap getNames vs)
      let modedVs' = zipWith Moded (map getMode vs) vs'
      let (_, branches) = instantiate bnd1 (map V vs' :: [Expr])
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
                $$ case _fnDefBranchCondition branch of
                    BoolLit True -> mempty
                    cond -> text "|" <+> ppr cond
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

instance WellScoped ExprName ArgLayout
instance WellScoped ExprName FnDef

instance WellScoped ExprName FnDefBranch where
  wellScoped inScopes branch =
    let inScopes' = inScopes ++ concatMap inputNames (_fnDefBranchInputAssertions branch)
    in
    wellScoped inScopes' (_fnDefBranchBody branch)

-- instance Arbitrary FnDef where
--   arbitrary = genValidFnDef
--   shrink = filter isValid . genericShrink

instance Arbitrary FnDefBranch where
  arbitrary = error "Arbitrary FnDefBranch"
  -- arbitrary = genValidBranch [] [] -- NOTE: Only closed FnDefBranch's
  shrink = genericShrink

instance Arbitrary InputAssertion where
  arbitrary = error "Arbitrary InputAssertion"
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
  validate fnDef = wellScoped ([] :: [ExprName]) fnDef <> validateFnDefWith validate fnDef
    -- check (isClosed @_ @Expr branches) "No free variables"

-- TODO: Make sure we are properly accounting for names "bound" by input assertions
validBranch :: (Expr -> Validation) -> [Name Expr] -> [ModedName Expr] -> FnDefBranch -> Validation
validBranch v outVars modedBvs branch =
    decorate (render (runFreshM (pprBranch (map (Moded Out) outVars) mempty branch))) $
    check (all (`elem` bvs) bodyFvs) "Well-scoped"
    <> check (not (null (_fnDefBranchInputAssertions branch))) "Has at least one parameter"
    <> check (null (inAsnVars `intersect` outVars)) "Input variables should not include output variables"
    <> decorate "Branch body expression is ok" (v (_fnDefBranchBody branch))
  where
    bodyFvs = toListOf @(Name Expr) fv (_fnDefBranchBody branch)

    inAsnVars = toListOf fv (_fnDefBranchInputAssertions branch)

    bvs = map modedNameName modedBvs `union` inAsnVars

validateFnDefWith :: (Expr -> Validation) -> FnDef -> Validation
validateFnDefWith v (FnDef _ _ _ (B inVars (B outVars branches))) =
  let vars = inVars ++ outVars
  in
  mconcat $ map (validBranch v (map modedNameName outVars) vars) $ snd branches

genValidFnDef :: Gen FnDef
genValidFnDef = do
  i <- choose (1, 4)
  j <- choose (1, 2)
  inParams <- (:[]) . nub <$> replicateM i arbitraryAlpha
  outParams <- (:[]) . nub <$> replicateM j arbitraryAlpha

  k <-
    if any (`elem` outParams) inParams -- Make sure inParams and outParams are disjoint
      then discard
      else choose (1, 4)

  let modedInParams = map (Moded In . string2Name) inParams
      modedOutParams = map (Moded Out . string2Name) outParams

  let params = modedInParams ++ modedOutParams

  FnDef "testFn"
    <$> replicateM (length modedOutParams) (choose (1, 4)) -- TODO: Does this make sense?
    <*> pure (FnType (LayoutId "Placeholder") (LayoutId "Placeholder2")) -- TODO: Generate type)
    <*>
      (bind modedInParams
        <$> (bind modedOutParams
              <$> (([],) <$> replicateM k (genValidBranch (map string2Name outParams) params)))) -- TODO: Generate ArgLayout values

genValidBranch :: [Name Expr] -> [ModedName Expr] -> Gen FnDefBranch
genValidBranch outVars = sized . genValidBranch' outVars

instance Arbitrary Input where
  arbitrary =
    oneof
      [ InputAsn <$> arbitrary
      , InputVar <$> arbitrary
      ]

genValidBranch' :: [Name Expr] -> [ModedName Expr] -> Int -> Gen FnDefBranch
genValidBranch' outVars modedBvs size = do
    i <- choose (1, 3)
    let asnBvs = bvs \\ outVars
        inAsnNames = (asnName : bvs) \\ outVars
    inAsns <- replicateM i (genValidAssertion asnBvs (const $ genAsnVar inAsnNames) (size `div` 2)) `suchThat` any (not . null)

    -- TODO: Figure this out:
    -- e <- genValidExpr' (asnName : bvs) (size `div` 2)
    e <- genValidExpr' bvs (size `div` 2)
      -- if not (null (toListOf fv inAsns `intersect` outVars))
      --   then discard
      --   else genValidExpr' bvs (size `div` 2)
    allocs <- genValidAllocations $ concat inAsns
    -- TODO: Generate the Boolean guard condition
    pure $ FnDefBranch (map (InputAsn . ExprAssertion) inAsns) allocs (BoolLit True) e
  where
    bvs = map modedNameName modedBvs
    asnName = newName bvs

    genAsnVar names = do
      name <- elements names
      pure $ V name

