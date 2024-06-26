{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}

module PikaC.Syntax.Pika.FnDef
  where

import PikaC.Syntax.Type
import PikaC.Syntax.Pika.Pattern
import PikaC.Syntax.Pika.Expr
import PikaC.Syntax.Pika.Layout
import PikaC.Syntax.Heaplet

import PikaC.Ppr
import PikaC.Utils

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Bind
import Unbound.Generics.LocallyNameless.Unsafe

import Test.QuickCheck

import Data.List

import Data.Bifunctor

import GHC.Generics

import Control.Monad
import Debug.Trace

import Data.Validity

import Control.DeepSeq

data FnDef' f =
  FnDef
    { fnDefName :: String
    , fnDefTypedBranches :: f [FnDefBranch]
    }
  deriving (Generic)

instance Size (f [FnDefBranch]) => Size (FnDef' f) where
  size (FnDef x y) = visibleNode $ size x + size y

overTypedBranches :: (f [FnDefBranch] -> g [FnDefBranch]) -> FnDef' f -> FnDef' g
overTypedBranches f (FnDef x y) = FnDef x (f y)

overTypedBranchesM :: Functor m => (f [FnDefBranch] -> m (g [FnDefBranch])) -> FnDef' f -> m (FnDef' g)
overTypedBranchesM f (FnDef x y) = FnDef x <$> (f y)

deriving instance Show (f [FnDefBranch]) => Show (FnDef' f)

type FnDef = FnDef' Typed

data GuardedExpr =
  GuardedExpr
    Expr -- Boolean condition
    Expr -- Body
  deriving (Show, Generic)

overGuardedExpr :: (Expr -> Expr) -> GuardedExpr -> GuardedExpr
overGuardedExpr f (GuardedExpr a b) = GuardedExpr (f a) (f b)

instance Size GuardedExpr where
  size (GuardedExpr x y) = visibleNode $ size x + size y

newtype FnDefBranch =
  FnDefBranch
  { fnBranchMatch :: PatternMatches Expr GuardedExpr
  }
    -- { fnBranchPats :: [Pattern Expr]
    -- , fnBranchBody :: Bind [ExprName] Expr
    -- }
  deriving (Show, Generic)

onFnDef :: Fresh m => (Expr -> m Expr) -> FnDef -> m FnDef
onFnDef f (FnDef name (Typed t bs)) =
  FnDef name . Typed t <$> traverse (onFnDefBranch f) bs

onFnDefBranch :: Fresh m => (Expr -> m Expr) -> FnDefBranch -> m FnDefBranch
onFnDefBranch f (FnDefBranch (PatternMatches bnd)) = do
  (pats, GuardedExpr e1 e2) <- unbind bnd
  e1' <- f e1
  e2' <- f e2
  pure $ FnDefBranch $ PatternMatches $ bind pats $ GuardedExpr e1' e2'

instance Size FnDefBranch where
  size (FnDefBranch x) = size x

getFnTypeSig :: FnDef -> (String, Type)
getFnTypeSig fnDef = (fnDefName fnDef, typePairType $ fnDefTypedBranches fnDef)

getFnDefExprs :: FnDef -> FreshM [Expr]
getFnDefExprs (FnDef _ (Typed _ xs0)) = concat <$> traverse goBranches xs0
  where
    goBranches :: FnDefBranch -> FreshM [Expr]
    goBranches (FnDefBranch (PatternMatches bnd)) = do
      (_, GuardedExpr e1 e2) <- unbind bnd
      pure [e1, e2]

-- | 'synth' directive to use SuSLik to synthesize a function
data Synth =
  Synth
    String   -- | Function name
    Expr     -- | "Pure part"
    [Type]   -- | Argument types
    Type     -- | Result type
  deriving (Show, Generic)

instance Size Synth where
  size (Synth a b c d) = visibleNode $ size a + size b + size c + size d

getSynthTypeSig :: Synth -> (String, Type)
getSynthTypeSig (Synth f _ argTypes resultType) =
  (f, foldr FnType resultType argTypes)

unguardMatches :: PatternMatches Expr GuardedExpr -> PatternMatches Expr Expr
unguardMatches (PatternMatches matches) =
  let (vars, GuardedExpr _ body) = unsafeUnbind matches
  in
  PatternMatches $ bind vars body

instance NFData (f [FnDefBranch]) => NFData (FnDef' f)
instance NFData GuardedExpr
instance NFData FnDefBranch
instance NFData Synth

instance Alpha GuardedExpr
instance Alpha FnDefBranch

instance Subst Expr (f [FnDefBranch]) => Subst Expr (FnDef' f)
instance Subst Expr FnDefBranch
instance Subst Expr a => Subst Expr (Typed a)

instance Alpha (f [FnDefBranch]) => Alpha (FnDef' f)
instance Alpha a => Alpha (Typed a)

instance Ppr Synth where
  ppr (Synth fnName purePart argTypes resultType) =
    text "synth" <+> text fnName <+> text ":" <+> ppr purePart <+> text ";;" <+> ppr (foldr FnType resultType argTypes)

-- | Take the patterns of the first branch. This is to be used when
-- determining names for arguments of base type. Only the PatternVar's
-- should be used. The other patterns are used so that the PatternVar's
-- land in the right places.
getFirstPatterns :: FnDef -> [Pattern Expr]
getFirstPatterns fnDef =
  let (FnDefBranch (PatternMatches matches):_) = typePairData $ fnDefTypedBranches fnDef
      (pats, _) = unsafeUnbind matches
  in
  pats

getResultAllocSize :: [Layout Expr] -> Type -> [ExprName] -> [Allocation Expr]
getResultAllocSize layouts ty outParams =
  let (_, resultTy) = splitFnType ty
      handleLayoutId layoutName = 
        let
            layout = lookupLayout layouts layoutName
        in
        maxAllocsForLayout layout outParams
  in
  case resultTy of
    _ | isBaseType resultTy -> []
    LayoutId layoutName -> handleLayoutId layoutName
    GhostApp (LayoutId layoutName) _ -> handleLayoutId layoutName

getResultAllocSizeInts :: [Layout Expr] -> Type -> [Int]
getResultAllocSizeInts layouts ty =
  let (_, t) = splitFnType ty
      layoutName = getLayoutId t
      layout = lookupLayout layouts layoutName
      allocs = getResultAllocSize layouts ty $ map modedNameName $ getLayoutParams layout
  in
  map allocSize allocs

instance (TypePair f, Ppr (TypePairType f)) => Ppr (FnDef' f) where
  ppr fn =
    vcat
      (hsep [text (fnDefName fn), text ":", ppr (typePairType (fnDefTypedBranches fn))] <> text ";"
        :
        map (\branch -> text (fnDefName fn) <+> ppr branch) (typePairData (fnDefTypedBranches fn))
      )

instance Ppr FnDefBranch where
  ppr (FnDefBranch matches@(PatternMatches bnd)) =
    sep
      [ hsep (map ppr (patternMatchesPats matches)) -- ++ [text ":="])
      , nest 1 $ ppr (openBind bnd) <> text ";"
      ]

--
-- Property tests
--

instance Validity (f [FnDefBranch]) => Validity (FnDef' f) where
  validate (FnDef _ branches) = validate branches

instance Validity FnDefBranch where
  validate (FnDefBranch (PatternMatches bnd)) =
    let (pats, body) = unsafeUnbind bnd
    in
    wellScoped pats body

instance Arbitrary FnDefBranch where
  arbitrary = error "Arbitrary FnDefBranch"
  shrink = genericShrink

instance TypePair f => Arbitrary (FnDef' f) where
  arbitrary = error "Arbitrary FnDef"
  shrink fn = do
    let b = map shrink (typePairData (fnDefTypedBranches fn))
        typed = mkTypePair (typePairType (fnDefTypedBranches fn))
    branches' <- sequenceA b
    pure $ fn { fnDefTypedBranches = typed branches' }

instance Arbitrary GuardedExpr where
  arbitrary = error "Arbitrary GuardedExpr"
  shrink = genericShrink

instance WellScoped (Pattern Expr) GuardedExpr

instance Subst Expr GuardedExpr
instance Subst (Pattern Expr) GuardedExpr

instance Ppr GuardedExpr where
  ppr (GuardedExpr (BoolLit True) body) = text ":=" <+> ppr body
  ppr (GuardedExpr cond body) =
    text "|" <+> ppr cond <+> text ":=" <+> ppr body

genFnSig ::
   [(LayoutName, [(String, [Maybe LayoutName])])] ->
   Gen (String, [Maybe LayoutName], LayoutName)
genFnSig layoutSigs = do
  fnName <- genFnName
  argCount <- choose (1, 3)
  args <- map Just <$> replicateM argCount (elements' (map fst layoutSigs))
  result <- elements' (map fst layoutSigs)
  pure (fnName, args, result)

genFnName :: Gen String
genFnName = do
  x <- elements "fgh"
  n <- choose (0, 8) :: Gen Int
  pure (x : show n)

genFnDef :: 
   [(String, [Maybe LayoutName], LayoutName)] -> -- Function signatures
   [(LayoutName, [(String, [Maybe LayoutName])])] -> -- Layouts with their constructors and those constructors' arities
   Int ->
   (String, [Maybe LayoutName], LayoutName) ->
   Gen FnDef
genFnDef fnSigs layouts size (fnName, inLayouts, outLayout) = do
  -- branches <- mapM (genFnDefBranch fnSigs layouts outLayout) inLayouts
  let constructorLists = map lookupConstructorList inLayouts
      params = sequenceA constructorLists
      ty = foldr1 FnType $ map goType (inLayouts ++ [Just outLayout])
  branches <- mapM (genFnDefBranch fnSigs layouts outLayout size) params
  pure $
      FnDef
      { fnDefName = fnName
          -- TypeSig
          -- {_typeSigLayoutConstraints = []
          -- ,_typeSigTy = ty 
          -- }
      , fnDefTypedBranches = Typed ty branches
      }
  where
    lookupConstructorList Nothing = error "lookupConstructorList: Nothing" -- TODO: Implement this case
    lookupConstructorList (Just layoutName) = 
      let Just r = lookup layoutName layouts
      in r

    goType (Just layoutName) = LayoutId layoutName
    goType Nothing = IntType -- For now, we just assume every base type is an Int

genFnDefBranch ::
   [(String, [Maybe LayoutName], LayoutName)] -> -- Function signatures
   [(LayoutName, [(String, [Maybe LayoutName])])] -> -- Layouts with their constructors and those constructors' arities
   LayoutName -> -- Output layout
   Int ->
   [(String, [Maybe LayoutName])] -> -- Input layouts
   Gen FnDefBranch
genFnDefBranch fnSigs layouts outLayout size inLayouts = do
  let locals = mkFreshVars $ map snd inLayouts
  body <- genForLayout fnSigs layouts (concat locals) size outLayout
  pure $ FnDefBranch
    { fnBranchMatch =
        PatternMatches
          $ bind (toPatterns $ zip (map fst inLayouts) $ map (map fst) locals)
            $ GuardedExpr (BoolLit True) -- TODO: Generate this condition expression
              $ body
    }

toPatterns :: [(String, [ExprName])] -> [Pattern Expr]
toPatterns = map (uncurry Pattern)

mkFreshVars :: [[Maybe LayoutName]] -> [[(ExprName, Maybe LayoutName)]]
mkFreshVars = go 0
  where
    go :: Int -> [[Maybe LayoutName]] -> [[(ExprName, Maybe LayoutName)]]
    go n [] = []
    go n (xs:xss) =
      let (n', xs') = go' n xs
      in
      xs' : go n' xss

    go' n [] = (n, [])
    go' n (x:xs) =
      let (n', r) = go' (n+1) xs
      in
      (n', (string2Name ('v':show n), x) : r)

