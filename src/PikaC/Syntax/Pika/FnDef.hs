{-# LANGUAGE DeriveGeneric #-}

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

data FnDef =
  FnDef
    { fnDefName :: String
    , fnDefTypeSig :: TypeSig
    , fnDefBranches :: [FnDefBranch]
    }
  deriving (Show, Generic)

data GuardedExpr =
  GuardedExpr
    Expr -- Boolean condition
    Expr -- Body
  deriving (Show, Generic)

newtype FnDefBranch =
  FnDefBranch
  { fnBranchMatch :: PatternMatches Expr GuardedExpr
  }
    -- { fnBranchPats :: [Pattern Expr]
    -- , fnBranchBody :: Bind [ExprName] Expr
    -- }
  deriving (Show, Generic)

getFnTypeSig :: FnDef -> (String, TypeSig)
getFnTypeSig fnDef = (fnDefName fnDef, fnDefTypeSig fnDef)

-- | 'synth' directive to use SuSLik to synthesize a function
data Synth =
  Synth
    String   -- | Function name
    Expr     -- | "Pure part"
    [Type]   -- | Argument types
    Type     -- | Result type
  deriving (Show, Generic)

getSynthTypeSig :: Synth -> (String, TypeSig)
getSynthTypeSig (Synth f _ argTypes resultType) =
  (f, TypeSig [] (foldr FnType resultType argTypes))

unguardMatches :: PatternMatches Expr GuardedExpr -> PatternMatches Expr Expr
unguardMatches (PatternMatches matches) =
  let (vars, GuardedExpr _ body) = unsafeUnbind matches
  in
  PatternMatches $ bind vars body

instance NFData FnDef
instance NFData GuardedExpr
instance NFData FnDefBranch
instance NFData Synth

instance Alpha GuardedExpr

instance Ppr Synth where
  ppr (Synth fnName purePart argTypes resultType) =
    text "synth" <+> text fnName <+> text ":" <+> ppr purePart <+> text ";;" <+> ppr (foldr FnType resultType argTypes)

-- | Take the patterns of the first branch. This is to be used when
-- determining names for arguments of base type. Only the PatternVar's
-- should be used. The other patterns are used so that the PatternVar's
-- land in the right places.
getFirstPatterns :: FnDef -> [Pattern Expr]
getFirstPatterns fnDef =
  let (FnDefBranch (PatternMatches matches):_) = fnDefBranches fnDef
      (pats, _) = unsafeUnbind matches
  in
  pats

getResultAllocSize :: [Layout Expr] -> TypeSig -> [ExprName] -> [Allocation Expr]
getResultAllocSize layouts (TypeSig _ ty) outParams =
  let (_, resultTy) = splitFnType ty
      handleTyVar layoutName = 
        let
            layout = lookupLayout layouts (name2String layoutName)
        in
        maxAllocsForLayout layout outParams
  in
  case resultTy of
    _ | isBaseType resultTy -> []
    TyVar layoutName -> handleTyVar layoutName
    GhostApp (TyVar layoutName) _ -> handleTyVar layoutName

getResultAllocSizeInts :: [Layout Expr] -> TypeSig -> [Int]
getResultAllocSizeInts layouts typeSig@(TypeSig _ ty) =
  let (_, t) = splitFnType ty
      layoutName = getTyVar t
      layout = lookupLayout layouts (name2String layoutName)
      allocs = getResultAllocSize layouts typeSig $ map modedNameName $ getLayoutParams layout
  in
  map allocSize allocs

instance Ppr FnDef where
  ppr fn =
    vcat
      (hsep [text (fnDefName fn), text ":", ppr (fnDefTypeSig fn)] <> text ";"
        :
        map (\branch -> text (fnDefName fn) <+> ppr branch) (fnDefBranches fn)
      )

instance Ppr FnDefBranch where
  ppr (FnDefBranch matches@(PatternMatches bnd)) =
    sep
      [ hsep (map ppr (patternMatchesPats matches) ++ [text ":="])
      , nest 1 $ ppr (openBind bnd) <> text ";"
      ]

--
-- Property tests
--

instance Validity FnDef where
  validate (FnDef _ _ branches) = mconcat (map validate branches)

instance Validity FnDefBranch where
  validate (FnDefBranch (PatternMatches bnd)) =
    let (pats, body) = unsafeUnbind bnd
    in
    wellScoped pats body

instance Arbitrary FnDefBranch where
  arbitrary = error "Arbitrary FnDefBranch"
  shrink = genericShrink

instance Arbitrary FnDef where
  arbitrary = error "Arbitrary FnDef"
  shrink fn = do
    let b = map shrink (fnDefBranches fn)
    branches' <- sequenceA b
    pure $ fn { fnDefBranches = branches' }

instance Arbitrary GuardedExpr where
  arbitrary = error "Arbitrary GuardedExpr"
  shrink = genericShrink

instance WellScoped (Pattern Expr) GuardedExpr

instance Subst Expr GuardedExpr

instance Ppr GuardedExpr where
  ppr (GuardedExpr (BoolLit True) body) = ppr body
  ppr (GuardedExpr cond body) =
    text "|" <+> ppr cond <+> text "=>" <+> ppr body

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
      , fnDefTypeSig =
          TypeSig
          {_typeSigLayoutConstraints = []
          ,_typeSigTy = ty 
          }
      , fnDefBranches = branches
      }
  where
    lookupConstructorList Nothing = error "lookupConstructorList: Nothing" -- TODO: Implement this case
    lookupConstructorList (Just layoutName) = 
      let Just r = lookup layoutName layouts
      in r

    goType (Just layoutName) = TyVar layoutName
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

