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

newtype FnDefBranch =
  FnDefBranch
  { fnBranchMatch :: PatternMatches Expr Expr
  }
    -- { fnBranchPats :: [Pattern Expr]
    -- , fnBranchBody :: Bind [ExprName] Expr
    -- }
  deriving (Show, Generic)

instance NFData FnDef
instance NFData FnDefBranch

getResultAllocSize :: [Layout Expr] -> FnDef -> [ExprName] -> [Allocation Expr]
getResultAllocSize layouts fnDef outParams =
  let TypeSig _ ty = fnDefTypeSig fnDef
      (_, TyVar layoutName) = splitFnType ty
      layout = lookupLayout layouts (name2String layoutName)
  in
  maxAllocsForLayout layout outParams

getResultAllocSizeInts :: [Layout Expr] -> FnDef -> [Int]
getResultAllocSizeInts layouts fnDef =
  let TypeSig _ ty = fnDefTypeSig fnDef
      (_, TyVar layoutName) = splitFnType ty
      layout = lookupLayout layouts (name2String layoutName)
  in
  map allocSize $ getResultAllocSize layouts fnDef $ map modedNameName $ getLayoutParams layout

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

