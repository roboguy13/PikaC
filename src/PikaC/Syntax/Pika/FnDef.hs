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

data FnDef =
  FnDef
    { fnDefName :: String
    , fnDefTypeSig :: TypeSig
    , fnDefBranches :: [FnDefBranch]
    }
  deriving (Show)

newtype FnDefBranch =
  FnDefBranch
  { fnBranchMatch :: PatternMatches Expr Expr
  }
    -- { fnBranchPats :: [Pattern Expr]
    -- , fnBranchBody :: Bind [ExprName] Expr
    -- }
  deriving (Show)

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
    sep
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

