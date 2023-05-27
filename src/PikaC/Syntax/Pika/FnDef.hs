module PikaC.Syntax.Pika.FnDef
  where

import PikaC.Syntax.Type
import PikaC.Syntax.Pika.Pattern
import PikaC.Syntax.Pika.Expr

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
  { fnBranchMatch :: PatternMatches Expr
  }
    -- { fnBranchPats :: [Pattern Expr]
    -- , fnBranchBody :: Bind [ExprName] Expr
    -- }
  deriving (Show)

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

