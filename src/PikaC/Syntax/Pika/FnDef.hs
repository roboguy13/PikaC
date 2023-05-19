module PikaC.Syntax.Pika.FnDef
  where

import PikaC.Syntax.Type
import PikaC.Syntax.Pika.Pattern
import PikaC.Syntax.Pika.Expr

import PikaC.Ppr

data FnDef =
  FnDef
    { fnDefName :: String
    , fnDefTypeSig :: TypeSig
    , fnDefBranches :: [FnDefBranch]
    }
  deriving (Show)

data FnDefBranch =
  FnDefBranch
    { fnBranchPats :: [Pattern Expr]
    , fnBranchBody :: Expr
    }
  deriving (Show)

instance Ppr FnDef where
  ppr fn =
    sep
      (hsep [text (fnDefName fn), text ":", ppr (fnDefTypeSig fn)] <> text ";"
        :
        map (\branch -> text (fnDefName fn) <+> ppr branch) (fnDefBranches fn)
      )

instance Ppr FnDefBranch where
  ppr branch =
    sep
      [ hsep (map ppr (fnBranchPats branch) ++ [text ":="])
      , nest 1 $ ppr (fnBranchBody branch) <> text ";"
      ]

