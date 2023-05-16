module PikaC.Syntax.Pika.FnDef
  where

import PikaC.Syntax.Type
import PikaC.Syntax.Pika.Pattern
import PikaC.Syntax.Pika.Expr

data FnDef =
  FnDef
    { fnDefName :: String
    , fnDefType :: Type
    , fnDefBranches :: [FnDefBranch]
    }

data FnDefBranch =
  FnDefBranch
    { fnBranchPat :: Pattern
    , fnBranchBody :: Expr
    }

