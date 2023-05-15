module PikaC.Syntax.Pika.FnDef
  where

import PikaC.Syntax.Type
import PikaC.Syntax.Pika.Pattern
import PikaC.Syntax.Pika.Expr

data FnDef a =
  FnDef
    { fnDefName :: String
    , fnDefType :: Type a
    , fnDefBranches :: [FnDefBranch a]
    }

data FnDefBranch a =
  FnDefBranch
    { fnBranchPat :: Pattern a
    , fnBranchBody :: Expr a
    }

