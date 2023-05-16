module PikaC.Syntax.Pika.FnDef
  where

import PikaC.Syntax.Type
import PikaC.Syntax.Pika.Pattern
import PikaC.Syntax.Pika.Expr

data FnDef t a =
  FnDef
    { fnDefName :: String
    , fnDefType :: Type t
    , fnDefBranches :: [FnDefBranch a]
    }

newtype FnDefBranch a =
  FnDefBranch { fnBranchPat :: PatternMatch Expr a }
  -- FnDefBranch
  --   { fnBranchPat :: Pattern a
  --   , fnBranchBody :: Expr a
  --   }

