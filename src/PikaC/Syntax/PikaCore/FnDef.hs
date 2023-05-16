{-# LANGUAGE ScopedTypeVariables #-}

module PikaC.Syntax.PikaCore.FnDef
  where

import PikaC.Syntax.PikaCore.Expr
import PikaC.Syntax.Heaplet
import PikaC.Ppr

data FnDef =
  FnDef
  { fnDefName :: String
  , fnDefBranches :: [FnDefBranch]
  , fnDefParams :: [LocName]
  }

data FnDefBranch =
  FnDefBranch
  { fnDefOutputParams :: LayoutArg
  , fnDefBranchInputAssertions :: [ExprAssertion]
  , fnDefBranchBody :: Expr
  }

fnDefInputNames :: FnDef -> [LocName]
fnDefInputNames = concatMap fnDefBranchInputNames . fnDefBranches

fnDefBranchInputNames :: FnDefBranch -> [LocName]
fnDefBranchInputNames =
  concatMap pointsToNames . fnDefBranchInputAssertions

computeBranchCondition :: FnDef -> FnDefBranch -> Base
computeBranchCondition def branch = not' $ go allInputNames
  where
    allInputNames = fnDefInputNames def
    branchNames = fnDefBranchInputNames branch

    checkName name =
      if name `elem` branchNames
        then Equal (LocV name) (IntLit 0)
        else Not (Equal (LocV name) (IntLit 0))

    go [] = BoolLit True
    go [name] = checkName name
    go (name:rest) =
      and' (checkName name) (go rest)

instance Ppr FnDef where
  ppr def = go (fnDefBranches def)
    where
      go :: [FnDefBranch] -> Doc
      go [] = mempty
      go (branch : rest) = (ppr (fnDefName def) <+> ppr branch) $$ go rest

instance Ppr FnDefBranch where
  ppr branch =
    sep
      [ hsep $ punctuate (text " ") (map ppr (fnDefBranchInputAssertions branch))
      , text "==>"
      , ppr (fnDefOutputParams branch)
      , text ":="
      , nest 1 $ ppr (fnDefBranchBody branch)
      , text ";"
      ]

and' :: Base -> Base -> Base
and' x (BoolLit True) = x
and' x y = And x y

not' :: Base -> Base
not' (Not x) = x
not' x = Not x

