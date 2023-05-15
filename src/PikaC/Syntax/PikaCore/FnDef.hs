{-# LANGUAGE ScopedTypeVariables #-}

module PikaC.Syntax.PikaCore.FnDef
  where

import PikaC.Syntax.PikaCore.Expr
import PikaC.Syntax.Heaplet
import PikaC.Ppr

data FnDef a =
  FnDef
  { fnDefName :: String
  , fnDefBranches :: [FnDefBranch a]
  , fnDefParams :: [a]
  }

data FnDefBranch a =
  FnDefBranch
  { fnDefOutputParams :: LayoutArg a
  , fnDefBranchInputAssertions :: [ExprAssertion a]
  , fnDefBranchBody :: Expr a
  }

fnDefInputNames :: Ord a => FnDef a -> [a]
fnDefInputNames = concatMap fnDefBranchInputNames . fnDefBranches

fnDefBranchInputNames :: Ord a => FnDefBranch a -> [a]
fnDefBranchInputNames =
  concatMap pointsToNames . fnDefBranchInputAssertions

computeBranchCondition :: Ord a => FnDef a -> FnDefBranch a -> Base a
computeBranchCondition def branch = not' $ go allInputNames
  where
    allInputNames = fnDefInputNames def
    branchNames = fnDefBranchInputNames branch

    checkName name =
      if name `elem` branchNames
        then Equal (V name) (IntLit 0)
        else Not (Equal (V name) (IntLit 0))

    go [] = BoolLit True
    go [name] = checkName name
    go (name:rest) =
      and' (checkName name) (go rest)

instance forall a. Ppr a => Ppr (FnDef a) where
  ppr def = go (fnDefBranches def)
    where
      go :: [FnDefBranch a] -> Doc
      go [] = mempty
      go (branch : rest) = (ppr (fnDefName def) <+> ppr branch) $$ go rest

instance Ppr a => Ppr (FnDefBranch a) where
  ppr branch =
    sep
      [ hsep $ punctuate (text " ") (map ppr (fnDefBranchInputAssertions branch))
      , text "==>"
      , ppr (fnDefOutputParams branch)
      , text ":="
      , nest 1 $ ppr (fnDefBranchBody branch)
      , text ";"
      ]

and' :: Base a -> Base a -> Base a
and' x (BoolLit True) = x
and' x y = And x y

not' :: Base a -> Base a
not' (Not x) = x
not' x = Not x

