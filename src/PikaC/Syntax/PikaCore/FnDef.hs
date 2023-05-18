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
  , fnDefParams :: [ExprName]
  }
  deriving (Show)

data FnDefBranch =
  FnDefBranch
  { fnDefOutputParams :: LayoutArg Expr
  , fnDefBranchInputAssertions :: [ExprAssertion]
  , fnDefBranchBody :: Expr
  }
  deriving (Show)

fnDefInputNames :: FnDef -> [ExprName]
fnDefInputNames = concatMap fnDefBranchInputNames . fnDefBranches

fnDefBranchInputNames :: FnDefBranch -> [ExprName]
fnDefBranchInputNames =
  concatMap pointsToNames . fnDefBranchInputAssertions

computeBranchCondition :: FnDef -> FnDefBranch -> Expr
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

instance Ppr FnDef where
  ppr def = go (fnDefBranches def)
    where
      go :: [FnDefBranch] -> Doc
      go [] = mempty
      go (branch : rest) = (ppr (fnDefName def) <+> ppr branch) $$ go rest

instance Ppr FnDefBranch where
  ppr branch =
    sep
      [ hsep [hsep $ punctuate (text " ") (map ppr (fnDefBranchInputAssertions branch))
              , text "==>"
              , ppr (fnDefOutputParams branch)
              , text ":="
              ]
      , nest 1 $ ppr (fnDefBranchBody branch) <> text ";"
      ]

and' :: Expr -> Expr -> Expr
and' x (BoolLit True) = x
and' x y = And x y

not' :: Expr -> Expr
not' (Not x) = x
not' x = Not x

