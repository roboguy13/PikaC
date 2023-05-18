{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module PikaC.Syntax.PikaCore.FnDef
  where

import PikaC.Syntax.PikaCore.Expr
import PikaC.Syntax.Heaplet
import PikaC.Ppr

import Control.Lens.TH

data FnDef =
  FnDef
  { _fnDefName :: String
  , _fnDefBranches :: [FnDefBranch]
  , _fnDefParams :: [ExprName]
  }
  deriving (Show)

data FnDefBranch =
  FnDefBranch
  { _fnDefOutputParams :: LayoutArg Expr
  , _fnDefBranchInputAssertions :: [ExprAssertion]
  , _fnDefBranchBody :: Expr
  }
  deriving (Show)

makeLenses ''FnDef
makeLenses ''FnDefBranch

fnDefInputNames :: FnDef -> [ExprName]
fnDefInputNames = concatMap fnDefBranchInputNames . _fnDefBranches

fnDefBranchInputNames :: FnDefBranch -> [ExprName]
fnDefBranchInputNames =
  concatMap pointsToNames . _fnDefBranchInputAssertions

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
  ppr def = go (_fnDefBranches def)
    where
      go :: [FnDefBranch] -> Doc
      go [] = mempty
      go (branch : rest) = (ppr (_fnDefName def) <+> ppr branch) $$ go rest

instance Ppr FnDefBranch where
  ppr branch =
    sep
      [ hsep [hsep $ punctuate (text " ") (map ppr (_fnDefBranchInputAssertions branch))
              , text "==>"
              , ppr (_fnDefOutputParams branch)
              , text ":="
              ]
      , nest 1 $ ppr (_fnDefBranchBody branch) <> text ";"
      ]

and' :: Expr -> Expr -> Expr
and' x (BoolLit True) = x
and' x y = And x y

not' :: Expr -> Expr
not' (Not x) = x
not' x = Not x

