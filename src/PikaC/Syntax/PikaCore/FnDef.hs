{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module PikaC.Syntax.PikaCore.FnDef
  where

import PikaC.Syntax.PikaCore.Expr
import PikaC.Syntax.Heaplet
import PikaC.Ppr

import Control.Lens.TH

import Debug.Trace

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

instance Ppr FnDef where
  ppr def = go (_fnDefBranches def)
    where
      go :: [FnDefBranch] -> Doc
      go [] = mempty
      go (branch : rest) = pprBranch (ppr (_fnDefName def)) branch $$ go rest

instance Ppr FnDefBranch where
  ppr = pprBranch mempty

pprBranch :: Doc -> FnDefBranch -> Doc
pprBranch doc branch =
    (doc <+>
      sep
        [ hsep [hsep $ punctuate (text " ") (map ppr (_fnDefBranchInputAssertions branch))
                , text "==>"
                , ppr (_fnDefOutputParams branch)
                , text ":="
                ]
        ])
      $$ nest 1 (ppr (_fnDefBranchBody branch) <> text ";")

and' :: Expr -> Expr -> Expr
and' x (BoolLit True) = x
and' x y = And x y

not' :: Expr -> Expr
not' (Not x) = x
not' x = Not x

