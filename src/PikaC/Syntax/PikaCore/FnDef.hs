{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PikaC.Syntax.PikaCore.FnDef
  where

import PikaC.Syntax.PikaCore.Expr
import PikaC.Syntax.Pika.Layout
import PikaC.Syntax.Heaplet
import PikaC.Ppr
import PikaC.Utils

import Control.Lens.TH

import Debug.Trace

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Bind

import GHC.Generics

data FnDef =
  FnDef
  { _fnDefName :: String
  , _fnDefBranches ::
      Bind [ModedName Expr]    -- Input parameters
        (Bind [ModedName Expr] -- Output parameters
           [FnDefBranch])
  -- , _fnDefParams :: [ModedName Expr]
  }
  deriving (Show, Generic)

data FnDefBranch =
  FnDefBranch
  -- { _fnDefOutputParams :: [ModedName Expr]
  { _fnDefBranchInputAssertions :: [ExprAssertion]
  , _fnDefBranchBody :: Expr
  }
  deriving (Show, Generic)

makeLenses ''FnDef
makeLenses ''FnDefBranch

instance Alpha FnDefBranch
instance Alpha FnDef

-- fnDefInputNames :: FnDef -> [ExprName]
-- fnDefInputNames = concatMap fnDefBranchInputNames . _fnDefBranches

-- fnDefBranchInputNames :: FnDefBranch -> [ExprName]
-- fnDefBranchInputNames =
--   concatMap pointsToNames . _fnDefBranchInputAssertions

instance Subst Expr FnDefBranch

instance Ppr FnDef where
  ppr def = go (openBind (_fnDefBranches def))
    where
      go :: Bind [ModedName Expr] [FnDefBranch] -> Doc
      go (B _ []) = mempty
      go (B vs (branch : rest)) = pprBranch (ppr (_fnDefName def)) (B vs branch) $$ go (B vs rest)

-- instance Ppr FnDefBranch where
--   ppr = pprBranch mempty

pprBranch :: Doc -> Bind [ModedName Expr] FnDefBranch -> Doc
pprBranch doc branchBind =
    let B outParams branch = branchBind
    in
    (doc <+>
      sep
        [ hsep [hsep $ punctuate (text " ") (map ppr (_fnDefBranchInputAssertions branch))
                , text "==>"
                , text "{" <+> hsep (punctuate (text ",") (map ppr outParams)) <+> text "}"
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

