{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module PikaC.Syntax.PikaCore.Expr
  where

-- import PikaC.Syntax.Heaplet
import PikaC.Ppr
import Data.List
import Data.Foldable

import Unbound.Generics.LocallyNameless

import PikaC.Syntax.Heaplet

import Control.Lens
import Control.Lens.TH

import GHC.Generics

type ExprName = Name Expr

-- type LocName = ExprName

data Base
  = V ExprName
  | LocV LocName
  | LayoutV LayoutArg    -- {x ...}
  | IntLit Int -- TODO: Add output locations?
  | BoolLit Bool

  | Add Base Base
  | Sub Base Base
  | Equal Base Base
  | Not Base
  | And Base Base
  deriving (Show, Eq, Ord, Generic)

data SimpleExpr
  = BaseExpr Base
  | WithIn                -- with
      LayoutArg       --   {x ...}
      Expr            --     := e
      SimpleExpr      -- in e

  | SslAssertion            -- layout
      LayoutArg         --     {x ...}
      ExprAssertion     --   { (x+1) :-> e ** ... }
  deriving (Show, Eq, Ord, Generic)

data Expr
  = SimpleExpr SimpleExpr
  -- | App (Expr a) (LayoutArg a)
  | App String [LayoutArg] -- | Fully saturated function application
  deriving (Show, Eq, Ord, Generic)

type PointsToExpr = PointsTo Base
type ExprAssertion = [PointsToExpr]

makePrisms ''Base
makePrisms ''SimpleExpr
makePrisms ''Expr

instance Alpha Base
instance Alpha SimpleExpr
instance Alpha Expr


getPointsToExpr :: Expr -> [PointsToExpr]
getPointsToExpr e = e ^.. (_SimpleExpr . _SslAssertion . _2 . traversed)

-- instance HasPointsTo SimpleExpr Base where
--   getPointsTo (BaseExpr {}) = []
--   getPointsTo (WithIn _ e e') =
--     getPointsTo e ++ getPointsTo e'
--   getPointsTo (SslAssertion arg xs) = xs

-- instance HasPointsTo Expr Base where
--   getPointsTo (SimpleExpr e) = getPointsTo e
--   getPointsTo (App _ _) = []

-- instance Ppr a => Ppr (LayoutArg a) where
--   ppr (LayoutArg xs) = text "{" <+> hsep (punctuate (text ",") (map ppr xs)) <+> text "}"

instance Ppr Base where
  ppr (V x) = ppr x
  ppr (LocV x) = ppr x
  ppr (LayoutV x) = ppr x
  ppr (IntLit i) = ppr i
  ppr (BoolLit b) = ppr b
  ppr (Add x y) = sep [pprP x, text "+", pprP y]
  ppr (Sub x y) = sep [pprP x, text "-", pprP y]
  ppr (Equal x y) = sep [pprP x, text "==", pprP y]
  ppr (Not x) = sep [text "!", pprP x]
  ppr (And x y) = sep [pprP x, text "&&", pprP y]

instance Ppr SimpleExpr where
  ppr (BaseExpr e) = ppr e
  ppr (WithIn vars bnd body) =
    sep [text "with", hsep [ppr vars, text ":=", ppr bnd], text "in", ppr body]

  ppr (SslAssertion vars heaplets) =
    sep [text "layout", ppr vars, ppr heaplets]

instance Ppr Expr where
  ppr (SimpleExpr e) = ppr e
  ppr (App f x) = ppr f <+> hsep (map ppr x)

instance IsNested Expr where
  isNested (SimpleExpr e) = isNested e
  isNested (App {}) = True

instance IsNested SimpleExpr where
  isNested (BaseExpr e) = isNested e
  isNested (WithIn {}) = True
  isNested (SslAssertion {}) = True

instance IsNested Base where
  isNested (V _) = False
  isNested (LocV _) = False
  isNested (LayoutV _) = False
  isNested (IntLit _) = False
  isNested (BoolLit _) = False

  isNested (Add {}) = True
  isNested (Sub {}) = True
  isNested (Equal {}) = True
  isNested (Not {}) = True
  isNested (And {}) = True

