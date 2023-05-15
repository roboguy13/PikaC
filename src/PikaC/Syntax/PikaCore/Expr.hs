module PikaC.Syntax.PikaCore.Expr
  where

import PikaC.Syntax.Heaplet
import PikaC.Ppr
import Data.List

data Base a
  = V a
  | LayoutV (LayoutArg a)    -- {x ...}
  | IntLit Int
  | BoolLit Bool

  | Add (Base a) (Base a)
  | Sub (Base a) (Base a)
  | Equal (Base a) (Base a)
  | Not (Base a)
  | And (Base a) (Base a)
  deriving (Show, Functor)

newtype LayoutArg a = LayoutArg [Base a]
  deriving (Show, Functor)

data SimpleExpr a
  = BaseExpr (Base a)
  | WithIn                -- with
      (LayoutArg a)       --   {x ...}
      (Expr a)            --     := e
      (SimpleExpr a)      -- in e

  | SslAssertion            -- layout
      (LayoutArg a)         --     {x ...}
      (ExprAssertion a)     --   { (x+1) :-> e ** ... }
  deriving (Show)

data Expr a
  = SimpleExpr (SimpleExpr a)
  | App (Expr a) (LayoutArg a)
  deriving (Show)

type PointsToExpr a = PointsTo a (Expr a)
type ExprAssertion a = [PointsToExpr a]

instance Ppr a => Ppr (LayoutArg a) where
  ppr (LayoutArg xs) = text "{" <+> hsep (punctuate (text ",") (map ppr xs)) <+> text "}"

instance Ppr a => Ppr (Base a) where
  ppr (V x) = ppr x
  ppr (LayoutV x) = ppr x
  ppr (IntLit i) = ppr i
  ppr (BoolLit b) = ppr b
  ppr (Add x y) = sep [pprP x, text "+", pprP y]
  ppr (Sub x y) = sep [pprP x, text "-", pprP y]
  ppr (Equal x y) = sep [pprP x, text "==", pprP y]
  ppr (Not x) = sep [text "!", pprP x]
  ppr (And x y) = sep [pprP x, text "&&", pprP y]

instance Ppr a => Ppr (SimpleExpr a) where
  ppr (BaseExpr e) = ppr e
  ppr (WithIn vars bnd body) =
    sep [text "with", ppr vars, text ":=", ppr bnd, text "in", ppr body]

  ppr (SslAssertion vars heaplets) =
    sep [text "layout", ppr vars, ppr heaplets]

instance Ppr a => Ppr (Expr a) where
  ppr (SimpleExpr e) = ppr e
  ppr (App f x) = ppr f <+> ppr x

instance IsNested (Base a) where
  isNested (V _) = False
  isNested (LayoutV _) = False
  isNested (IntLit _) = False
  isNested (BoolLit _) = False

  isNested (Add {}) = True
  isNested (Sub {}) = True
  isNested (Equal {}) = True
  isNested (Not {}) = True
  isNested (And {}) = True

