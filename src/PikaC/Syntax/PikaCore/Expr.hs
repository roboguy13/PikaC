{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wincomplete-patterns #-}

module PikaC.Syntax.PikaCore.Expr
  where

-- import PikaC.Syntax.Heaplet
import PikaC.Ppr
import Data.List
import Data.Foldable

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Bind

import PikaC.Syntax.Heaplet
import PikaC.Syntax.Pika.Layout
import PikaC.Syntax.Pika.Pattern

import Control.Lens
import Control.Lens.TH

import GHC.Generics

type ExprName = Name Expr

type instance PType Base = Expr
type instance PType SimpleExpr = Expr
type instance PType Expr = Expr

-- type LocName = ExprName

data Base
  = V ExprName
  | LayoutV [ExprName]    -- {x ...}
  | IntLit Int -- TODO: Add output locations?
  | BoolLit Bool

  | Add Base Base
  | Sub Base Base
  | Equal Base Base
  | Not Base
  | And Base Base
  deriving (Show, Generic)

data SimpleExpr
  = BaseExpr Base
  | WithIn                -- with
      Expr                --     := e
      (Bind [ExprName]     --   {x ...}
        SimpleExpr)       -- in e

  | SslAssertion            -- layout
      (LayoutArg Expr)     --     {x ...}
      ExprAssertion     --   { (x+1) :-> e ** ... }
  deriving (Show, Generic)

data Expr
  = SimpleExpr SimpleExpr
  -- | App (Expr a) (LayoutArg a)
  | App String [Expr] -- | Fully saturated function application
  deriving (Show, Generic)

type PointsToExpr = PointsTo Expr
type ExprAssertion = [PointsToExpr]

makePrisms ''Base
makePrisms ''SimpleExpr
makePrisms ''Expr

-- example :: SimpleExpr
-- example =
--   WithIn
--     (SimpleExpr (BaseExpr (IntLit 1)))
--     exampleBind

-- exampleBind :: Bind [LocName] SimpleExpr
-- exampleBind =
--   bind [string2Name "x"]
--     (BaseExpr (LocV (string2Name "x")))

instance Alpha Base
instance Alpha SimpleExpr
instance Alpha Expr

-- instance Subst Expr LocVar

instance Subst ExprName (PointsTo Expr)

instance Subst ExprName (Loc a)
instance Subst ExprName Expr
instance Subst ExprName SimpleExpr
instance Subst ExprName Base

-- instance Subst LocVar Base where
  -- isCoerceVar (LocV n) = Just $ SubstCoerce n (Just . LocV . string2Name . unLocVar)
  -- isCoerceVar _ = Nothing

-- instance Subst LocVar SimpleExpr where
  -- isCoerceVar (BaseExpr e) = do
  --   SubstCoerce x f <- isCoerceVar @LocVar @Base e
  --   pure $ SubstCoerce x (fmap BaseExpr . f)
  -- isCoerceVar _ = Nothing

-- instance Subst LocVar Expr where
  -- isCoerceVar (SimpleExpr e) = do
  --   SubstCoerce x f <- isCoerceVar @LocVar @SimpleExpr e
  --   pure $ SubstCoerce x (fmap SimpleExpr . f)
  -- isCoerceVar _ = Nothing

instance Subst Expr Base where
  -- isCoerceVar (V n) = Just $ SubstCoerce n go
  --   where
  --     go :: Expr -> Maybe Base
  --     go (SimpleExpr (BaseExpr e)) = Just e
  --     go _ = Nothing
  -- isCoerceVar _ = Nothing

instance Subst Expr SimpleExpr where
  -- isCoerceVar (BaseExpr (V n)) = Just $ SubstCoerce n go
  --   where
  --     go :: Expr -> Maybe SimpleExpr
  --     go (SimpleExpr e) = Nothing
  --     go _ = Nothing
  -- isCoerceVar _ = Nothing

instance Subst Expr Expr where
  isvar (SimpleExpr (BaseExpr (V x))) = Just $ SubstName x
  isvar _ = Nothing
instance Subst Expr a => Subst Expr (PointsTo a)
instance Subst Expr (Loc a)

-- instance Subst Expr (LayoutBody Expr)
instance Subst Expr a => Subst Expr (LayoutHeaplet a)
instance Subst Base (LayoutBody Base)
instance Subst Base (LayoutHeaplet Base)
instance Subst Base (PointsTo Base)
instance Subst Base (Loc a)
-- instance Subst Base LocVar

instance Subst Base Base where
  -- isvar (V x) = Just $ SubstName x
  --

instance Subst Expr a => Subst Expr (LayoutBody a)

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
  ppr (LayoutV x) = text "{" <+> hsep (punctuate (text ",") (map ppr x)) <+> text "}"
  ppr (IntLit i) = ppr i
  ppr (BoolLit b) = ppr b
  ppr (Add x y) = sep [pprP x, text "+", pprP y]
  ppr (Sub x y) = sep [pprP x, text "-", pprP y]
  ppr (Equal x y) = sep [pprP x, text "==", pprP y]
  ppr (Not x) = sep [text "!", pprP x]
  ppr (And x y) = sep [pprP x, text "&&", pprP y]

instance Ppr SimpleExpr where
  ppr (BaseExpr e) = ppr e
  ppr (WithIn bnd (B vars body)) =
    sep [hsep [text "with {", hsep . punctuate (text ",") $ map go vars, text "} :=", ppr bnd], text "in", ppr body]
      where
        go x = text "<" <> ppr x <> text ">"

  ppr (SslAssertion vars heaplets) =
    sep [text "layout", ppr vars, ppr heaplets]

instance Ppr Expr where
  ppr (SimpleExpr e) = ppr e
  ppr (App f x) = ppr f <+> hsep (map pprP x)

instance IsNested Expr where
  isNested (SimpleExpr e) = isNested e
  isNested (App {}) = True

instance IsNested SimpleExpr where
  isNested (BaseExpr e) = isNested e
  isNested (WithIn {}) = True
  isNested (SslAssertion {}) = True

instance IsNested Base where
  isNested (V _) = False
  isNested (LayoutV _) = False
  isNested (IntLit _) = False
  isNested (BoolLit _) = False

  isNested (Add {}) = True
  isNested (Sub {}) = True
  isNested (Equal {}) = True
  isNested (Not {}) = True
  isNested (And {}) = True

base :: Base -> Expr
base = SimpleExpr . BaseExpr

simple :: SimpleExpr -> Expr
simple = SimpleExpr

