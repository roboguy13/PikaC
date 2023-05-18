{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import GHC.Stack

type ExprName = Name Expr

type instance PType Expr = Expr

-- type LocName = ExprName

data Expr
  = V ExprName
  | LayoutV [ExprName]    -- {x ...}
  | IntLit Int -- TODO: Add output locations?
  | BoolLit Bool

  | Add Expr Expr
  | Sub Expr Expr
  | Equal Expr Expr
  | Not Expr
  | And Expr Expr
  -- | App (Expr a) (LayoutArg a)
  | WithIn                -- with
      Expr                --     := e
      [ExprName]     --   {x ...}
      Expr       -- in e

  | SslAssertion            -- layout
      (LayoutArg Expr)     --     {x ...}
      ExprAssertion     --   { (x+1) :-> e ** ... }
  | App String [Expr] -- | Fully saturated function application
  deriving (Show, Generic)

type PointsToExpr = PointsTo Expr
type ExprAssertion = [PointsToExpr]

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

instance Alpha Expr

-- instance Subst Expr LocVar

instance Subst ExprName (PointsTo Expr) where

instance Subst ExprName (Loc a) where

instance Subst ExprName Expr

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

instance Subst Expr Expr where
  isvar (V x) = Just $ SubstName x
  isvar _ = Nothing

instance Subst Expr a => Subst Expr (PointsTo a) where
  isCoerceVar (x :-> y) = do
    SubstCoerce p q <- isCoerceVar @Expr @(Loc a) x
    pure (SubstCoerce p (fmap (:-> y) . q))

instance forall a. Subst Expr a => Subst Expr (Loc a) where
  isCoerceVar (x :+ i) = do
    SubstCoerce p q <- isCoerceVar @Expr @(Name a) x
    pure (SubstCoerce p (fmap (:+ i) . q))
  -- isCoerceVar (V n) = Just $ SubstCoerce _ Just
  -- isCoerceVar _ = Nothing
  -- isvar (V x) = Just $ SubstName x
  --

instance Subst Expr a => Subst Expr (LayoutBody a)
instance Subst Expr a => Subst Expr (LayoutHeaplet a)

getPointsToExpr :: Expr -> [PointsToExpr]
getPointsToExpr e = e ^.. (_SslAssertion . _2 . traversed)

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

instance Ppr Expr where
  ppr (WithIn bnd vars body) =
    sep [hsep [text "with {", hsep . punctuate (text ",") $ map go vars, text "} :=", ppr bnd], text "in", ppr body]
      where
        go x = text "<" <> ppr x <> text ">"

  ppr (SslAssertion vars heaplets) =
    sep [text "layout", ppr vars, ppr heaplets]
  ppr (V x) = ppr x
  ppr (LayoutV x) = text "{" <+> hsep (punctuate (text ",") (map ppr x)) <+> text "}"
  ppr (IntLit i) = ppr i
  ppr (BoolLit b) = ppr b
  ppr (Add x y) = sep [pprP x, text "+", pprP y]
  ppr (Sub x y) = sep [pprP x, text "-", pprP y]
  ppr (Equal x y) = sep [pprP x, text "==", pprP y]
  ppr (Not x) = sep [text "!", pprP x]
  ppr (And x y) = sep [pprP x, text "&&", pprP y]
  ppr (App f x) = ppr f <+> hsep (map pprP x)

instance IsNested Expr where
  isNested (V _) = False
  isNested (LayoutV _) = False
  isNested (IntLit _) = False
  isNested (BoolLit _) = False

  isNested (Add {}) = True
  isNested (Sub {}) = True
  isNested (Equal {}) = True
  isNested (Not {}) = True
  isNested (And {}) = True
  isNested (App {}) = True
  isNested (WithIn {}) = True
  isNested (SslAssertion {}) = True

isBase :: Expr -> Bool
isBase (V x) = True
isBase (IntLit i) = True
isBase (BoolLit b) = True
isBase (Add x y) = True
isBase (Sub x y) = True
isBase (Equal x y) = True
isBase (Not x) = True
isBase (And x y) = True
isBase _ = False

