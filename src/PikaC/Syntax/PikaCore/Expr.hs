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
import PikaC.Utils
import PikaC.Ppr
import Data.List
import Data.Foldable

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Bind

import PikaC.Syntax.Heaplet
import PikaC.Syntax.Pika.Layout
import PikaC.Syntax.Pika.Pattern

import Control.Lens
import Control.Lens.Action

import GHC.Generics hiding (to)
import GHC.Stack

type ExprName = Name Expr

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
      Expr
      (Bind [ModedName Expr]
        Expr)
      -- Expr                --     := e
      -- [ExprName]     --   {x ...}
      -- Expr       -- in e

  | SslAssertion            -- layout
      (Bind [ModedName Expr]
         ExprAssertion)
      -- (LayoutArg Expr)     --     {x ...}
      -- ExprAssertion     --   { (x+1) :-> e ** ... }

  | App String [Expr] -- | Fully saturated function application
  deriving (Show, Generic)

instance HasVar Expr where mkVar = V

-- TODO: Does this work correct w.r.t. Bind's, etc?
instance Plated Expr where
  plate f (V x) = pure $ V x
  plate f (LayoutV x) = pure $ LayoutV x
  plate f (IntLit i) = pure $ IntLit i
  plate f (BoolLit b) = pure $ BoolLit b
  plate f (Add x y) = Add <$> f x <*> f y
  plate f (Sub x y) = Sub <$> f x <*> f y
  plate f (Equal x y) = Equal <$> f x <*> f y
  plate f (Not x) = Not <$> f x
  plate f (And x y) = And <$> f x <*> f y
  plate f (WithIn x (B y z)) =
      WithIn <$> f x <*> (B y <$> f z)
  plate f (SslAssertion (B a b)) =
    let z = plate (traverseOf (traversed.pointsToRhsLens) f) b
    in
    SslAssertion . B a <$> z
  plate f (App x ys) = App x <$> traverse f ys

type PointsToExpr = PointsTo Expr
type ExprAssertion = [PointsToExpr]

makePrisms ''Expr

instance HasApp Expr where
  mkApp = App

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

instance Subst Expr (ModedName a)
instance Subst Expr Mode
instance Subst ExprName (ModedName a)
instance Subst ExprName Mode

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

getPointsToExpr :: forall m. Fresh m => Expr -> m [PointsToExpr]
getPointsToExpr e = e ^!! (cosmos . _SslAssertion . to unbind' . acts . _2 . traversed)
  where
    unbind' = unbind @_ @_ @m


-- getPointsToExpr :: Expr -> [PointsToExpr]
-- getPointsToExpr e = e ^.. (cosmos . _SslAssertion . _)

-- getPointsToExpr e = e ^.. (cosmos . _SslAssertion . _2 . traversed)

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
  ppr (WithIn bnd (B vars body)) =
    sep [hsep [text "with {", hsep . punctuate (text ",") $ map go vars, text "} :=", ppr bnd], text "in", ppr body]
      where
        go x = text "<" <> ppr x <> text ">"

  ppr (SslAssertion (B vars heaplets)) =
    sep [text "layout", text "{" <+> hsep (punctuate (text ",") (map ppr vars)) <+> text "}", ppr heaplets]
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

