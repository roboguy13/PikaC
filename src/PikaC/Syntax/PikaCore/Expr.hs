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
  | LayoutV [Expr]    -- {x ...}
  -- | LayoutV [ExprName]    -- {x ...}
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

instance Subst Expr (LayoutBranch Expr)
instance Subst Expr (PatternMatch Expr (Bind [Exists Expr] (LayoutBody Expr)))
instance Subst Expr (Pattern Expr)


-- instance Subst Expr LocVar

-- instance Subst ExprName (PointsTo Expr) where

instance Subst (Name Expr) a => Subst ExprName (Loc a) where

instance Subst ExprName Expr

instance IsName Expr Expr where
  getName (V x) = x
  getName e = error $ "IsName PikaCore.Expr PikaCore.Expr requires var, got " ++ ppr' e

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

instance Subst (Moded Expr) Expr
instance Subst (Moded Expr) (Moded (Name Expr))
instance Subst (Moded Expr) Mode
instance Subst (Moded (Name Expr)) (LayoutBody Expr)
instance Subst (Moded (Name Expr)) (LayoutHeaplet Expr)
instance Subst (Moded (Name Expr)) (PointsTo Expr)
instance Subst (Moded (Name Expr)) (Loc Expr)
instance Subst (Moded (Name Expr)) Expr
instance Subst (Moded (Name Expr)) (Moded (Name Expr))
instance Subst (Moded (Name Expr)) Mode
instance Subst (Moded (Name Expr)) (Exists Expr)

instance Subst Expr Expr where
  isvar (V x) = Just $ SubstName x
  isvar _ = Nothing

instance Subst Expr (ModedName a)
instance Subst Expr Mode
-- instance Subst ExprName (ModedName a)
-- instance Subst ExprName Mode

instance Subst Expr a => Subst Expr (PointsTo a) where
  isCoerceVar (x :-> y) = do
    SubstCoerce p q <- isCoerceVar @Expr @(Loc a) x
    pure (SubstCoerce p (fmap (:-> y) . q))

instance forall a. Subst Expr a => Subst Expr (Loc a) where
  isCoerceVar (x :+ i) = do
    SubstCoerce p q <- isCoerceVar @Expr @_ x
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

instance Subst (Exists Expr) Expr

instance Ppr Expr where
  ppr = runFreshM . pprExpr

pprExpr :: Expr -> FreshM Doc
pprExpr (WithIn e bnd) = do
  -- (vars, body0) <- unbind bnd
  -- let body = instantiate bnd vars
  (vars, body) <- freshOpen @_ @Name bnd
  bodyDoc <- pprExpr body
  eDoc <- pprExpr e
  pure $ sep [hsep [text "with {", hsep . punctuate (text ",") $ map go vars, text "} :=", eDoc], text "in", bodyDoc]
    where
      go x = text "<" <> ppr x <> text ">"

pprExpr (SslAssertion bnd) = do
  (vars, heaplets) <- freshOpen @_ @Name bnd
  -- (vars, heaplets0) <- unbind bnd
  -- let heaplets = instantiate bnd vars
  pure $ sep [text "layout", text "{" <+> hsep (punctuate (text ",") (map ppr vars)) <+> text "}", ppr heaplets]
pprExpr (V x) = pure $ ppr x
pprExpr (LayoutV x) = pure $ text "{" <+> hsep (punctuate (text ",") (map ppr x)) <+> text "}"
pprExpr (IntLit i) = pure $ ppr i
pprExpr (BoolLit b) = pure $ ppr b
pprExpr (Add x y) = do
  xDoc <- pprExpr x
  yDoc <- pprExpr y
  pure $ sep [xDoc, text "+", yDoc]
pprExpr (Sub x y) = do
  xDoc <- pprExpr x
  yDoc <- pprExpr y
  pure $ sep [xDoc, text "-", yDoc]
pprExpr (Equal x y) = do
  xDoc <- pprExpr x
  yDoc <- pprExpr y
  pure $ sep [xDoc, text "==", yDoc]
pprExpr (Not x) = do
  xDoc <- pprExpr x
  pure $ sep [text "!", xDoc]
pprExpr (And x y) = do
  xDoc <- pprExpr x
  yDoc <- pprExpr y
  pure $ sep [xDoc, text "&&", yDoc]
pprExpr (App f x) = do
  xDoc <- mapM pprExpr x
  pure $ ppr f <+> hsep xDoc

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

