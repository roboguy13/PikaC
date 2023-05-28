{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

module PikaC.Syntax.Pika.Expr
  where

import PikaC.Syntax.Type
import PikaC.Utils

-- import Bound
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Bind
import Text.Show.Deriving

import Control.Monad
import Data.Void

import GHC.Generics

import PikaC.Syntax.Heaplet
import PikaC.Syntax.Pika.Layout
import PikaC.Syntax.Pika.Pattern

import PikaC.Ppr
import PikaC.Utils

import Control.Lens
import Control.Lens.TH

import Data.Data

data Expr
  = V ExprName
  | IntLit Int
  | BoolLit Bool
  | LayoutLambda AdtName (Bind LayoutName Expr)
  | ApplyLayout Expr TypeName
  | App String [Expr]
  | Add Expr Expr
  | Sub Expr Expr
  | Equal Expr Expr
  | LName LayoutName -- TODO: Remove?
  -- | Not Expr
  -- | And Expr Expr
  deriving (Show, Generic)

instance HasApp Expr where
  mkApp = App

instance Ppr Expr where
  ppr (V x) = ppr x
  ppr (IntLit i) = ppr i
  ppr (BoolLit b) = ppr b
  ppr (LayoutLambda a (B v p)) = hsep [text "/\\(" <> ppr v <> text " :~ " <> ppr a <> text ").", ppr p]
  ppr (ApplyLayout e ty) = hsep [ppr e, text "[" <> ppr ty <> text "]"]
  ppr (App f xs) = hsep (ppr f : map pprP xs)
  ppr (Add x y) = hsep [pprP x, text "+", pprP y]
  ppr (Sub x y) = hsep [pprP x, text "-", pprP y]
  ppr (Equal x y) = hsep [pprP x, text "==", pprP y]
  ppr (LName x) = ppr x

instance IsNested Expr where
  isNested (V _) = False
  isNested (IntLit _) = False
  isNested (BoolLit _) = False
  isNested (LayoutLambda {}) = True
  isNested (ApplyLayout {}) = True
  isNested (App {}) = True
  isNested (Add {}) = True
  isNested (Sub {}) = True
  isNested (Equal {}) = True
  isNested (LName {}) = False

-- example :: Expr
-- example =
--   ApplyLayout
--     (LayoutLambda (AdtName "A")
--       (bind (string2Name "alpha")
--         (ApplyLayout (IntLit 1) (LayoutName' (string2Name "alpha")))))
--     (LayoutName' (string2Name "TestLayout"))

instance HasVar Expr where mkVar = V

instance Subst Expr AdtName

instance Alpha Expr
instance Subst Expr Expr where
  isvar (V n) = Just $ SubstName n
  isvar _ = Nothing

instance Subst Expr a => Subst Expr (Loc a)

type ExprName = Name Expr

type LayoutName = TypeName

-- newtype LayoutName' = LayoutName' LayoutName
--   deriving (Eq, Ord, Show, Generic)
-- type LayoutName = Name LayoutName'

instance Subst Expr (LayoutBody Expr)
instance Subst Expr (LayoutHeaplet Expr)
instance Subst Expr (PointsTo Expr)
instance Subst Expr (ModedName Expr)
instance Subst Expr Mode
-- instance Subst Expr LocVar
instance Subst (Exists Expr) Expr
instance Subst (Pattern Expr) Expr

instance Subst (Name Expr) Expr
instance Subst (Name Expr) AdtName

instance Subst Expr (LayoutBranch Expr)
instance Subst Expr (PatternMatch Expr (Bind [Exists Expr] (LayoutBody Expr)))
instance Subst Expr (Pattern Expr)

instance Subst (Moded Expr) Expr
instance Subst (Moded Expr) AdtName

makePrisms ''Expr

instance IsName Expr Expr where
  getName (V x) = x
  getName e = error $ "IsName Pika.Expr Pika.Expr requires var, got " ++ ppr' e

-- | No layout lambdas
isConcrete :: Expr -> Bool
isConcrete (V {}) = True
isConcrete (IntLit {}) = True
isConcrete (BoolLit {}) = True
isConcrete (LayoutLambda {}) = False
isConcrete (ApplyLayout e _) = isConcrete e
isConcrete (App f xs) = all isConcrete xs

reduceLayouts :: Expr -> Expr
reduceLayouts = go
  where
    go :: Expr -> Expr
    go (V x) = V x
    go (IntLit i) = IntLit i
    go (BoolLit b) = BoolLit b
    go (LayoutLambda a (B p t)) =
      LayoutLambda a (B p (go t))
    go (ApplyLayout e arg) =
      case go e of
        (LayoutLambda _ (B p e)) ->
          rename [(p, arg)] e
          -- substBind bnd arg
        e' -> ApplyLayout e' arg
    go (App f args) =
      App f (map go args)
    go (Add x y) = Add (go x) (go y)
    go (Sub x y) = Sub (go x) (go y)
    go (Equal x y) = Equal (go x) (go y)
    go (LName x) = LName x

-- Tests
sllLayout :: Layout Expr
sllLayout =
  Layout
    { _layoutName = "Sll"
    , _layoutAdt = AdtName "List"
    , _layoutBranches =
        let x = string2Name "x"
        in
        bind [Moded Out x]
          [LayoutBranch
            (PatternMatch
              (bind (Pattern "Nil" [])
                (bind [] mempty)))

          ,let h = string2Name "h"
               t = string2Name "t"
               nxt = string2Name "nxt"
           in
           LayoutBranch
             (PatternMatch
               (bind (Pattern "Cons" [h, t])
                 (bind [Exists $ Moded In nxt]
                   $ LayoutBody
                       [LPointsTo $ (V x :+ 0) :-> V h
                       ,LPointsTo $ (V x :+ 1) :-> V nxt
                       ,LApply "Sll"
                          (V t)
                          [V nxt]
                       ])))
          ]
    }

dllLayout :: Layout Expr
dllLayout =
  Layout
    { _layoutName = "Dll"
    , _layoutAdt = AdtName "List"
    , _layoutBranches =
        let x = string2Name "x"
            z = string2Name "z"
        in
        bind [Moded Out x, Moded In z]
          [LayoutBranch
            (PatternMatch
              (bind (Pattern "Nil" [])
                (bind [] mempty)))

          ,let h = string2Name "h"
               t = string2Name "t"
               nxt = string2Name "nxt"
           in
           LayoutBranch
             (PatternMatch
               (bind (Pattern "Cons" [h, t])
                 (bind [Exists $ Moded In nxt]
                   $ LayoutBody
                       [LPointsTo $ (V x :+ 0) :-> V h
                       ,LPointsTo $ (V x :+ 1) :-> V nxt
                       ,LPointsTo $ (V x :+ 2) :-> V z
                       ,LApply "Dll"
                          (V t)
                          [V nxt, V x]
                       ])))
          ]
    }

