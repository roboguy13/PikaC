{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

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

import Control.Lens hiding (elements)
import Control.Lens.TH

import Data.Data
import Data.Maybe

import Data.Validity
import Test.QuickCheck

import Control.DeepSeq

import Debug.Trace

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

instance NFData Expr

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

instance Plated Expr where
  plate f (V x) = pure $ V x
  plate f (IntLit i) = pure $ IntLit i
  plate f (BoolLit b) = pure $ BoolLit b
  plate f (ApplyLayout e a) =
    ApplyLayout <$> plate f e <*> pure a
  plate f (App k xs) =
    App k <$> traverse (plate f) xs
  plate f (Add x y) =
    Add <$> plate f x <*> plate f y
  plate f (Sub x y) =
    Sub <$> plate f x <*> plate f y
  plate f (Equal x y) =
    Equal <$> plate f x <*> plate f y
  plate f (LName x) = pure $ LName x

-- example :: Expr
-- example =
--   ApplyLayout
--     (LayoutLambda (AdtName "A")
--       (bind (string2Name "alpha")
--         (ApplyLayout (IntLit 1) (LayoutName' (string2Name "alpha")))))
--     (LayoutName' (string2Name "TestLayout"))

instance HasVar Expr where mkVar = V
instance IsBase Expr where
  isVar (V {}) = True
  isVar _ = False

  isLit (IntLit {}) = True
  isLit (BoolLit {}) = True
  isLit _ = False

instance Subst Expr AdtName

instance Alpha Expr
instance Subst Expr Expr where
  isvar (V n) = Just $ SubstName n
  isvar _ = Nothing

instance Subst Expr a => Subst Expr (Loc a)

type ExprName = Name Expr

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
instance Subst Expr (Layout Expr)

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

--
-- Property tests --
--

layoutTest :: Layout Expr
layoutTest =
  Layout
  { _layoutName = "Test"
  , _layoutAdt = AdtName "A"
  , _layoutBranches =
      let x = string2Name "x"
          n = string2Name "n"
          nxt = string2Name "nxt"
          z = string2Name "z"
      in
      bind [Moded Out x]
        [LayoutBranch
          $ PatternMatch
              $ bind (Pattern "C" [n])
                  $ bind [Exists $ Moded In nxt]
                    $ LayoutBody
                        [LPointsTo ((V x :+ 0) :-> V n)
                        ,LPointsTo ((V x :+ 1) :-> V nxt)
                        ,LPointsTo ((V x :+ 2) :-> V z)
                        ]
        ]
  }

instance WellScoped (Name Expr) Expr
-- TODO: Figure out a way to handle this case properly
instance WellScoped (Name Expr) (Bind LayoutName Expr) where
  wellScoped inScopeVars (B v body) =
    wellScoped inScopeVars body
instance WellScoped (Name Expr) (Name Type) where
  wellScoped _ _ = mempty
instance WellScoped a Bool where
  wellScoped _ _ = mempty

instance Arbitrary Expr where
  arbitrary = error "Arbitrary Expr"
  shrink (App f xs) = do
    xs' <- sequenceA $ map shrink xs
    pure $ App f xs'
  shrink e0 = genericShrink e0

-- isValidExpr :: Expr -> Validation
-- isValidExpr = mconcat . map go . universe
--   where
--     go (App f 

isConcreteExpr :: Expr -> Validation
isConcreteExpr e0 = mconcat (map go (universe e0))
  where
    go (LName {}) = invalid "Concrete expression should not have an LName"
    go (LayoutLambda {}) = invalid "Concrete expression should not have a layout lambda"
    go _ = mempty

genConcreteExpr' ::
   [(String, [Maybe LayoutName], LayoutName)] -> -- Function signatures
   [(LayoutName, [(String, [Maybe LayoutName])])] -> -- Layouts with their constructors and those constructors' arities
   [(ExprName, Maybe LayoutName)] -> -- Local names
   Int -> -- Generator size
   Gen Expr
genConcreteExpr' fnSigs layouts locals size =
  oneof
    [genSimpleExpr' (getSimpleLocals locals) size
    ,do
      fnSig <- elements' fnSigs
      genCall fnSigs layouts locals (size-1) fnSig
    ,do
      constructorSig <- elements' layouts
      genConstructorApp fnSigs layouts locals (size-1) constructorSig
    ]

genForLayout :: 
   [(String, [Maybe LayoutName], LayoutName)] -> -- Function signatures
   [(LayoutName, [(String, [Maybe LayoutName])])] -> -- Layouts with their constructors and those constructors' arities
   [(ExprName, Maybe LayoutName)] -> -- Local names
   Int -> -- Generator size
   LayoutName ->
   Gen Expr
genForLayout fnSigs layouts locals size layoutName =
  let layoutLocals = filter ((== layoutName) . snd) (mapMaybe (\(x, y) -> fmap (x,) y) locals)
  in
  oneof $
    if not (null layoutLocals)
      then [elements' layoutLocals >>= \case
            (var, layoutName') ->
                pure $ V var]
      else []
    ++

    [do
      layout@(layoutName', constructors) <- elements' (filter ((== layoutName) . fst) layouts)
      genConstructorApp fnSigs layouts locals (size-1) layout

    ,do
      let filteredFnSigs = filter (\(_, _, z) -> z == layoutName) fnSigs
      if null filteredFnSigs
        then discardM
        else do
          fnSig@(fn, inLayouts, outLayout) <-
            elements' filteredFnSigs
          genCall fnSigs layouts locals size fnSig
    ]

-- | On Nothing, generate a simple expression
genForMaybeLayout ::
   [(String, [Maybe LayoutName], LayoutName)] -> -- Function signatures
   [(LayoutName, [(String, [Maybe LayoutName])])] -> -- Layouts with their constructors and those constructors' arities
   [(ExprName, Maybe LayoutName)] -> -- Local names
   Int -> -- Generator size
   Maybe LayoutName ->
   Gen Expr
genForMaybeLayout fnSigs layouts locals size Nothing = genSimpleExpr' (getSimpleLocals locals) size
  where
genForMaybeLayout fnSigs layouts locals size (Just layoutName) =
  genForLayout fnSigs layouts locals size layoutName

getSimpleLocals :: [(ExprName, Maybe LayoutName)] -> [ExprName]
getSimpleLocals ((x, Nothing):xs) = x : getSimpleLocals xs
getSimpleLocals ((_, Just _):xs) = getSimpleLocals xs
getSimpleLocals [] = []

genCall :: 
   [(String, [Maybe LayoutName], LayoutName)] -> -- Function signatures
   [(LayoutName, [(String, [Maybe LayoutName])])] -> -- Layouts with their constructors and those constructors' arities
   [(ExprName, Maybe LayoutName)] -> -- Local names
   Int -> -- Generator size
   (String, [Maybe LayoutName], LayoutName) ->
   Gen Expr
genCall fnSigs layouts locals size (fn, inLayouts, outLayout) = do
  let newSize = size `div` length (inLayouts ++ [Just outLayout])
  App fn <$> mapM (genForMaybeLayout fnSigs layouts locals newSize) inLayouts

genConstructorApp ::
   [(String, [Maybe LayoutName], LayoutName)] -> -- Function signatures
   [(LayoutName, [(String, [Maybe LayoutName])])] -> -- Layouts with their constructors and those constructors' arities
   [(ExprName, Maybe LayoutName)] -> -- Local names
   Int -> -- Generator size
   (LayoutName, [(String, [Maybe LayoutName])]) ->
   Gen Expr
genConstructorApp _      _       _      size _ | size <= 0 = discardM
genConstructorApp fnSigs layouts locals size (layout, constructorSigs) = do
  (cName, arity) <- elements' constructorSigs
  let newSize = size `div` length arity
  ApplyLayout
    <$> (App cName <$> mapM (genForMaybeLayout fnSigs layouts locals newSize) arity)
    <*> pure layout

genSimpleExpr' :: [ExprName] -> Int -> Gen Expr
genSimpleExpr' [] _ =
  oneof
    [IntLit <$> arbitrary
    ,BoolLit <$> arbitrary
    ]
genSimpleExpr' locals 0 =
  oneof
    [V <$> elements' locals
    ,genSimpleExpr' [] 0
    ]
genSimpleExpr' locals size =
  oneof
    [genSimpleExpr' locals 0
    ,Add <$> halvedGen <*> halvedGen
    ,Sub <$> halvedGen <*> halvedGen
    ,Equal <$> halvedGen <*> halvedGen
    ]
  where
    halvedGen = genSimpleExpr' locals (size `div` 2)

-- -- Tests
-- sllLayout :: Layout Expr
-- sllLayout =
--   Layout
--     { _layoutName = "Sll"
--     , _layoutAdt = AdtName "List"
--     , _layoutBranches =
--         let x = string2Name "x"
--         in
--         bind [Moded Out x]
--           [LayoutBranch
--             (PatternMatch
--               (bind (Pattern "Nil" [])
--                 (bind [] mempty)))
--
--           ,let h = string2Name "h"
--                t = string2Name "t"
--                nxt = string2Name "nxt"
--            in
--            LayoutBranch
--              (PatternMatch
--                (bind (Pattern "Cons" [h, t])
--                  (bind [Exists $ Moded In nxt]
--                    $ LayoutBody
--                        [LPointsTo $ (V x :+ 0) :-> V h
--                        ,LPointsTo $ (V x :+ 1) :-> V nxt
--                        ,LApply "Sll"
--                           (V t)
--                           [V nxt]
--                        ])))
--           ]
--     }
--
-- dllLayout :: Layout Expr
-- dllLayout =
--   Layout
--     { _layoutName = "Dll"
--     , _layoutAdt = AdtName "List"
--     , _layoutBranches =
--         let x = string2Name "x"
--             z = string2Name "z"
--         in
--         bind [Moded Out x, Moded In z]
--           [LayoutBranch
--             (PatternMatch
--               (bind (Pattern "Nil" [])
--                 (bind [] mempty)))
--
--           ,let h = string2Name "h"
--                t = string2Name "t"
--                nxt = string2Name "nxt"
--            in
--            LayoutBranch
--              (PatternMatch
--                (bind (Pattern "Cons" [h, t])
--                  (bind [Exists $ Moded In nxt]
--                    $ LayoutBody
--                        [LPointsTo $ (V x :+ 0) :-> V h
--                        ,LPointsTo $ (V x :+ 1) :-> V nxt
--                        ,LPointsTo $ (V x :+ 2) :-> V z
--                        ,LApply "Dll"
--                           (V t)
--                           [V nxt, V x]
--                        ])))
--           ]
--     }

