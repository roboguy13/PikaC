{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module PikaC.Syntax.Pika.Expr
  where

import PikaC.Syntax.Type
import PikaC.Utils

-- import Bound
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Bind
import Unbound.Generics.LocallyNameless.Unsafe
import Text.Show.Deriving

import Control.Monad
import Data.Void

import GHC.Generics

import PikaC.Stage
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
  | LayoutLambda AdtName (Bind TypeName Expr)
  | ApplyLayout Expr Type -- The type should either be a TyVar or a LayoutId
  | App Expr [Expr]
  | Div Expr Expr
  | Mod Expr Expr
  | Add Expr Expr
  | And Expr Expr
  | IfThenElse Expr Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Equal Expr Expr
  | Lt Expr Expr
  | Le Expr Expr
  | Not Expr

  | Lambda (Bind [ExprName] Expr)

  -- For the ghost language:
  | EmptySet
  | SingletonSet Expr
  | SetUnion Expr Expr
  deriving (Show, Generic)

instance Size Expr where
  size (V _) = 1
  size (IntLit _) = 1
  size (BoolLit _) = 1
  size (LayoutLambda x y) = visibleNode $ size x + size y
  size (ApplyLayout x y) = visibleNode $ size x + size y
  size (App f xs) = visibleNode $ size f + size xs
  size (Div x y) = visibleNode $ size x + size y
  size (Mod x y) = visibleNode $ size x + size y
  size (Add x y) = visibleNode $ size x + size y
  size (And x y) = visibleNode $ size x + size y
  size (IfThenElse x y z) = visibleNode $ size x + size y + size z
  size (Mul x y) = visibleNode $ size x + size y
  size (Sub x y) = visibleNode $ size x + size y
  size (Equal x y) = visibleNode $ size x + size y
  size (Lt x y) = visibleNode $ size x + size y
  size (Le x y) = visibleNode $ size x + size y
  size (Not x) = visibleNode $ size x
  size (Lambda bnd) = visibleNode $ size bnd
  size EmptySet = 1
  size (SingletonSet x) = visibleNode $ size x
  size (SetUnion x y) = visibleNode $ size x + size y
  

data Test a =
  Test
  { _testName :: String
  , _testExpr :: a
  , _testResultType :: Type
  }
  deriving (Show, Generic)

-- NOTE: We completely exclude test directives from the AST node count
instance Size (Test a) where size _ = 0

instance NFData Expr
instance NFData a => NFData (Test a)

instance HasApp Expr where
  mkApp f = App (V f)

instance Ppr a => Ppr (Test a) where
  ppr test =
    ((text "test" <+> text (show (_testName test)) <+> ppr (_testResultType test))
      <> text ":")
      <+> ppr (_testExpr test)

instance Ppr Expr where
  ppr (V x) = text $ name2String x
  ppr (IntLit i) = ppr i
  ppr (BoolLit b) = ppr b
  ppr (LayoutLambda a (B v p)) = hsep [text "/\\(" <> ppr v <> text " :~ " <> ppr a <> text ").", ppr p]
  ppr (Lambda bnd) =
    let (vs, body) = unsafeUnbind bnd
    in
    hsep (text "\\" : map ppr vs) <+> text "." <+> ppr body
  ppr (ApplyLayout e ty) = hsep [pprP e, text "[" <> ppr ty <> text "]"]
  ppr (App f xs) = hsep (ppr f : map pprP xs)
  ppr (Mul x y) = hsep [pprP x, text "*", pprP y]
  ppr (Mod x y) = hsep [pprP x, text "%", pprP y]
  ppr (Div x y) = hsep [pprP x, text "/", pprP y]
  ppr (Add x y) = hsep [pprP x, text "+", pprP y]
  ppr (And x y) = hsep [pprP x, text "&&", pprP y]
  ppr (IfThenElse x y z) = hsep [text "if", ppr x, text "then", ppr y, text "else", ppr z]
  ppr (Sub x y) = hsep [pprP x, text "-", pprP y]
  ppr (Equal x y) = hsep [pprP x, text "==", pprP y]
  ppr (Lt x y) = hsep [pprP x, text "<", pprP y]
  ppr (Le x y) = hsep [pprP x, text "<=", pprP y]
  ppr (Not x) = hsep [text "not", parens (pprP x)]
  ppr EmptySet = text "{}"
  ppr (SingletonSet x) = braces (ppr x)
  ppr (SetUnion x y) = pprP x <+> text "++" <+> pprP y

instance IsNested Expr where
  isNested (V _) = False
  isNested (IntLit _) = False
  isNested (BoolLit _) = False
  isNested (LayoutLambda {}) = True
  isNested (Lambda {}) = True
  isNested (ApplyLayout {}) = True
  isNested (App {}) = True
  isNested (Div {}) = True
  isNested (Mod {}) = True
  isNested (Add {}) = True
  isNested (And {}) = True
  isNested (IfThenElse {}) = True
  isNested (Mul {}) = True
  isNested (Sub {}) = True
  isNested (Equal {}) = True
  isNested (Lt {}) = True
  isNested (Le {}) = True
  isNested (Not {}) = True
  isNested EmptySet = False
  isNested (SingletonSet {}) = False
  isNested (SetUnion {}) = True

instance Plated Expr where
  plate f (V x) = pure $ V x
  plate f (IntLit i) = pure $ IntLit i
  plate f (BoolLit b) = pure $ BoolLit b
  plate f (ApplyLayout e a) =
    ApplyLayout <$> f e <*> pure a
  plate f (Lambda bnd) =
    let (vs, body) = unsafeUnbind bnd
    in
    Lambda . bind vs <$> f body
  plate f (App k xs) =
    App k <$> traverse f xs
  plate f (Mod x y) =
    Mod <$> f x <*> f y
  plate f (Div x y) =
    Div <$> f x <*> f y
  plate f (And x y) =
    And <$> f x <*> f y
  plate f (IfThenElse x y z) =
    IfThenElse <$> f x <*> f y <*> f z
  plate f (Add x y) =
    Add <$> f x <*> f y
  plate f (Mul x y) =
    Mul <$> f x <*> f y
  plate f (Sub x y) =
    Sub <$> f x <*> f y
  plate f (Equal x y) =
    Equal <$> f x <*> f y
  plate f (Lt x y) =
    Lt <$> f x <*> f y
  plate f (Le x y) =
    Le <$> f x <*> f y
  plate f (Not x) = Not <$> f x
  plate f (SingletonSet x) = SingletonSet <$> f x
  plate f EmptySet = pure EmptySet
  plate f (SetUnion x y) = SetUnion <$> f x <*> f y

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

  intLit = IntLit
  boolLit = BoolLit
  mkNot = Not
  mkEqual = Equal
  mkAnd = error "Pika.mkAnd"

-- instance Subst Expr AdtName

instance Alpha Expr
-- instance (Subst a AdtName, Subst a Expr) => Subst a LayoutConstraint
-- instance (Subst a AdtName, Subst a Expr, Subst a Type) => Subst a ConstrainedType
-- instance (Subst (f a) Expr, Subst (f a) AdtName, Subst a Type) => Subst (f a) Type
-- instance (Subst (f a) a, Subst a Type) => Subst (f a) Expr
-- instance (Subst (f a) a) => Subst (f a) AdtName
instance Subst Expr Type
-- instance (Subst a AdtName, Subst a Expr) => Subst a Type
-- instance Subst Expr Type
instance Subst Expr Expr where
  isvar (V n) = Just $ SubstName n
  isvar _ = Nothing

-- instance Subst (Exists Expr) Expr

-- instance Subst Expr a => Subst Expr (Loc a)

instance Subst (f a) (f a) => Subst (f a) Expr

type ExprName = Name Expr

-- newtype LayoutName' = LayoutName' LayoutName
--   deriving (Eq, Ord, Show, Generic)
-- type LayoutName = Name LayoutName'

-- instance Subst Expr (LayoutBody Expr)
-- instance Subst Expr (LayoutHeaplet Expr)
-- instance Subst Expr (PointsTo Expr)
-- instance Subst Expr (ModedName Expr)
-- instance Subst Expr Mode
-- -- instance Subst Expr LocVar
-- instance Subst (Exists Expr) Expr
-- instance Subst (Pattern Expr) Expr
--
-- -- instance Subst (Name Expr) Expr
-- -- instance Subst (Name Expr) AdtName
--
-- instance Subst Expr (LayoutBranch Expr)
-- instance Subst Expr (PatternMatch Expr (Bind [Exists Expr] (LayoutBody Expr)))
-- instance Subst Expr (PatternMatch Expr (Bind [Exists Expr] (GhostCondition Expr (LayoutBody Expr))))
-- instance Subst Expr (Pattern Expr)
--
-- instance Subst [ModedName Expr] (PatternMatch Expr (Bind [Exists Expr] (GhostCondition Expr (LayoutBody Expr))))
-- instance Subst [ModedName Expr] (GhostCondition Expr (LayoutBody Expr))
-- instance Subst [ModedName Expr] (LayoutBody Expr)
-- instance Subst [ModedName Expr] (LayoutHeaplet Expr)
-- instance Subst [ModedName Expr] (PointsTo Expr)
-- instance Subst [ModedName Expr] (Loc Expr)
-- -- instance Subst [ModedName Expr] Expr
-- -- instance Subst [ModedName Expr] AdtName
-- instance Subst [ModedName Expr] (Exists Expr)
-- instance Subst [ModedName Expr] (Moded' PC (Name Expr))
-- instance Subst [ModedName Expr] Mode
-- instance Subst [ModedName Expr] (Pattern Expr)
--
-- -- instance Subst (Moded Expr) Expr
-- -- instance Subst (Moded Expr) AdtName
-- instance Subst Expr (Layout Expr)
-- instance Subst Expr (Ghost Expr)
-- instance Subst Expr GhostType

instance Subst Type Expr where

instance (Alpha a, Subst Expr a) => Subst Expr (TypeSig' a)
instance Subst Expr ConstrainedType
instance Subst Expr LayoutConstraint


makePrisms ''Expr
makeLenses ''Test


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
isConcrete (Lambda bnd) =
  let (_, e) = unsafeUnbind bnd
  in
  isConcrete e
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
    go (ApplyLayout e (LayoutId arg)) =
      case go e of
        (LayoutLambda _ (B p e)) ->
          rename [(p, string2Name arg)] e
          -- substBind bnd arg
        e' -> ApplyLayout e' (LayoutId arg)
    go (Lambda bnd) =
      let (vs, body) = unsafeUnbind bnd
      in
      Lambda $ bind vs $ go body
    go (App f args) =
      App f (map go args)
    go (Mod x y) = Mod (go x) (go y)
    go (And x y) = And (go x) (go y)
    go (IfThenElse x y z) = IfThenElse (go x) (go y) (go z)
    go (Add x y) = Add (go x) (go y)
    go (Sub x y) = Sub (go x) (go y)
    go (Not x) = Not (go x)
    go (SingletonSet x) = Not (go x)
    go EmptySet = EmptySet
    go (SetUnion x y) = SetUnion (go x) (go y)
    go (Equal x y) = Equal (go x) (go y)
    go (Lt x y) = Lt (go x) (go y)
    go (Le x y) = Le (go x) (go y)

--
-- Property tests --
--

-- layoutTest :: Layout Expr
-- layoutTest =
--   Layout
--   { _layoutName = "Test"
--   , _layoutAdt = AdtName "A"
--   , _layoutBranches =
--       let x = string2Name "x"
--           n = string2Name "n"
--           nxt = string2Name "nxt"
--           z = string2Name "z"
--       in
--       bind [Moded Out x]
--         [LayoutBranch
--           $ PatternMatch
--               $ bind (Pattern "C" [n])
--                   $ bind [Exists $ Moded In nxt]
--                     $ LayoutBody
--                         [LPointsTo ((V x :+ 0) :-> V n)
--                         ,LPointsTo ((V x :+ 1) :-> V nxt)
--                         ,LPointsTo ((V x :+ 2) :-> V z)
--                         ]
--         ]
--   }

instance WellScoped (Name Expr) Type
instance WellScoped (Name Expr) (Bind (TypeName, Embed AdtName) Type)
instance WellScoped (Name Expr) (Embed AdtName)
instance WellScoped (Name Expr) ConstrainedType
instance WellScoped (Name Expr) LayoutConstraint
instance WellScoped (Name Expr) Expr
-- TODO: Figure out a way to handle this case properly
instance WellScoped (Name Expr) (Bind TypeName Expr) where
  wellScoped inScopeVars (B v body) =
    wellScoped inScopeVars body
instance WellScoped (Name Expr) (Name Type) where
  wellScoped _ _ = mempty

instance Arbitrary Expr where
  arbitrary = error "Arbitrary Expr"
  shrink (App f xs) = do
    xs' <- sequenceA $ map shrink xs
    pure $ App f xs'
  shrink (ApplyLayout e layoutName) =
    ApplyLayout <$> shrink e <*> pure layoutName
  shrink e0 = genericShrink e0

-- -- | Only applies to @App@ constructors
-- renameFnName :: (ExprName -> ExprName) -> Expr -> Expr
-- renameFnName rho = transform go
--   where
--     go (App fnName args) = App (rho fnName) args
--     go e = e

getFnNames :: Expr -> [ExprName]
getFnNames = map getV . toListOf (_App._1)
  where
    getV (V x) = x

-- isValidExpr :: Expr -> Validation
-- isValidExpr = mconcat . map go . universe
--   where
--     go (App f 

isConcreteExpr :: Expr -> Validation
isConcreteExpr e0 = mconcat (map go (universe e0))
  where
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
  mkApp (string2Name fn) <$> mapM (genForMaybeLayout fnSigs layouts locals newSize) inLayouts

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
    <$> (mkApp (string2Name cName) <$> mapM (genForMaybeLayout fnSigs layouts locals newSize) arity)
    <*> pure (LayoutId layout)

-- TODO: Right now assumes the lambda is fully applied
betaReduce :: Expr -> Maybe Expr
betaReduce (App (Lambda bnd) args) =
  Just $ instantiate bnd args
betaReduce _ = Nothing

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

