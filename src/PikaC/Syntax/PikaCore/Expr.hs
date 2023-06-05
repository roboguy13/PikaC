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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

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
import PikaC.Syntax.Type

import PikaC.Stage

import Control.Lens hiding (elements)
import Control.Lens.Extras
import Control.Lens.Action

import GHC.Generics hiding (to)
import GHC.Stack

import Control.Monad

import Data.Validity
import Data.GenValidity
import Test.QuickCheck

import Unbound.Generics.LocallyNameless.Unsafe -- Just for implementing QuickCheck shrinking

import Data.Char
import Data.String
import Data.Typeable

type ExprName = Name Expr

-- type LocName = ExprName

data Expr' (s :: Stage)
  = V (Name (Expr' s))
  | LayoutV [Expr' s]    -- {x ...}
  -- | LayoutV [ExprName]    -- {x ...}
  | IntLit Int -- TODO: Add output locations?
  | BoolLit Bool

  | Add (Expr' s) (Expr' s)
  | Sub (Expr' s) (Expr' s)
  | Equal (Expr' s) (Expr' s)
  | Not (Expr' s)
  | And (Expr' s) (Expr' s)
  -- | App (Expr a) (LayoutArg a)
  | WithIn                -- with
      (Expr' s)
      -- [Allocation Expr]
      -- [Int] -- Allocation sizes for the names
      (Bind [ModedName' s (Expr' s)]
        (Expr' s))
      -- Expr                --     := e
      -- [ExprName]     --   {x ...}
      -- Expr       -- in e

  | SslAssertion            -- layout
      (Bind [ModedName' s (Expr' s)]
         (ExprAssertion' s))

      -- (LayoutArg Expr)     --     {x ...}
      -- ExprAssertion     --   { (x+1) :-> e ** ... }

  | App -- | Fully saturated function application
      FnName
      [Int] -- | Allocation size for results
      [Expr' s]
  deriving (Generic)

type family XV (s :: Stage)

type instance XV PC = ()

type LiftClassExpr' c s =
  (c (XV s))

-- pattern V x = V' () x

deriving instance (Show (XModed s), Show (XV s)) => Show (Expr' s)

-- pattern LayoutV xs = LayoutV' () xs

type Expr = Expr' PC

type FnName = FnName' String
newtype FnName' a = FnName a
  deriving (Show, Eq, Ord, Generic)

instance IsString FnName where
  fromString = FnName

instance Subst a b => Subst a (FnName' b)

instance Alpha FnName
instance Ppr FnName where ppr (FnName f) = text f

fnNameIsOk :: FnName -> Bool
fnNameIsOk (FnName str) =
  all (`elem` ['a'..'z']) str && not (null str)

instance HasVar (Expr' s) where mkVar = V

instance IsBase Expr where
  isVar (V _) = True
  isVar _ = False

  isLit (IntLit _) = True
  isLit (BoolLit _) = True
  isLit _ = False

  intLit = IntLit
  boolLit = BoolLit
  mkNot = Not
  mkEqual = Equal
  mkAnd = And

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
  plate f (App x sz ys) = App x sz <$> traverse f ys

type PointsToExpr = PointsTo Expr
type ExprAssertion = [PointsToExpr]
type ExprAssertion' s = [PointsTo (Expr' s)]

makePrisms ''Expr'

bindXV :: forall s. (Show (Expr' s), XV s ~ XModed s, Alpha (XModed s), Typeable s, Alpha (Expr' s)) =>
  (Expr -> ModedName Expr -> ModedName' s (Expr' s)) ->
  (ExprAssertion -> ModedName Expr -> ModedName' s (Expr' s)) ->
  [ModedName' s (Expr' s)] ->
  Expr -> Expr' s
bindXV convertNameExpr convertNameAsn vars = go
  where
    -- lookupV :: ExprName -> ModedName' s (Expr' s)
    -- lookupV v = case find ((== v) . string2Name . show . modedNameName) vars of
    --   Nothing -> --error $ "bindXV: Cannot find " ++ show v
    --     Moded' 0 _ v
    --   Just r -> r
    --
    -- toV' :: ExprName -> Expr' s
    -- toV' x = case lookupV x of
    --   r@(Moded' ann _ x') -> V (modedNameName r)
    --   -- Moded' ann _ x' -> V' ann x

    go' = bindXV convertNameExpr convertNameAsn

    go :: Expr -> Expr' s
    go (V x) = V (string2Name (show x)) --toV' x
    go (LayoutV xs) = LayoutV $ map go xs
    go (IntLit i) = IntLit i
    go (BoolLit b) = BoolLit b
    go (Add x y) = Add (go x) (go y)
    go (Sub x y) = Sub (go x) (go y)
    go (Equal x y) = Equal (go x) (go y)
    go (Not x) = Not (go x)
    go (And x y) = And (go x) (go y)
    go (App f sz args) =
      App f sz (map go args)
    go (WithIn e bnd) =
      let -- TODO: Is this a safe use of unsafeUnbind?
          (newVars, body) = unsafeUnbind bnd
          newVars' = map (convertNameExpr body) newVars
      in
      WithIn (go e)
      $ bind newVars'
        $ go' (newVars' ++ vars) body
    go (SslAssertion bnd) =
      let (newVars, asn) = unsafeUnbind bnd
          newVars' = map (convertNameAsn asn) newVars
      in
      SslAssertion
        $ bind newVars'
          $ map (mapPointsTo (go' (newVars' ++ vars))) asn


instance HasApp Expr where
  mkApp x y = App (FnName x) [] y

-- example :: SimpleExpr
-- example =
--   WithIn
--     (SimpleExpr (BaseExpr (IntLit 1)))
--     exampleBind

-- exampleBind :: Bind [LocName] SimpleExpr
-- exampleBind =
--   bind [string2Name "x"]
--     (BaseExpr (LocV (string2Name "x")))

instance (Typeable s, Alpha (XV s), Alpha (XModed s)) => Alpha (Expr' s)

instance Subst Expr (LayoutBranch Expr)
instance Subst Expr (PatternMatch Expr (Bind [Exists Expr] (LayoutBody Expr)))
instance Subst Expr (Pattern Expr)


-- instance Subst Expr LocVar

-- instance Subst ExprName (PointsTo Expr) where

instance Subst (Name Expr) a => Subst ExprName (Loc a) where

instance Subst ExprName Expr

instance (Alpha (XV s), Alpha (XModed s), Typeable s, Subst (Expr' s) (XModed s)) => IsName (Expr' s) (Expr' s) where
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

instance (Alpha (XV s), Subst (Moded' s (Expr' s)) (XModed s), Subst (Moded' s (Name (Expr' s))) (Moded' s (Name (Expr' s))), Alpha (XV s), Alpha (XModed s), Typeable s) => Subst (Moded' s (Expr' s)) (Expr' s)
instance (Alpha (XV s), Alpha (XModed s), Typeable s, Subst (Moded' s (Name (Expr' s))) (Moded' s (Name (Expr' s))), Subst (Moded' s (Expr' s)) (XModed s)) => Subst (Moded' s (Expr' s)) (Moded' s (Name (Expr' s)))
instance (Alpha (XV s), Alpha (XModed s), Typeable s, Subst (Moded' s (Name (Expr' s))) (Moded' s (Name (Expr' s)))) =>Subst (Moded' s (Expr' s)) Mode
instance (Alpha (XV s), Alpha (XModed s), Typeable s, Subst (Moded' s (Name (Expr' s))) (Moded' s (Name (Expr' s)))) =>Subst (Moded' s (Name (Expr' s))) (LayoutBody (Expr' s))
instance (Alpha (XV s), Alpha (XModed s), Typeable s, Subst (Moded' s (Name (Expr' s))) (Moded' s (Name (Expr' s)))) =>Subst (Moded' s (Name (Expr' s))) (LayoutHeaplet (Expr' s))
instance (Alpha (XV s), Alpha (XModed s), Typeable s, Subst (Moded' s (Name (Expr' s))) (Moded' s (Name (Expr' s)))) =>Subst (Moded' s (Name (Expr' s))) (PointsTo (Expr' s))
-- instance (Alpha (XV s), Subst ((Moded' s) (Expr' s)) (XModed s), Alpha (XModed s), Typeable s, Subst (Moded' s (Name (Expr' s))) (Moded' s (Name (Expr' s)))) =>Subst (Moded' s (Expr' s)) (PointsTo (Expr' s))
-- instance (Alpha (XV s), Subst (Moded' s (Expr' s)) (XModed s), Alpha (XModed s), Typeable s, Subst (Moded' s (Name (Expr' s))) (Moded' s (Name (Expr' s)))) =>Subst (Moded' s (Expr' s)) (Loc (Expr' s))
instance (Alpha (XV s), Alpha (XModed s), Typeable s, Subst (Moded' s (Name (Expr' s))) (Moded' s (Name (Expr' s)))) =>
      Subst (Moded' s (Name (Expr' s))) (Loc (Expr' s))
instance (Alpha (XV s), Alpha (XModed s), Typeable s, Subst (Moded' s (Name (Expr' s))) (Moded' s (Name (Expr' s)))) =>
  Subst (Moded' s (Name (Expr' s))) (Expr' s)
-- instance Subst (Moded' s (Name (Expr' s))) Expr
instance Subst (Moded' s (Name (Expr' s))) (Moded' PC (Name (Expr' s)))
instance Subst (Moded' s (Name (Expr' s))) Mode
instance Subst (Moded' s (Name (Expr' s))) (Exists (Expr' s))

instance (Alpha (XModed s), Alpha (XV s), Subst (Expr' s) (Moded' s (Name (Expr' s))), Typeable s) => Subst (Expr' s) (Expr' s) where
  isvar (V x) = Just $ SubstName x
  isvar _ = Nothing

instance (Subst (Expr' s) (XModed s)) => Subst (Expr' s) (ModedName' s a)
instance Subst (Expr' s) Mode
-- instance Subst ExprName (ModedName a)
-- instance Subst ExprName Mode

instance Subst (Expr' s) a => Subst (Expr' s) (PointsTo a) where
  isCoerceVar (x :-> y) = do
    SubstCoerce p q <- isCoerceVar @(Expr' s) @(Loc a) x
    pure (SubstCoerce p (fmap (:-> y) . q))

instance forall s a. Subst (Expr' s) a => Subst (Expr' s) (Loc a) where
  isCoerceVar (x :+ i) = do
    SubstCoerce p q <- isCoerceVar @(Expr' s) @_ x
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
instance Subst Expr (Layout Expr)
instance Subst Expr AdtName
instance Subst (Exists Expr) (Allocation Expr)
instance Subst (Moded (Name Expr)) (Allocation Expr)
instance Subst (Moded Expr) (Allocation Expr)
instance Subst (Name Expr) (Allocation Expr)

instance (Subst (Expr' s) (XModed s), Typeable s, Show (XV s), Alpha (XModed s), Alpha (XV s)) => Ppr (Expr' s) where
  ppr = runFreshM . pprExpr

pprExpr :: forall s. (Subst (Expr' s) (XModed s), Typeable s, Show (XV s), Alpha (XModed s), Alpha (XV s)) =>
  Expr' s -> FreshM Doc
pprExpr (WithIn e bnd) = do
  -- (vars, body0) <- unbind bnd
  -- let body = instantiate bnd vars
  (vars, body) <- freshOpen @_ @Name bnd
  bodyDoc <- pprExpr body
  eDoc <- pprExpr e
  pure $ sep [hsep ([text "with {", hsep . punctuate (text ",") $ map go vars, text "} :=", eDoc]), text "in", bodyDoc]
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
  xDoc <- pprExprP x
  yDoc <- pprExprP y
  pure $ sep [xDoc, text "+", yDoc]
pprExpr (Sub x y) = do
  xDoc <- pprExprP x
  yDoc <- pprExprP y
  pure $ sep [xDoc, text "-", yDoc]
pprExpr (Equal x y) = do
  xDoc <- pprExpr x
  yDoc <- pprExpr y
  pure $ sep [xDoc, text "==", yDoc]
pprExpr (Not x) = do
  xDoc <- pprExprP x
  pure $ sep [text "!", xDoc]
pprExpr (And x y) = do
  xDoc <- pprExprP x
  yDoc <- pprExprP y
  pure $ sep [xDoc, text "&&", yDoc]
pprExpr (App f sz x) = do
  xDoc <- mapM pprExprP x
  pure $ ppr f <+> (text "[" <> text (show sz) <> text "]") <+> hsep xDoc

pprExprP :: (Subst (Expr' s) (XModed s), Typeable s, Show (XV s), Alpha (XModed s), Alpha (XV s)) => Expr' s -> FreshM Doc
pprExprP e
  | isNested e = parens <$> pprExpr e
  | otherwise = pprExpr e

instance IsNested (Expr' s) where
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

isBasic :: Expr' s -> Bool
isBasic (V x) = True
isBasic (LayoutV _) = True
isBasic (IntLit i) = True
isBasic (BoolLit b) = True
isBasic (Add x y) = True
isBasic (Sub x y) = True
isBasic (Equal x y) = True
isBasic (Not x) = True
isBasic (And x y) = True
isBasic _ = False

getV :: (HasCallStack, Ppr (Expr' s)) => Expr' s -> Name (Expr' s)
getV (V x) = x
getV e = error $ "getV: " ++ ppr' e

modedExpr :: ModedName Expr -> Moded Expr
modedExpr (Moded m v) = Moded m (V v)

-- Property testing --

instance Arbitrary FnName where
  arbitrary = (FnName <$> arbitrary) `suchThat` fnNameIsOk
  shrink = filter fnNameIsOk . genericShrink

instance Arbitrary Expr where
  arbitrary = genValidExpr [] -- NOTE: Only generates *closed* expressions
  shrink = filter isValid . genericShrink

-- shrinkExpr' :: Expr -> [Expr]
-- shrinkExpr' = genericShrink

-- shrinkExpr :: Expr -> [Expr]
-- -- shrinkExpr (V n) = V <$> shrinkName n
-- shrinkExpr (V n) = [] --pure $ V n
-- shrinkExpr (LayoutV ns) = [] --pure $ LayoutV ns
-- shrinkExpr (IntLit i) = IntLit <$> shrink i
-- shrinkExpr (BoolLit b) = BoolLit <$> shrink b
-- shrinkExpr (Add x y) = [x, y] ++ (Add <$> shrinkExpr x <*> shrinkExpr y)
-- shrinkExpr (Sub x y) = [x, y] ++ (Sub <$> shrinkExpr x <*> shrinkExpr y)
-- shrinkExpr (Equal x y) = [x, y] ++ (Equal <$> shrinkExpr x <*> shrinkExpr y)
-- shrinkExpr (Not x) = [x] ++ (Not <$> shrinkExpr x)
-- shrinkExpr (And x y) = [x, y] ++ (And <$> shrinkExpr x <*> shrinkExpr y)
-- shrinkExpr (WithIn e bnd) =
--   let (vars, body) = unsafeUnbind bnd
--   in
--   [e] ++
--   -- [instantiate bnd (replicate (length vars) (IntLit 2))] ++
--   (WithIn <$> shrinkExpr e <*> (bind vars <$> shrinkExpr body))
-- shrinkExpr (SslAssertion bnd) =
--   let (vars, asn) = unsafeUnbind bnd
--   in
--   SslAssertion <$> (bind vars <$> shrinkAssertion shrinkExpr asn)
-- shrinkExpr (App f args) =
--   args ++
--   (App <$> shrink f <*> mapM shrinkExpr args)

instance WellScoped ExprName Expr
instance WellScoped ExprName ()
instance WellScoped ExprName (FnName' String)

instance Validity Expr where
  validate = exprIsOk

exprIsOk :: Expr -> Validation
exprIsOk = mconcat . map go . universe
  where
    go (SslAssertion bnd) =
      let (vars, body) = unsafeUnbind bnd
      in
      asnIsOk vars body
    go (App _ _ []) = invalid "Application must have at least one argument"
    go (App _ [] _) = invalid "Application must have at least one output size"
    go (App _ szs _)
      | not (all (> 0) szs) = invalid "All sizes in application must by positive"
    -- go (SslAssertion (B _ [])) = invalid "Empty assertion"
    go (WithIn _ (B [] _)) = invalid "with-in with empty variable list"
    go (WithIn e (B vs' _)) =
      check (length vs' == getOutputCount e)
        "When a layout { ... } { ... } is bound by a with-in, the with-in must have the right number of variables"
    go _ = mempty

asnIsOk :: [Moded ExprName] -> ExprAssertion -> Validation
asnIsOk _ = mconcat . map go
  where
    go (x :-> App _ sz _) =
      check (length sz == 1)
        "An application in the RHS of a points-to must have one output"
    go _ = mempty

getOutputCount :: Expr -> Int
getOutputCount (V {}) = 1
getOutputCount (LayoutV xs) = length xs
getOutputCount (IntLit {}) = 1
getOutputCount (BoolLit {}) = 1
getOutputCount (Add {}) = 1
getOutputCount (Sub {}) = 1
getOutputCount (Equal {}) = 1
getOutputCount (And {}) = 1
getOutputCount (Not {}) = 1
getOutputCount (WithIn _ bnd) =
  let (_, body) = unsafeUnbind bnd
  in
  getOutputCount body
getOutputCount (SslAssertion bnd@(B [] _)) =
  let (_, asns) = unsafeUnbind bnd
  in
  length $ fastNub $ map (getV . locBase . pointsToLhs) asns
getOutputCount (SslAssertion (B vs _)) = length vs
getOutputCount (App _ xs _) = length xs

-- getAsnParamCount :: Bind [ModedName Expr] ExprAssertion -> Int
-- getAsnParamCount bnd@(B [] _) =
--   let (_, asns) = unsafeUnbind bnd
--   in
--   length $ fastNub $ map (getV . locBase . pointsToLhs) asns
-- getAsnParamCount (B vs _) = length vs

exprBasicArgs :: Expr -> Validation
exprBasicArgs e0 = validate e0 <> mconcat (map go (universe e0))
  where
    go (App _ _ xs) =
      check (all (not . is _App) xs) "No nested applications"
      -- <> mconcat (map goWithIn (filter (is _WithIn) xs))
    go _ = mempty

    -- goWithIn (WithIn b (B _ e)) =
    --   check (isSimpleArg b) "All with-ins that appear as arguments must only bind basic expressions"
    --   <> check (isSimpleArg e) "All with-ins that appear as arguments must have basic bodies"
    --   -- check (not (is _App e)) "No applications in with-in that are themselves arguments to applications"
    -- goWithIn _ = mempty

exprIsSimplified :: Expr -> Validation
exprIsSimplified e0@(App (FnName f) _ _) | isConstructor f =
  decorate (ppr' e0) $
  invalid "Top-level constructor application"
exprIsSimplified e0@(App (FnName f) _ args) =
  decorate (ppr' e0) $
  check (all isBasic args)
    "Function application not bound by with-in has all basic expressions for arguments"
exprIsSimplified e0 = exprIsOk e0 <> mconcat (map go (universe e0))
  where
    go (App _ _ xs) =
      check (all isSimpleArg xs) "Function should be applied to base expressions"
    go (WithIn (WithIn _ _) _) = invalid "Nested with-ins"
    go (SslAssertion (B _ xs)) =
      decorate "in assertion" $ mconcat $ map (validateWithRhs (flip check "All right-hand sides of points-tos should be basic expressions" . isBasic)) xs
    go _ = mempty

isSimpleArg :: Expr -> Bool
isSimpleArg (LayoutV {}) = True
isSimpleArg e = isBasic e

genValidExpr :: [Name Expr] -> Gen Expr
genValidExpr bvs = sized (genValidExpr' bvs)

instance HasVars (Expr' s) (Expr' s) where
  mkVars [v] = V v
  mkVars vs = LayoutV $ map V vs

-- No SslAssertion's or WithIn's
genSimpleExpr :: [Name Expr] -> Int -> Gen Expr
genSimpleExpr [] 0 =
  oneof
    [IntLit <$> arbitrary
    ,BoolLit <$> arbitrary
    ]
genSimpleExpr bvs 0 =
  oneof
    [V <$> elements bvs
    ,genSimpleExpr [] 0
    ]
genSimpleExpr bvs size =
  oneof
    [genSimpleExpr bvs 0
    ,Add <$> halvedGen <*> halvedGen
    ,Sub <$> halvedGen <*> halvedGen
    ,Equal <$> halvedGen <*> halvedGen
    ,Not <$> genSimpleExpr bvs (size - 1)
    ,And <$> halvedGen <*> halvedGen
    ]
  where
    halvedGen = halvedGenWith bvs
    halvedGenWith bvs' = genSimpleExpr bvs' (size `div` 2)

genValidExpr' :: [Name Expr] -> Int -> Gen Expr
genValidExpr' [] 0 =
  oneof
    [IntLit <$> arbitrary
    ,BoolLit <$> arbitrary
    ]
genValidExpr' bvs 0 =
  oneof
    [V <$> elements bvs
    ,genValidExpr' [] 0
    ]

genValidExpr' bvs size =
  oneof
    [genValidExpr' bvs 0
    ,Add <$> halvedGen <*> halvedGen
    ,Sub <$> halvedGen <*> halvedGen
    ,Equal <$> halvedGen <*> halvedGen
    ,Not <$> genValidExpr' bvs (size - 1)
    ,And <$> halvedGen <*> halvedGen
    ,let n' = newName bvs
     in
     WithIn <$> halvedGen
      <*> (bind [Moded Out n']
            <$> halvedGenWith (n' : bvs))

    ,SslAssertion
      <$> (bind [] <$> genValidAssertion bvs (genSimpleExpr bvs) (size - 1))

    ,do
      isBinary <- arbitrary :: Gen Bool
      sz <- choose (1, 3)
      if isBinary
        then do
          x <- halvedGen
          y <- halvedGen
          App <$> fmap FnName (replicateM 3 arbitraryAlpha) <*> pure [sz] <*> pure [x, y]
        else do
          x <- genValidExpr' bvs (size - 1)
          App <$> fmap FnName (replicateM 3 arbitraryAlpha) <*> pure [sz] <*> pure [x]
    ]
  where
    halvedGen = halvedGenWith bvs
    halvedGenWith bvs' = genValidExpr' bvs' (size `div` 2)

newName :: [Name Expr] -> Name Expr
newName [] = string2Name "a"
newName ns = last . runFreshM $ mapM fresh ns

