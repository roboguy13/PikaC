module PikaC.Syntax.PikaCore.Expr
  where

import PikaC.Syntax.Heaplet
import PikaC.Ppr
import Data.List

data Base a
  -- = V a
  = LayoutV (LayoutArg a)    -- {x ...}
  | IntLit Int -- TODO: Add output locations?
  | BoolLit Bool

  | Add (Base a) (Base a)
  | Sub (Base a) (Base a)
  | Equal (Base a) (Base a)
  | Not (Base a)
  | And (Base a) (Base a)
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
  -- | App (Expr a) (LayoutArg a)
  | App String [LayoutArg a] -- | Fully saturated function application
  deriving (Show)

type PointsToExpr a = PointsTo Base a
type ExprAssertion a = [PointsToExpr a]

instance LayoutRename Base where
  -- renameLayoutArg old new (V x) = V x
  renameLayoutArg old new (LayoutV xs) =
    LayoutV $ renameLayoutArg old new xs
  renameLayoutArg old new (IntLit i) = IntLit i
  renameLayoutArg old new (BoolLit b) = BoolLit b
  renameLayoutArg old new (Add x y) =
    Add (renameLayoutArg old new x)
        (renameLayoutArg old new y)
  renameLayoutArg old new (Sub x y) =
    Sub (renameLayoutArg old new x)
        (renameLayoutArg old new y)
  renameLayoutArg old new (Equal x y) =
    Equal (renameLayoutArg old new x)
          (renameLayoutArg old new y)
  renameLayoutArg old new (Not x) =
    Not (renameLayoutArg old new x)
  renameLayoutArg old new (And x y) =
    And (renameLayoutArg old new x)
        (renameLayoutArg old new y)

instance LayoutRename SimpleExpr where
  renameLayoutArg old new (BaseExpr e) =
    BaseExpr $ renameLayoutArg old new e

  renameLayoutArg old0@(LayoutArg old) new0@(LayoutArg new) (WithIn vars0@(LayoutArg vars) bnd body) =
    WithIn
      vars0
      (renameLayoutArg old0 new0 bnd)
      (renameLayoutArg (LayoutArg old') (LayoutArg new') body)
    where
      assocs = filter ((`notElem` vars) . fst) $ zip old new
      (old', new') = unzip assocs

instance LayoutRename Expr where
  renameLayoutArg old new (SimpleExpr e) =
    SimpleExpr $ renameLayoutArg old new e

  renameLayoutArg old new (App f x) =
    App f (map (renameLayoutArg old new) x)

instance Ppr a => Ppr (LayoutArg a) where
  ppr (LayoutArg xs) = text "{" <+> hsep (punctuate (text ",") (map ppr xs)) <+> text "}"

instance Ppr a => Ppr (Base a) where
  -- ppr (V x) = ppr x
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
    sep [text "with", hsep [ppr vars, text ":=", ppr bnd], text "in", ppr body]

  ppr (SslAssertion vars heaplets) =
    sep [text "layout", ppr vars, ppr heaplets]

instance Ppr a => Ppr (Expr a) where
  ppr (SimpleExpr e) = ppr e
  ppr (App f x) = ppr f <+> hsep (map ppr x)

instance IsNested (Expr a) where
  isNested (SimpleExpr e) = isNested e
  isNested (App {}) = True

instance IsNested (SimpleExpr a) where
  isNested (BaseExpr e) = isNested e
  isNested (WithIn {}) = True
  isNested (SslAssertion {}) = True

instance IsNested (Base a) where
  -- isNested (V _) = False
  isNested (LayoutV _) = False
  isNested (IntLit _) = False
  isNested (BoolLit _) = False

  isNested (Add {}) = True
  isNested (Sub {}) = True
  isNested (Equal {}) = True
  isNested (Not {}) = True
  isNested (And {}) = True

