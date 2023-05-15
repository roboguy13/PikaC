module PikaC.Backend.C.Syntax
  where

import PikaC.Syntax.Heaplet
import PikaC.Ppr

data CExpr a
  = V (Loc a)
  | IntLit Int
  | BoolLit Bool
  | Add (CExpr a) (CExpr a)
  | Sub (CExpr a) (CExpr a)
  | Equal (CExpr a) (CExpr a)
  | And (CExpr a) (CExpr a)
  | Not (CExpr a)
  | Deref (Loc a)
  deriving (Show, Eq, Ord)

data Command a
  = Assign (Loc a) (CExpr a)
  | IfThenElse (CExpr a) [Command a] [Command a]
  | Call
      String
      [CExpr a] -- Input parameters
      [CExpr a] -- Output parameters
  | IntoMalloc a Int
  | Let a (Loc a)
  | Free a
  | Nop
  deriving (Show, Eq, Ord)

data CFunction a =
  CFunction
    { cfunctionName :: String
    , cfunctionParams :: [a]
    , cfunctionBody :: [Command a]
    }

intoMalloc :: String -> Int -> Command String
intoMalloc v = IntoMalloc ("_local_" <> v)

instance LayoutRename CExpr where
  renameLayoutArg old new (V x) = V $ renameLayoutArg old new x
  renameLayoutArg old new (IntLit i) = IntLit i
  renameLayoutArg old new (BoolLit b) = BoolLit b
  renameLayoutArg old new (Add x y) = Add (renameLayoutArg old new x) (renameLayoutArg old new y)
  renameLayoutArg old new (Sub x y) = Sub (renameLayoutArg old new x) (renameLayoutArg old new y)
  renameLayoutArg old new (Equal x y) = Equal (renameLayoutArg old new x) (renameLayoutArg old new y)
  renameLayoutArg old new (And x y) = And (renameLayoutArg old new x) (renameLayoutArg old new y)
  renameLayoutArg old new (Not x) = Not (renameLayoutArg old new x)
  renameLayoutArg old new (Deref x) = Deref $ renameLayoutArg old new x

instance LayoutRename Command where
  renameLayoutArg old new (Assign lhs rhs) =
    Assign (renameLayoutArg old new lhs) (renameLayoutArg old new rhs)
  renameLayoutArg old new (IfThenElse c t f) =
    IfThenElse (renameLayoutArg old new c)
      (map (renameLayoutArg old new) t)
      (map (renameLayoutArg old new) f)
  renameLayoutArg old new (IntoMalloc a sz) = IntoMalloc a sz
  renameLayoutArg old new (Free x) = Free $ renameLayoutArg' old new x

instance HasLocs CExpr where
  getLocs (V x) = [x]
  getLocs (IntLit i) = []
  getLocs (BoolLit b) = []
  getLocs (Add x y) = getLocs x <> getLocs y
  getLocs (Sub x y) = getLocs x <> getLocs y
  getLocs (Equal x y) = getLocs x <> getLocs y
  getLocs (Not x) = getLocs x
  getLocs (And x y) = getLocs x <> getLocs y
  getLocs (Deref x) = [x]

instance Ppr a => Ppr (Command a) where
  ppr (Assign loc e) =
    hsep [text "*" <> ppr loc, text "=", ppr e] <> text ";"

  ppr (IfThenElse c t f) =
    foldr1 ($$) [hsep [text "if (" <> ppr c <> text ")", text "{"]
        ,nest 1 (sep (map ppr t))
        ,hsep [text "}", text "else", text "{"]
        ,nest 1 (hsep (map ppr f))
        ,text "}"
        ]

  ppr (Call f inArgs outArgs) =
    let args = inArgs <> outArgs
    in
    text f <> text "(" <> hsep (punctuate (text ",") (map ppr args)) <> text ");"

  ppr (IntoMalloc target size) =
    hsep [ppr target, text "=", text "malloc(", ppr size, text "*sizeof(loc));"]

  ppr (Free x) =
    hsep [text "free(", ppr x, text ");"]

  ppr (Let x y) = ("loc" <+> ppr x <+> "=" <+> ppr y) <> ";"
  ppr Nop = ";"

instance Ppr a => Ppr (CFunction a) where
  ppr fn =
    hsep [text "void", text (cfunctionName fn) <> text "(" <> hsep (punctuate (text ",") (map ((text "loc" <+>) . ppr) (cfunctionParams fn))) <> text ")", text "{"]
      $$
       nest 1 (vcat (map ppr (cfunctionBody fn)))
      $$
       text "}"

instance Ppr a => Ppr (CExpr a) where
  ppr (V x) = ppr x
  ppr (IntLit i) = ppr i
  ppr (BoolLit True) = "true"
  ppr (BoolLit False) = "false"
  ppr (Add x y) = sep [pprP x, text "+", pprP y]
  ppr (Sub x y) = sep [pprP x, text "-", pprP y]
  ppr (Equal x y) = sep [pprP x, text "==", pprP y]
  ppr (Not x) = sep [text "!", pprP x]
  ppr (And x y) = sep [pprP x, text "&&", pprP y]
  ppr (Deref x) = "*" <> ppr x

instance IsNested (CExpr a) where
  isNested (V _) = False
  isNested (IntLit _) = False
  isNested (BoolLit _) = False

  isNested (Add {}) = True
  isNested (Sub {}) = True
  isNested (Equal {}) = True
  isNested (Not {}) = True
  isNested (And {}) = True
  isNested (Deref {}) = False

