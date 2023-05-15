module PikaC.Backend.C.Syntax
  where

import PikaC.Syntax.Heaplet
import PikaC.Ppr

data CExpr a
  = V a
  | BoolLit Bool
  | IntLit Int
  | Add (CExpr a) (CExpr a)
  | Sub (CExpr a) (CExpr a)
  | Equal (CExpr a) (CExpr a)
  | And (CExpr a) (CExpr a)
  | Not (CExpr a)
  | Deref (Loc a)

data Command a
  = Assign (Loc a) (CExpr a)
  | IfThenElse (CExpr a) [Command a] [Command a]
  | Call String [CExpr a]
  | IntoMalloc a Int
  | Free a

data CFunction a =
  CFunction
    { cfunctionName :: String
    , cfunctionParams :: [a]
    , cfunctionBody :: [Command a]
    }

instance Ppr a => Ppr (Command a) where
  ppr (Assign loc e) =
    hsep [text "*", ppr loc, text "=", ppr e] <> text ";"

  ppr (IfThenElse c t f) =
    sep [hsep [text "if", text "(", ppr c, text ")", text "{"]
        ,nest 1 (hsep (map ppr t))
        ,hsep [text "}", text "else", text "{"]
        ,nest 1 (hsep (map ppr f))
        ,text "}"
        ]

  ppr (Call f args) =
    hsep [text f, text "(", hsep (punctuate (text ",") (map ppr args)), text ")", text ";"]

  ppr (IntoMalloc target size) =
    hsep [ppr target, text "=", text "malloc(", ppr size, text "*sizeof(loc));"]

  ppr (Free x) =
    hsep [text "free(", ppr x, text ");"]

instance Ppr a => Ppr (CFunction a) where
  ppr fn =
    vcat [text "void", text (cfunctionName fn), text "(", hsep (punctuate (text ",") (map ((text "loc" <+>) . ppr) (cfunctionParams fn))), text ")", text "{"
         ,vcat (map ppr (cfunctionBody fn))
         ,text "}"
         ]

instance Ppr a => Ppr (CExpr a) where
  ppr (V x) = ppr x
  ppr (IntLit i) = ppr i
  ppr (BoolLit b) = ppr b
  ppr (Add x y) = sep [pprP x, text "+", pprP y]
  ppr (Sub x y) = sep [pprP x, text "-", pprP y]
  ppr (Equal x y) = sep [pprP x, text "==", pprP y]
  ppr (Not x) = sep [text "!", pprP x]
  ppr (And x y) = sep [pprP x, text "&&", pprP y]

instance IsNested (CExpr a) where
  isNested (V _) = False
  isNested (IntLit _) = False
  isNested (BoolLit _) = False

  isNested (Add {}) = True
  isNested (Sub {}) = True
  isNested (Equal {}) = True
  isNested (Not {}) = True
  isNested (And {}) = True

