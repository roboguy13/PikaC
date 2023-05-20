{-# LANGUAGE DeriveGeneric  #-}

module PikaC.Backend.C.Syntax
  where

import PikaC.Syntax.Heaplet
import PikaC.Ppr

import Unbound.Generics.LocallyNameless

import GHC.Generics

type CName = Name CExpr
type CLoc = Loc CExpr

data CExpr
  = V CName
  | LocValue CLoc
  | IntLit Int
  | BoolLit Bool
  | Add CExpr CExpr
  | Sub CExpr CExpr
  | Equal CExpr CExpr
  | And CExpr CExpr
  | Not CExpr
  | Deref CLoc
  deriving (Show, Eq, Ord, Generic)

data Command
  = Assign CLoc CExpr
  | IfThenElse CExpr [Command] [Command]
  | Call
      String
      [CExpr] -- Input parameters
      [CExpr] -- Output parameters
  | IntoMalloc CName Int
  | Let CName CLoc
  | Free CName
  | Decl CName
  | Nop
  deriving (Show, Eq, Ord, Generic)

data CFunction =
  CFunction
    { cfunctionName :: String
    , cfunctionParams :: [CName]
    , cfunctionBody :: [Command]
    }

instance Alpha Command

instance Alpha CExpr

instance Ppr Command where
  ppr (Decl n) = hsep [text "loc", ppr n <> text " = NULL;"]
  ppr (Assign loc e) =
    writeLoc loc e
    -- hsep [text "*" <> ppr loc, text "=", ppr e] <> text ";"

  ppr (IfThenElse c t f) =
    foldr1 ($$) [hsep [text "if (" <> ppr c <> text ")", text "{"]
        ,nest 1 (vcat (map ppr t))
        ,hsep [text "}", text "else", text "{"]
        ,nest 1 (hsep (map ppr f))
        ,text "}"
        ]

  ppr (Call f inArgs outArgs) =
    text f <> text "(" <> hsep (punctuate (text ",") (map ppr inArgs ++ map ppr outArgs)) <> text ");"

  ppr (IntoMalloc target size) =
    hsep [text "loc", ppr target, text "=", text "(loc)malloc(" <> ppr size <> text " * sizeof(loc));"]

  ppr (Free x) =
    hsep [text "free(", ppr x, text ");"]

  -- ppr (Let x y) = ("loc" <+> ppr x <+> "=" <+> ppr (Deref y)) <> ";"
  ppr (Let x y) = text "loc" <+> ppr x <+> text "=" <+> readLoc y
  ppr Nop = mempty --text ";"

writeLoc :: Ppr b => CLoc -> b -> Doc
writeLoc (x :+ i) y =
  text "WRITE_LOC(" <> hsep (punctuate (text ",") [ppr x , ppr i, ppr y]) <> text ");"

readLoc :: CLoc -> Doc
readLoc (x :+ i) =
  text "READ_LOC(" <> hsep (punctuate (text ",") [ppr x, ppr i]) <> text ");"

instance Ppr CFunction where
  ppr fn =
    hsep [text "void", text (cfunctionName fn) <> text "(" <> hsep (punctuate (text ",") (map ((text "loc" <+>) . ppr) (cfunctionParams fn))) <> text ")", text "{"]
      $$
       nest 1 (vcat (map ppr (cfunctionBody fn)))
      $$
       text "}"

instance Ppr CExpr where
  ppr (V x) = ppr x
  ppr (LocValue x) = ppr x
  ppr (IntLit i) = ppr i
  ppr (BoolLit True) = text "true"
  ppr (BoolLit False) = text "false"
  ppr (Add x y) = sep [intCast (pprP x), text "+", intCast (pprP y)]
  ppr (Sub x y) = sep [intCast (pprP x), text "-", intCast (pprP y)]
  ppr (Equal x y) = sep [pprP x, text "==", pprP y]
  ppr (Not x) = sep [text "!", pprP x]
  ppr (And x y) = sep [pprP x, text "&&", pprP y]
  ppr (Deref x) = text "*" <> ppr x

intCast :: Doc -> Doc
intCast d = text "(int)" <> d

instance IsNested CExpr where
  isNested (V _) = False
  isNested (LocValue _) = False
  isNested (IntLit _) = False
  isNested (BoolLit _) = False

  isNested (Add {}) = True
  isNested (Sub {}) = True
  isNested (Equal {}) = True
  isNested (Not {}) = True
  isNested (And {}) = True
  isNested (Deref {}) = False

findSetToZero :: [CName] -> [CName] -> [PointsTo CExpr] -> [CName]
findSetToZero possiblyUpdated names xs =
    let modified = go xs
    in
    filter (`elem` possiblyUpdated) $ filter (`notElem` modified) names
    -- filter (`notElem` modified) names
  where
    go [] = []
    -- go ((x :-> V y):rest) = locBase x : y : go rest
    -- go ((x :-> LocValue y):rest) = locBase x : locBase y : go rest
    go ((x :-> y):rest) = locBase x : go rest

