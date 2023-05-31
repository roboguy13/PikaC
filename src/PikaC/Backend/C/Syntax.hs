{-# LANGUAGE DeriveGeneric  #-}

module PikaC.Backend.C.Syntax
  where

import PikaC.Syntax.Heaplet
import PikaC.Ppr
import PikaC.Utils

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
  | IntoMalloc Int (Bind CName [Command])
  | Let CLoc (Bind CName [Command])
  | Free CName
  | Decl CName
  | Nop
  deriving (Show, Generic)

data CFunction =
  CFunction
    { cfunctionName :: String
    , cfunctionParams :: [CName]
    , cfunctionBody :: [Command]
    }
  deriving (Show)

instance Alpha Command

instance Alpha CExpr

instance Ppr Command where
  ppr = runFreshM . go
    where
      go :: Command -> FreshM Doc
      go (Decl n) = do
        pure $ hsep [text "loc", ppr n <> text " = NULL;"]
      go (Assign loc e) =
        pure $ writeLoc loc e
        -- hsep [text "*" <> ppr loc, text "=", ppr e] <> text ";"

      go (IfThenElse c t f) = do
        tDoc <- mapM go t
        fDoc <- mapM go f
        pure $ foldr1 ($$) [hsep [text "if (" <> ppr c <> text ")", text "{"]
            ,nest 1 (vcat tDoc)
            ,hsep [text "}", text "else", text "{"]
            ,nest 1 (hsep fDoc)
            ,text "}"
            ]

      go (Call f inArgs outArgs) =
        pure $ text f <> text "(" <> hsep (punctuate (text ",") (map ppr inArgs ++ map ppr outArgs)) <> text ");"

      go (IntoMalloc size bnd) = do
        (target, body) <- unbind bnd
        bodyDocs <- mapM go body
        pure $ text "{" $$ hsep [text "loc", ppr target, text "=", text "(loc)malloc(" <> ppr size <> text " * sizeof(loc));"] $$ vcat bodyDocs $$ text "}"

      go (Free x) =
        pure $ hsep [text "free(", ppr x, text ");"]

      -- ppr (Let x y) = ("loc" <+> ppr x <+> "=" <+> ppr (Deref y)) <> ";"
      go (Let x bnd) = do
        (var, body) <- unbind bnd
        bodyDocs <- mapM go body
        pure $
          text "{" $$ (text "loc" <+> ppr var <+> text "=" <+> readLoc x)
          $$ vcat bodyDocs $$ text "}"
      go Nop = pure mempty --text ";"

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
  ppr (Not x) = text "!" <> pprP x
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

instance IsName CExpr CExpr where
  getName (V x) = x
  getName e = error $ "IsName CExpr CExpr requires var, got " ++ ppr' e

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
    go ((x :-> y):rest) = getName (locBase x) : go rest

computeBranchCondition :: [CName] -> [CName] -> CExpr
computeBranchCondition defNames branchNames =
    go allInputNames
  where
    allInputNames = defNames --fnDefInputNames def

    checkName name =
      if name `elem` branchNames
        then Not (Equal (V name) (IntLit 0))
        else Equal (V name) (IntLit 0)

    go [] = BoolLit True
    go [name] = checkName name
    go (name:rest) =
      And (checkName name) (go rest)

