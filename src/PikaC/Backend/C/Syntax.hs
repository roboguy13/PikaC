{-# LANGUAGE DeriveGeneric  #-}

module PikaC.Backend.C.Syntax
  where

import PikaC.Syntax.Heaplet
import PikaC.Ppr
import PikaC.Utils

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Unsafe

import GHC.Generics

import Data.Validity

import Control.Lens.Plated

type CName = Name CExpr
type CLoc = Loc CExpr

data CExpr
  = V CName
  | LocValue CLoc
  | IntLit Int
  | BoolLit Bool
  | Add CExpr CExpr
  | Mul CExpr CExpr
  | Sub CExpr CExpr
  | Equal CExpr CExpr
  | Lt CExpr CExpr
  | Le CExpr CExpr
  | And CExpr CExpr
  | Mod CExpr CExpr
  | Div CExpr CExpr
  | Not CExpr
  | Deref CLoc
  | DerefLocType CLoc
  | AsInt CExpr
  | If CExpr CExpr CExpr
  deriving (Show, Eq, Ord, Generic)

data Command
  = Assign CLoc CExpr
  | SimpleAssign CName CName
  | SetToNull CName
  | IfThenElse CExpr [Command] [Command]
  | IfThen CExpr [Command]
  | Assert CExpr [Command]
  | Call
      String
      [CExpr] -- Input parameters
      [CExpr] -- Output parameters
  | IntoMalloc Int CName [Command]
  -- | Let CLoc (Bind CName [Command])
  | Let CLoc CName [Command]
  | Free CName
  | Decl CName
  | Printf String [CExpr]
  | ToInt CName
  | Nop
  deriving (Show, Generic)

instance Plated Command where
  plate _ cmd@(Assign {}) = pure cmd
  plate _ cmd@(SimpleAssign {}) = pure cmd
  plate _ cmd@(SetToNull {}) = pure cmd
  plate f (IfThenElse c x y) = IfThenElse c <$> traverse f x <*> traverse f y
  plate f (Assert e cmds) = Assert e <$> traverse f cmds
  plate f cmd@(Call {}) = pure cmd
  plate f (IntoMalloc sz v cmds) = IntoMalloc sz v <$> traverse f cmds
  plate f (Let loc v cmds) = Let loc v <$> traverse f cmds
  plate f cmd@(Free {}) = pure cmd
  plate f cmd@(Decl {}) = pure cmd
  plate f cmd@(Printf {}) = pure cmd
  plate f cmd@(ToInt {}) = pure cmd
  plate f Nop = pure Nop

data CFunction =
  CFunction
    { cfunctionName :: String
    , cfunctionParams :: [CName]
    , cfunctionBody :: [Command]
    }
  deriving (Show, Generic)

declFunction :: CFunction -> Doc
declFunction fn =
  text "void " <>
   text (cfunctionName fn) <> text "(" <> hsep (punctuate (text ",") (map ((text "loc" <+>) . ppr) (cfunctionParams fn))) <> text ");"

instance HasVar CExpr where
  mkVar = V

instance Alpha CFunction

instance Alpha Command

instance Alpha CExpr

instance Ppr Command where
  ppr = runFreshM . go
    where
      go :: Command -> FreshM Doc
      go (ToInt n) =
        pure $ ppr n <+> text "=" <+> (text "(long)(" <> ppr n <> text "->ssl_int);")
      go (SimpleAssign x y) =
        pure $ (ppr x <> text " = &" <> ppr y) <> text ";"
      go (Decl n) = do
        pure $ hsep [text "loc", ppr n <> text " = NULL;"]
      go (Assign loc e) =
        pure $ writeLoc loc e
        -- hsep [text "*" <> ppr loc, text "=", ppr e] <> text ";"

      go (SetToNull n) =
        pure $ ppr n <+> text "=" <+> text "NULL;"

      go (Assert c rest) = do
        restDoc <- mapM go rest
        pure (text "assert(" <> ppr c <> text ");" $$ vcat restDoc)

      go (IfThen c t) = do
        tDoc <- mapM go t
        -- fDoc <- mapM go f
        pure $ foldr1 ($$) [hsep [text "if (" <> ppr c <> text ")", text "{"]
            ,nest 1 (vcat tDoc)
            -- ,hsep [text "}", text "else", text "{"]
            -- ,nest 1 (hsep fDoc)
            ,text "}"
            ]

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
        pure $ text f <> text "(" <> hsep (punctuate (text ",") (map ppr inArgs ++ map ((text "&" <>) . ppr) outArgs)) <> text ");"

      go (IntoMalloc size target body) = do
        -- (target, body) <- unbind bnd
        bodyDocs <- mapM go body
        pure $ hsep [text "loc", ppr target, text "=", text "(loc)malloc(" <> ppr size <> text " * sizeof(loc));"] $$ vcat bodyDocs

      go (Free x) =
        pure $ hsep [text "free(", ppr x, text ");"]

      -- ppr (Let x y) = ("loc" <+> ppr x <+> "=" <+> ppr (Deref y)) <> ";"
      go (Let x var body) = do
        bodyDocs <- mapM go body
        pure $
          (text "loc" <+> ppr var <+> text "=" <+> readLoc x)
          $$ vcat bodyDocs

      go (Printf fmt args) =
        pure $
          text "printf("
            <> hsep (punctuate (text ",") ((text "\"" <> text fmt <> text "\"") : map ppr args))
            <> text ");"

      go Nop = pure mempty --text ";"

writeLoc :: Ppr b => CLoc -> b -> Doc
writeLoc (x :+ i) y =
  text "WRITE_LOC(" <> hsep (punctuate (text ",") [ppr x , ppr i, ppr y]) <> text ");"

readLoc :: CLoc -> Doc
readLoc (x :+ i) =
  text "READ_LOC(" <> hsep (punctuate (text ",") [ppr x, ppr i]) <> text ");"

instance Ppr CFunction where
  ppr fn =
    hsep [
         if cfunctionName fn == "main"
           then text "int"
           else text "void"
         ,
         text (cfunctionName fn) <> text "(" <> hsep (punctuate (text ",") (map ((text "loc" <+>) . ppr) (cfunctionParams fn))) <> text ")", text "{"]
      $$
       nest 1 (vcat (map ppr (cfunctionBody fn)))
      $$
        (if cfunctionName fn == "main"
          then nest 1 $ text "return 0;"
          else mempty)
      $$
       text "}"

instance Ppr CExpr where
  ppr (V x) = ppr x
  ppr (AsInt x) = ppr x <> text "->ssl_int"
  ppr (LocValue x) = ppr x
  ppr (IntLit i) = ppr i
  ppr (BoolLit True) = text "true"
  ppr (BoolLit False) = text "false"
  ppr (Mod x y) = sep [intCast (pprP x), text "%", intCast (pprP y)]
  ppr (Div x y) = sep [intCast (pprP x), text "/", intCast (pprP y)]
  ppr (Add x y) = sep [intCast (pprP x), text "+", intCast (pprP y)]
  ppr (Mul x y) = sep [intCast (pprP x), text "*", intCast (pprP y)]
  ppr (Sub x y) = sep [intCast (pprP x), text "-", intCast (pprP y)]
  ppr (Equal x y) = sep [pprP x, text "==", pprP y]
  ppr (Lt x y) = sep [pprP x, text "<", pprP y]
  ppr (Le x y) = sep [pprP x, text "<=", pprP y]
  ppr (Not x) = text "!" <> pprP x
  ppr (And x y) = sep [pprP x, text "&&", pprP y]
  ppr (Deref x) = text "*" <> ppr x
  ppr (DerefLocType x) = text "*(loc*)" <> ppr x
  ppr (If c t f) = ppr c <+> text "?" <+> ppr t <+> text ":" <+> ppr f

intCast :: Doc -> Doc
intCast d = text "(long)" <> d

instance IsNested CExpr where
  isNested (V _) = False
  isNested (LocValue _) = False
  isNested (IntLit _) = False
  isNested (BoolLit _) = False

  isNested (AsInt _) = True
  isNested (Div {}) = True
  isNested (Mod {}) = True
  isNested (Add {}) = True
  isNested (Mul {}) = True
  isNested (Sub {}) = True
  isNested (Equal {}) = True
  isNested (Lt {}) = True
  isNested (Le {}) = True
  isNested (Not {}) = True
  isNested (And {}) = True
  isNested (Deref {}) = False
  isNested (DerefLocType {}) = False
  isNested (If {}) = True

whenTrue :: CExpr -> [Command] -> [Command]
whenTrue (BoolLit True) cmds = cmds
whenTrue cond cmds = [IfThen cond cmds]

instance IsName CExpr CExpr where
  getName (V x) = x
  getName e = error $ "IsName CExpr CExpr requires var, got " ++ ppr' e

-- findSetToZero :: [CName] -> [CName] -> [PointsTo CExpr] -> [CName]
-- findSetToZero possiblyUpdated names xs =
--     let modified = go xs
--     in
--     filter (`elem` possiblyUpdated) $ filter (`notElem` modified) names
--     -- filter (`notElem` modified) names
--   where
--     go [] = []
--     -- go ((x :-> V y):rest) = locBase x : y : go rest
--     -- go ((x :-> LocValue y):rest) = locBase x : locBase y : go rest
--     go ((x :-> y):rest) = getName (locBase x) : go rest

instance IsBase CExpr where
  isVar (V {}) = True
  isVar _ = False

  isLit (BoolLit {}) = True
  isLit (IntLit {}) = True
  isLit _ = False

  intLit = IntLit
  boolLit = BoolLit
  mkNot = Not
  mkEqual = Equal
  mkAnd = And

andExpr :: CExpr -> CExpr -> CExpr
andExpr x (BoolLit True) = x
andExpr (BoolLit True) y = y
andExpr x y = And x y

--
-- Property tests
--

-- instance WellScoped (Name CExpr) CExpr
-- instance WellScoped (Name CExpr) Command
-- instance WellScoped (Name CExpr) (Bind CName [Command]) where
--   wellScoped inScopes bnd =
--     let (var, cmds) = unsafeUnbind bnd
--     in
--     wellScoped @_ @[Command] (inScopes ++ [var]) cmds
--
-- instance WellScoped (Name CExpr) CFunction where
--   wellScoped inScopes fn =
--     wellScoped (inScopes ++ cfunctionParams fn) (cfunctionBody fn)
--
instance Validity CFunction where
  validate fn = mempty --wellScoped ([] :: [CName]) fn

