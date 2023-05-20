module PikaC.Syntax.Pika.Parser
  where

import PikaC.Syntax.ParserUtils

import PikaC.Syntax.Pika.Expr
import PikaC.Syntax.Pika.Pattern
import PikaC.Syntax.Pika.FnDef
import PikaC.Syntax.Pika.Layout
import PikaC.Syntax.Heaplet

import PikaC.Syntax.Type.Parser
import PikaC.Syntax.Type

import Text.Megaparsec
import Text.Megaparsec.Char

import Control.Applicative hiding (some, many)
import Data.Functor
import Data.Maybe

import Unbound.Generics.LocallyNameless (string2Name, bind)

data PikaModule =
  PikaModule
  { moduleLayouts :: [Layout Expr]
  , moduleFnDefs :: [FnDef]
  , moduleGenerates :: [String]
  }
  deriving (Show)

instance Semigroup PikaModule where
  PikaModule xs1 ys1 zs1 <> PikaModule xs2 ys2 zs2 =
    PikaModule (xs1 <> xs2) (ys1 <> ys2) (zs1 <> zs2)

instance Monoid PikaModule where
  mempty = PikaModule mempty mempty mempty

singleLayout :: Layout Expr -> PikaModule
singleLayout x = mempty { moduleLayouts = [x] }

singleFnDef :: FnDef -> PikaModule
singleFnDef x = mempty { moduleFnDefs = [x] }

singleGenerate :: String -> PikaModule
singleGenerate x = mempty { moduleGenerates = [x] }

moduleLookupLayout :: PikaModule -> String -> Layout Expr
moduleLookupLayout = lookupLayout . moduleLayouts

moduleLookupFn :: PikaModule -> String -> FnDef
moduleLookupFn pikaModule name = go (moduleFnDefs pikaModule)
  where
    go [] = error $ "moduleLookupFn: Cannot find function named " ++ show name
    go (x:xs)
      | fnDefName x == name = x
      | otherwise           = go xs

parsePikaModule :: Parser PikaModule
parsePikaModule = do
  generates <- mconcat . map singleGenerate <$> some parseGenerate
  layouts <- mconcat . map singleLayout <$> some parseLayout
  fnDefs <- mconcat . map singleFnDef <$> some parseFnDef

  pure (generates <> layouts <> fnDefs)

  -- fmap mconcat $ some $
  --   try (singleGenerate <$> parseGenerate) <|>
  --   try (singleLayout <$> parseLayout) <|>
  --   (singleFnDef <$> parseFnDef)

parseGenerate :: Parser String
parseGenerate = label "generate directive" $ lexeme $ do
  keyword "%generate"
  parseFnName

parseFnDef :: Parser FnDef
parseFnDef = label "function definition" $ lexeme $ do
  fnName <- parseFnName
  symbol ":"
  sig <- parseTypeSig
  symbol ";"

  FnDef fnName sig <$> some (parseFnDefBranch fnName)

parseFnDefBranch :: String -> Parser FnDefBranch
parseFnDefBranch fnName = label "function branch" $ lexeme $ try $ do
  nameHere <- parseFnName
  parserGuard (nameHere == fnName) (Just nameHere) fnName

  pats <- some parsePattern
  symbol ":="
  body <- parseExpr

  symbol ";"

  pure $ FnDefBranch pats body

parsePattern :: Parser (Pattern Expr)
parsePattern = label "pattern" $ lexeme $
  try (PatternVar <$> parsePatternVar) <|>
  try go <|>
  try ((`Pattern` []) <$> parseConstructorName)
  where
    go = do
      symbol "("
      c <- parseConstructorName
      vs <- some parsePatternVar
      symbol ")"
      pure $ Pattern c vs


parseExpr :: Parser Expr
parseExpr = label "expression" $ lexeme $
  try (parseBinOp "+" Add) <|>
  try (parseBinOp "-" Sub) <|>
  try (parseBinOp "==" Equal) <|>
  try parseLayoutLambda <|>
  try parseApplyLayout <|>
  try parseApp <|>
  try parseExpr'

parseExpr' :: Parser Expr
parseExpr' = label "expression" $ lexeme $
  try (symbol "(" *> parseExpr <* symbol ")") <|>
  try (IntLit . read <$> some digitChar) <|>
  try (keyword "False" $> BoolLit False) <|>
  try (keyword "True" $> BoolLit True) <|>
  try (V <$> parseLowercaseExprName)

parseLayoutLambda :: Parser Expr
parseLayoutLambda = label "layout lambda" $ lexeme $ do
  symbol "/\\"
  (ty :~ adt) <- parseLayoutConstraint
  symbol "."
  LayoutLambda adt . bind ty <$> parseExpr

parseApplyLayout :: Parser Expr
parseApplyLayout = label "layout application" $ lexeme $ do
  e <- try parseApp <|> parseExpr' <|> parseNullaryConstructorApp
  symbol "["
  ty <- parseTypeName
  symbol "]"
  pure (ApplyLayout e ty)

parseNullaryConstructorApp :: Parser Expr
parseNullaryConstructorApp = lexeme $ App <$> parseConstructorName <*> pure []

parseApp :: Parser Expr
parseApp = label "function application" $ lexeme $
  App <$> parseFnName <*> some parseExpr'

parseBinOp :: String -> (Expr -> Expr -> Expr) -> Parser Expr
parseBinOp op p = do
  x <- parseExpr'
  symbol op
  p x <$> parseExpr'

parseLayout :: Parser (Layout Expr)
parseLayout = label "layout definition" $ lexeme $ do
  layoutName <- parseLayoutName
  keyword ":"
  sig <- parseLayoutSig
  keyword ";"
  branches <- some (parseLayoutBranch layoutName)
  pure $ Layout layoutName sig branches

parseLayoutSig :: Parser (LayoutSig Expr)
parseLayoutSig = lexeme $ do
  keyword "layout"
  symbol "["
  params <- some parseLayoutVar
  symbol "]"
  symbol "("
  adt <- parseAdtName
  symbol ")"
  pure $ LayoutSig adt params


parseLayoutBranch :: String -> Parser (LayoutBranch Expr)
parseLayoutBranch layoutName = lexeme $ try $ do
  nameHere <- parseLayoutName
  parserGuard (nameHere == layoutName) (Just nameHere) layoutName

  pat <- parsePattern
  keyword ":="
  body <- parseLayoutBody
  keyword ";"

  pure $ LayoutBranch pat body

parseLayoutBody :: Parser (LayoutBody Expr)
parseLayoutBody = label "layout body" $ lexeme $
  try (keyword "emp" $> LayoutBody []) <|>
  try (LayoutBody <$> sepBy1 parseLayoutHeaplet (symbol "**"))

parseLayoutHeaplet :: Parser (LayoutHeaplet Expr)
parseLayoutHeaplet = label "heaplet" $ lexeme $
  try parseLayoutApply <|>
  try (LPointsTo <$> parsePointsTo)

parsePointsTo :: Parser (PointsTo Expr)
parsePointsTo = label "points-to" $ lexeme $ do
  lhs <- parseLoc
  symbol ":->"
  rhs <- parseExpr
  pure (lhs :-> rhs)

parseLoc :: Parser (Loc Expr)
parseLoc = label "location" $ lexeme $
  try go <|>
  try (fmap (:+ 0) parseExprName)
  where
    go = do
      symbol "("
      v <- parseExprName
      symbol "+"
      i <- read <$> some digitChar
      symbol ")"
      pure (v :+ i)

parseLayoutApply :: Parser (LayoutHeaplet Expr)
parseLayoutApply = do
  layoutName <- parseLayoutName
  n <- parseExprName
  LApply layoutName (V n) <$> some parseLayoutArg

parseLayoutArg :: Parser ExprName
parseLayoutArg = label "layout argument" $ lexeme $
  symbol "[" *> parseLayoutVar <* symbol "]"

parseLayoutVar :: Parser ExprName
parseLayoutVar = label "layout variable" $ lexeme $
  fmap string2Name (parseLowercaseName <|> parseUppercaseName)

parseExprName :: Parser ExprName
parseExprName = label "variable" $ lexeme $ string2Name <$> (parseLowercaseName <|> parseUppercaseName)

parseLowercaseExprName :: Parser ExprName
parseLowercaseExprName = label "variable" $ lexeme $ string2Name <$> parseLowercaseName

parseConstructorName :: Parser String
parseConstructorName = label "constructor name" $ lexeme parseUppercaseName

parseFnName :: Parser String
parseFnName = label "function name" $ lexeme (parseLowercaseName <|> parseUppercaseName)

parseLayoutName :: Parser String
parseLayoutName = label "layout name" $ lexeme parseUppercaseName

parsePatternVar :: Parser ExprName
parsePatternVar = label "pattern variable" $ string2Name <$> lexeme parseLowercaseName

