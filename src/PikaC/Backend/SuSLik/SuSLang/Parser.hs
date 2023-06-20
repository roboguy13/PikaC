module PikaC.Backend.SuSLik.SuSLang.Parser
  where

import PikaC.Backend.SuSLik.SuSLang.Syntax

import Text.Megaparsec hiding (parseTest)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import PikaC.Syntax.ParserUtils hiding (sc, lexeme, keywords, parseLowercaseName)
import PikaC.Syntax.Heaplet

import Unbound.Generics.LocallyNameless

import Data.Functor

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parseFunction :: Parser Function
parseFunction = do
  keyword "void"
  fn <- parseName
  symbol "("
  (types, params) <- unzip <$> parseParam `sepBy1` symbol ","
  symbol ")"
  symbol "{"
  body <- many parseCommand
  symbol "}"
  pure $
    Function
    { functionName = fn
    , functionParamTypes = types
    , functionBody = bind params body
    }

parseParam :: Parser (SuSType, ExprName)
parseParam = liftA2 (,) parseTypeS parseExprName
  where
    parseTypeS =
      (keyword "loc" $> LocS) <|>
      (keyword "int" $> IntS) <|>
      (keyword "bool" $> BoolS)

parseCommand :: Parser Command
parseCommand = label "command" $ lexeme $
  try parseLetMalloc <|>
  try parseLet <|>
  try parseIfThenElse <|>
  try parseWrite <|>
  try parseCall <|>
  try parseFree

parseLetMalloc :: Parser Command
parseLetMalloc = label "let-malloc" $ lexeme $ do
  keyword "let"
  x <- parseExprName
  symbol "="
  keyword "malloc"
  symbol "("
  sz <- parseInt
  symbol ")"
  symbol ";"
  rest <- some parseCommand
  pure $ LetMalloc sz (bind x rest)

parseLet :: Parser Command
parseLet = label "let" $ lexeme $ do
  keyword "let"
  x <- parseExprName
  symbol "="
  e <- parseExpr
  symbol ";"
  rest <- some parseCommand
  pure $ Let e (bind x rest)

parseIfThenElse :: Parser Command
parseIfThenElse = label "if-then-else" $ lexeme $ do
  keyword "if"
  symbol "("
  cond <- parseExpr
  symbol ")"
  symbol "{"
  t <- many parseCommand
  symbol "}"
  keyword "else"
  symbol "{"
  f <- many parseCommand
  symbol "}"
  pure $ IfThenElse cond t f

parseCall :: Parser Command
parseCall = label "function call" $ lexeme $ do
  fn <- parseName
  symbol "("
  args <- parseExpr `sepBy1` symbol ","
  symbol ")"
  symbol ";"
  pure $ Call fn args

parseWrite :: Parser Command
parseWrite = label "write command" $ lexeme $ do
  lhs <- parseLoc
  symbol "="
  rhs <- parseExpr
  symbol ";"
  pure $ Write lhs rhs

parseFree :: Parser Command
parseFree = label "free" $ lexeme $ do
  keyword "free"
  symbol "("
  x <- parseExprName
  symbol ")"
  symbol ";"
  pure $ Free $ V x

parseLoc :: Parser (Loc Expr)
parseLoc = label "dereferenced location" $ lexeme $
  locHere <|> go
  where
    locHere = try $ do
      symbol "*"
      x <- parseExprName
      pure (V x :+ 0)
    go = do
      symbol "*"
      symbol "("
      x <- parseExprName
      symbol "+"
      i <- parseInt
      symbol ")"
      pure (V x :+ i)

parseExpr :: Parser Expr
parseExpr = label "expression" $ lexeme $
  try (fmap LocVal parseLoc) <|>
  try (parseBinOp "%" Mod) <|>
  try (parseBinOp "&&" And) <|>
  try (parseBinOp "/" Div) <|>
  try (parseBinOp "+" Add) <|>
  try (parseBinOp "*" Mul) <|>
  try (parseBinOp "-" Sub) <|>
  try (parseBinOp "==" Equal) <|>
  try (parseBinOp "<" Lt) <|>
  try parseExpr'

parseExpr' :: Parser Expr
parseExpr' = label "expression" $ lexeme $
  try (symbol "(" *> parseExpr <* symbol ")") <|>
  try (IntLit <$> parseInt) <|>
  try (keyword "false" $> BoolLit False) <|>
  try (keyword "true" $> BoolLit True) <|>
  try (V <$> parseExprName)

parseExprName :: Parser ExprName
parseExprName = string2Name <$> parseName

parseBinOp :: String -> (Expr -> Expr -> Expr) -> Parser Expr
parseBinOp op p = do
  x <- parseExpr'
  symbol op
  p x <$> parseExpr'

parseName :: Parser String
parseName = lexeme $ do
  n <- liftA2 (:) (lowerChar <|> upperChar) parseNameTail
  parserGuard (n `notElem` keywords) (Just n) "identifier"
  pure n

keywords :: [String]
keywords = ["if", "let", "malloc", "else", "free", "void"]

