module PikaC.Syntax.ParserUtils
  where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Monad.Combinators.Expr

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set

import Data.Void

import Control.Applicative hiding (some, many)

type Parser = Parsec Void String

parse' :: Parser a -> String -> a
parse' = parse'' "<input>"

parse'' :: String -> Parser a -> String -> a
parse'' sourceName p str =
  case parse p sourceName str of
    Left err -> error $ errorBundlePretty err
    Right x -> x

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "--")
  (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

keyword :: String -> Parser String
keyword str = lexeme (string str <* notFollowedBy alphaNumChar)

parseBracketed :: Parser a -> Parser a -> Parser b -> Parser b
parseBracketed left right p = left *> p <* right

parseInt :: Parser Int
parseInt = read <$> some digitChar

parserGuard :: Bool -> Maybe String -> String -> Parser ()
parserGuard True _ _ = pure ()
parserGuard False unexpected expected =
  failure (fmap (Label . NonEmpty.fromList) unexpected) (Set.singleton (Label (NonEmpty.fromList expected)))

parseNameTail :: Parser String
parseNameTail = many (alphaNumChar <|> char '_')

parseUppercaseName :: Parser String
parseUppercaseName = lexeme $ liftA2 (:) upperChar parseNameTail

parseLowercaseName :: Parser String
parseLowercaseName = lexeme $ do
  n <- liftA2 (:) lowerChar parseNameTail
  parserGuard (n `notElem` keywords) (Just n) "identifier"
  pure n

parseIdentifier :: Parser String
parseIdentifier = label "identifier"
  parseLowercaseName

keywords :: [String]
keywords = ["not", "data", "deref", "addr", "in", "with", "layout", "let", "if", "then", "else", "synth"]

parseConstructorName :: Parser String
parseConstructorName = label "constructor name"
  parseUppercaseName

binaryL name f = InfixL  (f <$ symbol name)
binaryR name f = InfixR  (f <$ symbol name)
binaryN name f = InfixN  (f <$ symbol name)
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

