module PikaC.Syntax.Type.Parser
  where

import PikaC.Syntax.Type
import PikaC.Syntax.ParserUtils

import PikaC.Utils

import Text.Megaparsec
import Text.Megaparsec.Char

import Control.Applicative hiding (some, many)
import Data.Functor
import Data.Maybe

import Unbound.Generics.LocallyNameless

import Control.Lens

parseTypeSig :: Alpha a => Parser a -> Parser (TypeSig' a)
parseTypeSig p = label "type signature" $ lexeme $ do
  ty <- parseType --parseConstrainedType -- TODO: Fix parseConstrainedType and use it here
  let ctype = ConstrainedType [] ty
  x <- p

  let typeFVs :: [TypeName]
      typeFVs = fastNub $ toListOf fv ctype

  pure $ TypeSig $ bind typeFVs (ctype, x)

parseConstrainedType :: Parser ConstrainedType
parseConstrainedType = label "constrained type" $ lexeme $ do
  constraints <- fromMaybe [] <$> optional (parseLayoutConstraintList <* symbol "=>")
  ConstrainedType constraints <$> parseType

parseType :: Parser Type
parseType = label "type" $ lexeme $
  try parseFnType <|>
  try parseType'

parseGhostApp :: Parser Type
parseGhostApp = do
  t <- parseType''
  xs <- some parseGhostArg
  pure $ GhostApp t xs

parseGhostArg :: Parser String
parseGhostArg = do
  char '@'
  parseLowercaseName

parseType' :: Parser Type
parseType' = 
  try parseGhostApp <|>
  parseType''

parseType'' :: Parser Type
parseType'' = label "type" $ lexeme $
  try (symbol "(" *> parseType <* symbol ")") <|>
  try (keyword "Int" $> IntType) <|>
  try (keyword "Bool" $> BoolType) <|>
  try (LayoutId <$> parseLayoutId) <|>
  (TyVar <$> parseTypeName)

parseFnType :: Parser Type
parseFnType = label "function type" $ lexeme $ do
  src <- parseType'
  symbol "->"
  FnType src <$> parseType

parseLayoutConstraintList :: Parser [LayoutConstraint]
parseLayoutConstraintList = label "layout constraint list" $ lexeme $ do
  symbol "("
  constraints <- many parseLayoutConstraint
  symbol ")"
  pure constraints

parseLayoutConstraint :: Parser LayoutConstraint
parseLayoutConstraint = label "layout constraint" $ lexeme $ do
  tyVar <- parseTypeName
  symbol ":~"
  -- keyword "layout"
  -- symbol "("
  adt <- parseAdtName
  -- symbol ")"
  pure (tyVar :~ adt)

parseTypeName :: Parser TypeName
parseTypeName = label "type variable" $ lexeme $ string2Name <$> parseLowercaseName

parseLayoutId :: Parser String
parseLayoutId = label "layout name" $ lexeme $ parseUppercaseName

parseAdtName :: Parser AdtName
parseAdtName = label "ADT name" $ lexeme $ AdtName <$> parseUppercaseName

