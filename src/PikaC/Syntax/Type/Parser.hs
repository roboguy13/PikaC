module PikaC.Syntax.Type.Parser
  where

import PikaC.Syntax.Type
import PikaC.Syntax.ParserUtils

import Text.Megaparsec

import Control.Applicative hiding (some, many)
import Data.Functor
import Data.Maybe

import Unbound.Generics.LocallyNameless (string2Name)

parseTypeSig :: Parser TypeSig
parseTypeSig = label "type signature" $ lexeme $ do
  constraints <- fromMaybe [] <$> optional (parseLayoutConstraintList <* symbol "=>")
  TypeSig constraints <$> parseType

parseType :: Parser Type
parseType = label "type" $ lexeme $
  try parseFnType <|>
  try parseType'

parseType' :: Parser Type
parseType' = label "type" $ lexeme $
  try (symbol "(" *> parseType <* symbol ")") <|>
  try (keyword "Int" $> IntType) <|>
  try (keyword "Bool" $> BoolType) <|>
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
  keyword "layout"
  symbol "("
  adt <- parseAdtName
  symbol ")"
  pure (tyVar :~ adt)

parseTypeName :: Parser TypeName
parseTypeName = label "type variable" $ lexeme $ string2Name <$> (parseLowercaseName <|> parseUppercaseName)

parseAdtName :: Parser AdtName
parseAdtName = label "ADT name" $ lexeme $ AdtName <$> parseUppercaseName

