{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module PikaC.Syntax.Pika.Parser
  where

import PikaC.Syntax.ParserUtils

import PikaC.Syntax.Pika.Expr
import PikaC.Syntax.Pika.Pattern
import PikaC.Syntax.Pika.FnDef
import PikaC.Syntax.Pika.Layout
import PikaC.Syntax.Heaplet
import qualified PikaC.Syntax.PikaCore.Expr as PikaCore

import PikaC.Syntax.Type.Parser
import PikaC.Syntax.Type

import PikaC.Utils
import PikaC.Ppr hiding (char)

import Text.Megaparsec
import Text.Megaparsec.Char

import Control.Applicative hiding (some, many)
import Data.Functor
import Data.Maybe

import Unbound.Generics.LocallyNameless (string2Name, bind)

import Test.QuickCheck hiding (label)

import Control.Monad
import GHC.Generics

import Debug.Trace

import Control.DeepSeq

data PikaModule =
  PikaModule
  { moduleLayouts :: [Layout Expr]
  , moduleFnDefs :: [FnDef]
  , moduleGenerates :: [String]
  }
  deriving (Show, Generic)

instance NFData PikaModule

instance Ppr PikaModule where
  ppr (PikaModule layouts fns generates) =
    text "-- Layouts:"
    $$ vcat (map ppr layouts)
    $$ text "-- Fn defs:"
    $$ vcat (map ppr fns)

isTrivialModule :: PikaModule -> Bool
isTrivialModule (PikaModule x y z) =
  null x || null y || null z

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

  pure $ FnDefBranch (PatternMatches (bind pats body))

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
  (params, adt) <- parseLayoutSig
  keyword ";"
  branches <- some (parseLayoutBranch layoutName)
  pure $ Layout layoutName adt (bind params branches)

parseLayoutSig :: Parser ([ModedName Expr], AdtName)
parseLayoutSig = lexeme $ do
  keyword "layout"
  symbol "["
  params <- parseModedLayoutVar `sepBy1` symbol ","
  symbol "]"
  symbol "("
  adt <- parseAdtName
  symbol ")"
  pure (params, adt)


parseLayoutBranch :: String -> Parser (LayoutBranch Expr)
parseLayoutBranch layoutName = lexeme $ try $ do
  nameHere <- parseLayoutName
  parserGuard (nameHere == layoutName) (Just nameHere) layoutName

  pat <- parsePattern
  symbol ":="
  existVars <- fromMaybe [] <$> optional parseExists
  body <- parseLayoutBody
  symbol ";"

  pure $ LayoutBranch (PatternMatch (bind pat (bind existVars body)))

parseExists :: Parser [Exists Expr]
parseExists = label "existential quantifier" $ lexeme $ do
  keyword "exists"
  vars <- parseModedLayoutVar `sepBy1` symbol ","
  symbol "."
  pure $ map Exists vars

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
  try (fmap ((:+ 0) . V) parseExprName)
  where
    go = do
      symbol "("
      v <- V <$> parseExprName
      symbol "+"
      i <- read <$> some digitChar
      symbol ")"
      pure (v :+ i)

parseLayoutApply :: Parser (LayoutHeaplet Expr)
parseLayoutApply = do
  layoutName <- parseLayoutName
  n <- parseExprName
  LApply layoutName (V n) <$> some parseLayoutArg

parseLayoutArg :: Parser Expr
parseLayoutArg = label "layout argument" $ lexeme $
  symbol "[" *> fmap V parseLayoutVar <* symbol "]"

parseModedLayoutVar :: Parser (ModedName Expr)
parseModedLayoutVar =
  Moded <$> parseMode <*> parseLayoutVar

parseMode :: Parser Mode
parseMode = (char '+' $> In) <|> (char '-' $> Out)

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

--
-- Property tests --
--

instance Arbitrary PikaModule where
  arbitrary = error "Arbitrary PikaModule"
  shrink mod0@(PikaModule x y z) = do
    let x' = map shrink x
        y' = map shrink y
        z' = map shrink z
    mod <- PikaModule <$> sequenceA x' <*> sequenceA y' <*> sequenceA z'
    pure mod
  -- shrink = filter (not . isTrivialModule) . genericShrink

genModule :: Gen PikaModule
genModule = sized genModule'

genModule' :: Int -> Gen PikaModule
genModule' size = do
  adtCount <- choose (1, 2)
  fnCount <- choose (1, 2)
  layoutCount <- choose (1, 2)

  let dividedSize = size `div` (adtCount + fnCount + layoutCount)
      -- TODO: This should probably be weighted more towards generating
      -- functions

  adtSigs <- replicateM adtCount genAdtSig
              `suchThat` (\xs ->
                            noDups (map fst xs) -- ADT names are unique
                              &&
                            noDups (map fst (concatMap snd xs))) -- Constructor names are unique
  lNames <- replicateM layoutCount genLayoutName `suchThat` noDups
  layouts <- mapM (\lName -> genLayout @Expr lName adtSigs dividedSize) lNames

  let layoutSigs =
        map (convertSig adtSigs) layouts

  -- let layoutSigs =
  --       zipWith convertAdtSig
  --         (map (string2Name . _layoutName) layouts)
  --         adtSigs
  fnSigs <- replicateM fnCount (genFnSig layoutSigs)
              `suchThat` (noDups . map (\(x, _, _) -> x))

  fns <- mapM (genFnDef fnSigs layoutSigs dividedSize) fnSigs

  pure $ PikaModule
    { moduleLayouts = layouts
    , moduleFnDefs = fns
    , moduleGenerates = map fnDefName fns
    }
  where
    convertSig ::
      [(AdtName, [(String, [AdtArg])])] ->
      Layout Expr ->
      (LayoutName, [(String, [Maybe LayoutName])])
    convertSig adtSigs layout =
      let Just constructors = lookup (_layoutAdt layout) adtSigs
      in
      (string2Name $ _layoutName layout, map go constructors)
      where
        go :: (String, [AdtArg]) -> (String, [Maybe LayoutName])
        go (cName, args) = (cName, map goArg args)

        goArg BaseArg = Nothing
        goArg RecArg = Just $ string2Name $ _layoutName layout

    -- convertAdtSig ::
    --   LayoutName ->
    --   (AdtName, [(String, [AdtArg])]) ->
    --   (LayoutName, [(String, [Maybe LayoutName])])
    -- convertAdtSig lName (_, args) =
    --   (lName, map (go lName) args)
    --
    -- go ::
    --   LayoutName ->
    --   (String, [AdtArg]) ->
    --   (String, [Maybe LayoutName])
    -- go lName (cName, xs) = (cName, map (go' lName) xs)
    --
    -- go' lName BaseArg = Nothing
    -- go' lName RecArg = Just lName
    --

