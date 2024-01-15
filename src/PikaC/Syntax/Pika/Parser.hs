{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}

module PikaC.Syntax.Pika.Parser
  where

import PikaC.Syntax.ParserUtils

import PikaC.Syntax.Pika.Expr
import PikaC.Syntax.Pika.Pattern
import PikaC.Syntax.Pika.FnDef
import PikaC.Syntax.Pika.Layout
import PikaC.Syntax.Heaplet
import qualified PikaC.Syntax.PikaCore.Expr as PikaCore

import PikaC.Syntax.Type.Parser hiding (parseGhostArg)
import PikaC.Syntax.Type

import PikaC.TypeChecker.Mode

import PikaC.Utils
import PikaC.Ppr hiding (char)

import Text.Megaparsec hiding (parseTest)
import Text.Megaparsec.Char

import Control.Applicative hiding (some, many)
import Data.Functor
import Data.Maybe

import Data.Either
import Data.Semigroup
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty

import Unbound.Generics.LocallyNameless (string2Name, bind, Embed (..))
import Unbound.Generics.LocallyNameless.Unsafe

import Test.QuickCheck hiding (label)

import Control.Monad
import GHC.Generics hiding (Constructor)

import Debug.Trace

import Control.DeepSeq

import Data.Validity

data PikaModule' f =
  PikaModule
  { moduleAdts :: [Adt]
  , moduleLayouts :: [Layout Expr]
  , moduleFnDefs :: [FnDef' f]
  , moduleSynths :: [Synth]
  , moduleGenerates :: [String]
  , moduleTests :: [Test Expr]
  }
  deriving (Generic)

deriving instance Show (f [FnDefBranch]) => Show (PikaModule' f)

instance Size (FnDef' f) => Size (PikaModule' f) where
  size PikaModule { .. } =
    sizeList moduleAdts + size moduleLayouts + sizeList moduleFnDefs + size moduleSynths + size moduleGenerates + size moduleTests

type PikaModule = PikaModule' TypeSig'
type PikaModuleElaborated = PikaModule' Typed

toPikaModuleElaborated_unsafe :: PikaModule -> PikaModuleElaborated
toPikaModuleElaborated_unsafe pikaModule =
  pikaModule { moduleFnDefs = map (overTypedBranches fromTypeSig_unsafe) (moduleFnDefs pikaModule) }

instance NFData (f [FnDefBranch]) => NFData (PikaModule' f)

instance (TypePair f, Ppr (TypePairType f)) => Ppr (PikaModule' f) where
  ppr (PikaModule adts layouts fns synths generates tests) =
    text "-- Layouts:"
    $$ vcat (map ppr layouts)
    $$ text "-- Synths:"
    $$ vcat (map ppr synths)
    $$ text "-- Fn defs:"
    $$ vcat (map ppr fns)
    $$ text "-- Tests:"
    $$ vcat (map ppr tests)

isTrivialModule :: PikaModule -> Bool
isTrivialModule (PikaModule x y z w a b) =
  null x || null y || null z || null w || null a || null b

instance Semigroup PikaModule where
  PikaModule xs1 ys1 zs1 ws1 as1 bs1 <> PikaModule xs2 ys2 zs2 ws2 as2 bs2 =
    PikaModule (xs1 <> xs2) (ys1 <> ys2) (zs1 <> zs2) (ws1 <> ws2) (as1 <> as2) (bs1 <> bs2)

instance Monoid PikaModule where
  mempty = PikaModule mempty mempty mempty mempty mempty mempty

singleAdt :: Adt -> PikaModule
singleAdt x = mempty { moduleAdts = [x] }

singleLayout :: Layout Expr -> PikaModule
singleLayout x = mempty { moduleLayouts = [x] }

singleFnDef :: FnDef' TypeSig' -> PikaModule' TypeSig'
singleFnDef x = (mempty :: PikaModule' TypeSig') { moduleFnDefs = [x] }

singleSynth :: Synth -> PikaModule
singleSynth x = mempty { moduleSynths = [x] }

singleGenerate :: String -> PikaModule
singleGenerate x = mempty { moduleGenerates = [x] }

singleTest :: Test Expr -> PikaModule
singleTest x = mempty { moduleTests = [x] }

moduleLookupLayout :: PikaModule' a -> String -> Layout Expr
moduleLookupLayout = lookupLayout . moduleLayouts

moduleLookupFn :: PikaModule' a -> String -> FnDef' a
moduleLookupFn pikaModule name = go (moduleFnDefs pikaModule)
  where
    go [] = error $ "moduleLookupFn: Cannot find function named " ++ show name
    go (x:xs)
      | fnDefName x == name = x
      | otherwise           = go xs

parsePikaModule :: Parser PikaModule
parsePikaModule = do
  generates <- mconcat . map singleGenerate <$> some parseGenerate
  adts <- mconcat . map singleAdt <$> many parseAdt
  layouts <- mconcat . map singleLayout <$> many parseLayout
  synths <- mconcat . map singleSynth <$> many parseSynth
  fnDefs <- mconcat . map singleFnDef <$> some parseFnDef
  tests <- mconcat . map singleTest <$> many parseTest

  pure (generates <> adts <> layouts <> fnDefs <> tests <> synths)

  -- fmap mconcat $ some $
  --   try (singleGenerate <$> parseGenerate) <|>
  --   try (singleLayout <$> parseLayout) <|>
  --   (singleFnDef <$> parseFnDef)

parseTest :: Parser (Test Expr)
parseTest = label "test definition" $ lexeme $ do
  keyword "%test"
  name <- parseString
  ty <- parseType
  symbol ":"
  e <- parseExpr
  symbol ";"
  pure $ Test name e ty
  where
    escape :: Parser String
    escape = do
      string "\\\""
      pure "\\\""

    nonEscape :: Parser Char
    nonEscape = noneOf ("\\\"" :: String)

    strChar :: Parser String
    strChar = fmap pure nonEscape <|> escape

    parseString :: Parser String
    parseString = lexeme $ do
      char '"'
      strs <- many strChar
      char '"'
      pure $ concat strs

parseAdt :: Parser Adt
parseAdt = label "data type definition" $ lexeme $ do
  keyword "data"
  name <- parseAdtName
  symbol ":="
  cs <- parseConstructor name `sepBy1` symbol "|"
  symbol ";"
  pure $ Adt name cs

parseConstructor :: AdtName -> Parser Constructor
parseConstructor adt = do
  name <- parseConstructorName
  tys <- many parseType'

  pure $ mkConstructor name $ mkFnType (tys ++ [LayoutId (unAdtName adt)])
  -- where
  --   recTypeVar :: TypeName
  --   recTypeVar = string2Name "t"
  --
  --   go (LayoutId x)
  --     | x == unAdtName adt = TyVar recTypeVar
  --   go x = x


parseGenerate :: Parser String
parseGenerate = label "generate directive" $ lexeme $ do
  keyword "%generate"
  parseFnName

parseFnDef :: Parser (FnDef' TypeSig')
parseFnDef = label "function definition" $ lexeme $ do
  fnName <- parseFnName
  symbol ":"
  sig <- parseTypeSig $ do
    symbol ";"
    some (parseFnDefBranch fnName)

  pure $ FnDef fnName sig

parseSynth :: Parser Synth
parseSynth = do
  keyword "synth"
  fn <- parseFnName
  symbol ":"
  purePart <- fromMaybe (BoolLit True) <$> optional (parseExpr <* symbol ";;")
  ty <- parseType
  let (argTypes, resultType) = splitFnType ty
  symbol ";"
  pure $ Synth fn purePart argTypes resultType

-- parseSynthSig :: Parser (Type, Type)
-- parseSynthSig = do
--   FnType x y <- parseFnType
--   pure (x, y)

parseFnDefBranch :: String -> Parser FnDefBranch
parseFnDefBranch fnName = label "function branch" $ lexeme $ try $ do
  nameHere <- parseFnName
  parserGuard (nameHere == fnName) (Just nameHere) fnName

  pats <- some parsePattern
  cond <- fromMaybe (BoolLit True) <$> (optional parseGuard)
  symbol ":="
  body <- parseExpr

  symbol ";"

  pure $ FnDefBranch (PatternMatches (bind pats (GuardedExpr cond body)))

parseGuard :: Parser Expr
parseGuard = label "guard" $ lexeme $ do
  symbol "|"
  cond <- parseExpr
  pure cond

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
  try (parseBinOp "%" Mod) <|>
  try (parseBinOp "&&" And) <|>
  try (parseBinOp "/" Div) <|>
  try (parseBinOp "+" Add) <|>
  try (parseBinOp "*" Mul) <|>
  try (parseBinOp "-" Sub) <|>
  try (parseBinOp "==" Equal) <|>
  try (parseBinOp "<" Lt) <|>
  try (parseBinOp "<=" Le) <|>
  try parseSetUnion <|>
  -- try parseGhostExpr'
  try (Not <$> (keyword "not" *> parseExpr')) <|>
  try parseLayoutLambda <|>
  try parseApplyLayout <|>
  try parseApp <|>
  try parseIfThenElse <|>
  try parseExpr'

parseExpr' :: Parser Expr
parseExpr' = label "expression" $ lexeme $
  try (symbol "(" *> parseExpr <* symbol ")") <|>
  try (IntLit <$> parseInt) <|>
  try (keyword "False" $> BoolLit False) <|>
  try (keyword "True" $> BoolLit True) <|>
  try (V <$> parseLowercaseExprName) <|>
  try (lexeme (symbol "{" *> symbol "}" *> pure EmptySet)) <|>
  try (lexeme (fmap SingletonSet (symbol "{" *> parseExpr <* symbol "}")))

parseLayoutLambda :: Parser Expr
parseLayoutLambda = label "layout lambda" $ lexeme $ do
  symbol "/\\"
  (ty :~ adt) <- parseLayoutConstraint
  symbol "."
  LayoutLambda adt . bind ty <$> parseExpr

parseApplyLayout :: Parser Expr
parseApplyLayout = label "layout application" $ lexeme $ do
  e <- try parseApp <|> parseExpr' <|> parseNullaryConstructorApp
  -- e <- parseExpr' <|> parseNullaryConstructorApp
  symbol "["
  ty <- parseType
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
  (ghosts, params, adt) <- parseLayoutSig
  keyword ";"
  branches <- some (parseLayoutBranch layoutName)
  pure $ Layout layoutName adt (bind ghosts (bind params branches))

parseGhostDecl :: Parser (Ghost Expr)
parseGhostDecl = do
  symbol "@"
  symbol "("
  g <- parseLowercaseExprName
  symbol ":"
  t <- parseGhostType
  symbol ")"
  pure $ Ghost t g

parseGhostType :: Parser GhostType
parseGhostType =
  (keyword "set" $> SetGhost) <|> (keyword "int" $> IntGhost)

parseLayoutSig :: Parser ([Ghost Expr], [ModedName Expr], AdtName)
parseLayoutSig = lexeme $ do
  ghosts <- many parseGhostDecl
  keyword "layout"
  symbol "["
  params <- parseModedLayoutVar `sepBy1` symbol ","
  symbol "]"
  symbol "("
  adt <- parseAdtName
  symbol ")"
  pure (ghosts, params, adt)


parseLayoutBranch :: String -> Parser (LayoutBranch Expr)
parseLayoutBranch layoutName = lexeme $ try $ do
  nameHere <- parseLayoutName
  parserGuard (nameHere == layoutName) (Just nameHere) layoutName

  pat <- parsePattern
  symbol ":="
  existVars <- fromMaybe [] <$> optional parseExists
  body <- parseCondLayoutBody
  symbol ";"

  pure $ LayoutBranch (PatternMatch (bind pat (bind existVars body)))

parseCondLayoutBody :: Parser (GhostCondition Expr (LayoutBody Expr))
parseCondLayoutBody = label "ghost conditioned layout body" $ lexeme $ do
  cond <- optional (try (parseExpr <* symbol ";;"))
  body <- parseLayoutBody
  pure (GhostCondition cond body)

-- parseGhostExpr' :: Parser Expr
-- parseGhostExpr' = lexeme $
--   try parseExpr <|>
--   try (lexeme (symbol "{" *> symbol "}" *> pure EmptySet)) <|>
--   try (lexeme (fmap SingletonSet (symbol "{" *> parseGhostExpr <* symbol "}"))) <|>
--   try (symbol "(" *> parseGhostExpr <* symbol ")")

-- parseGhostExpr :: Parser Expr
-- parseGhostExpr = label "ghost expression" $ lexeme $
--   where
-- parseEq = lexeme $ do
--   x <- parseExpr'
--   symbol "=="
--   y <- parseExpr
--   pure $ Equal x y

parseSetUnion = lexeme $ do
  x <- parseExpr'
  symbol "++"
  y <- parseExpr
  pure $ SetUnion x y

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
      i <- parseInt
      symbol ")"
      pure (v :+ i)

parseIfThenElse :: Parser Expr
parseIfThenElse = label "if-then-else" $ lexeme $ do
  keyword "if"
  x <- parseExpr
  keyword "then"
  y <- parseExpr
  keyword "else"
  z <- parseExpr
  pure $ IfThenElse x y z

parseLayoutApply :: Parser (LayoutHeaplet Expr)
parseLayoutApply = label "layout application" $ lexeme $ do
  layoutName <- parseLayoutName
  n <- parseExprName
  ghosts <- many parseGhostArg
  args <- some parseLayoutArg
  pure $ LApply layoutName ghosts (V n) args

parseGhostArg :: Parser Expr
parseGhostArg = label "ghost argument" $ lexeme $ do
  char '@'
  parseExpr'

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

-- parseConstructorName :: Parser String
-- parseConstructorName = label "constructor name" $ lexeme parseUppercaseName

parseFnName :: Parser String
parseFnName = label "function name" $ lexeme (parseLowercaseName <|> parseUppercaseName)

parseLayoutName :: Parser String
parseLayoutName = label "layout name" $ lexeme parseUppercaseName

parsePatternVar :: Parser ExprName
parsePatternVar = label "pattern variable" $ string2Name <$> lexeme parseLowercaseName

--
-- Property tests --
--

instance (TypePair f) => Arbitrary (PikaModule' f) where
  arbitrary = error "Arbitrary PikaModule"
  shrink mod0@(PikaModule x y z w a b) = do
    let y' = map shrink y
        z' = map shrink z
    mod <- PikaModule x <$> sequenceA y' <*> sequenceA z' <*> pure w <*> pure a <*> pure b
    pure mod
  -- shrink = filter (not . isTrivialModule) . genericShrink

instance Validity PikaModule where
  validate (PikaModule x y z w _ _) =
    validate y <> validate z

instance Validity (TypeSig' [FnDefBranch]) where
  validate (TypeSig bnd) =
    let (_, (_, branches)) = unsafeUnbind bnd
    in
    validate branches

instance Validity (Typed [FnDefBranch]) where
  validate (Typed _ branches) =
    validate branches

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
  layouts <-
    mapM (\lName -> genLayout @Expr lName adtSigs dividedSize) lNames
      `suchThat` (\(y : ys) -> isRight (sconcat (NonEmpty.map (modeCheck (y : ys)) (y :| ys))))

  let layoutSigs =
        map (convertSig adtSigs) layouts

  -- let layoutSigs =
  --       zipWith convertAdtSig
  --         (map (string2Name . _layoutName) layouts)
  --         adtSigs
  fnSigs <- replicateM fnCount (genFnSig layoutSigs)
              `suchThat` (noDups . map (\(x, _, _) -> x))

  fns <- mapM (genFnDef fnSigs layoutSigs dividedSize) fnSigs
          `suchThat` (and . map isValid)

  pure $ PikaModule
    { moduleAdts = [] -- TODO: Generate these
    , moduleLayouts = layouts
    , moduleFnDefs = map (overTypedBranches toTypeSig) fns
    , moduleGenerates = map fnDefName fns
    , moduleSynths = []
    , moduleTests = []
    }
  where
    convertSig ::
      [(AdtName, [(String, [AdtArg])])] ->
      Layout Expr ->
      (LayoutName, [(String, [Maybe LayoutName])])
    convertSig adtSigs layout =
      let Just constructors = lookup (_layoutAdt layout) adtSigs
      in
      (_layoutName layout, map go constructors)
      where
        go :: (String, [AdtArg]) -> (String, [Maybe LayoutName])
        go (cName, args) = (cName, map goArg args)

        goArg BaseArg = Nothing
        goArg RecArg = Just $ _layoutName layout

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

