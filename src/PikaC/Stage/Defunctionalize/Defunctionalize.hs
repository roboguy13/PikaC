{-# OPTIONS_GHC -Wincomplete-patterns #-}

module PikaC.Stage.Defunctionalize.Defunctionalize
  (defunctionalizeModule)
  where

import PikaC.Syntax.Pika.Expr
import PikaC.Syntax.Pika.FnDef
import PikaC.Syntax.Pika.Parser
import PikaC.Syntax.Pika.Pattern
import PikaC.Syntax.Type
import PikaC.Ppr
import PikaC.Utils

import Data.Maybe
import Data.List

import Unbound.Generics.LocallyNameless

import Control.Lens

import Debug.Trace

defunctionalizeModule :: PikaModuleElaborated -> PikaModuleElaborated
defunctionalizeModule mod = runFreshM $ do
  let fnDefs = moduleFnDefs mod

  specializations :: [[Specialization]]
      <- nub <$>
         traverse (getSpecializations mod . fnDefName) fnDefs

  let defsSpecializations :: [(FnDef, [Specialization])]
      defsSpecializations = zip fnDefs specializations

  pure $ mod { moduleFnDefs = concatMap (uncurry defunctionalizeFnDef) defsSpecializations }

-- | The (concrete) function arguments that a function is called with at
-- a particular call-site
newtype Specialization = Specialization [FnArg]
  deriving (Show, Eq)

data FnArg
  = FnArgName String
  | FnArgLambda String -- An name used internally for the lambda
                (Bind [ExprName] Expr)
  deriving (Show)

getFnArgName :: FnArg -> String
getFnArgName (FnArgName x) = x
getFnArgName (FnArgLambda x _) = x

-- NOTE: This compares the internal names of lambdas
instance Eq FnArg where
  FnArgName x     == FnArgName y     = x == y
  FnArgLambda x _ == FnArgLambda y _ = x == y
  _ == _ = False

updateToUseSpecializations :: PikaModuleElaborated -> PikaModuleElaborated
updateToUseSpecializations mod =
  mod { moduleFnDefs = map go (moduleFnDefs mod) }
  where
    go = runFreshM . onFnDef (pure . goExpr)
      where
        goExpr :: Expr -> Expr
        goExpr app@(App fn args) =
          case lookupFnDef mod (name2String fn) of
            Nothing -> app
            Just fnDef ->
              let fnType = getFnDefType fnDef
              in
              case findSpecialization fnType args of
                Nothing -> app
                Just specialization -> useSpecialization fnType (name2String fn) args specialization
        goExpr e = e

useSpecialization :: Type -> String -> [Expr] -> Specialization -> Expr
useSpecialization ty name origArgs specialization =
  let newArgs = dropFnTyped ty origArgs
  in
  App (string2Name (getDefunName name specialization)) newArgs

-- | Get the function arguments that the given function name is called with
-- in the module
getSpecializations :: PikaModuleElaborated -> String -> FreshM [Specialization]
getSpecializations mod fnName =
  concat <$> traverse (getSpecializationsFromDef fnName) (moduleFnDefs mod)

-- |
-- @fnName@: The function name we are searching for
--
-- @fnDef@: The function being searched
getSpecializationsFromDef :: String -> FnDef -> FreshM [Specialization]
getSpecializationsFromDef fnName fnDef = do
  exprs <- getFnDefExprs fnDef
  let apps = mapMaybe getCandidateApp $ concatMap universe exprs
      ty = getFnDefType fnDef
      specs = mapMaybe (findSpecialization (getFnDefType fnDef)) apps

  pure -- $ trace ("name = " ++ fnDefName fnDef ++ "; type = " ++ ppr' (getFnDefType fnDef) ++ "; specs = " ++ show specs ++ "; def = " ++ ppr' fnDef)
    $ specs
  where
    getCandidateApp :: Expr -> Maybe [Expr]
    getCandidateApp (App f args) = Just args
    getCandidateApp _ = Nothing

-- | We find an @App "fnName" [arg1, ..., argN]@
-- and we want to get the specialization that this represents.
findSpecialization :: Type -> [Expr] -> Maybe Specialization
findSpecialization ty exprs =
  case map toFnArg $ getFnTyped ty exprs of
    [] -> Nothing
    fnArgs -> Just $ Specialization fnArgs

toFnArg :: Expr -> FnArg
toFnArg (V x) = FnArgName (name2String x)
toFnArg e = error $ "toFnArg: " ++ ppr' e

freshLambdaName :: FreshM String
freshLambdaName = do
  x <- fresh (s2n "_lambda_") :: FreshM ExprName
  pure $ name2String x

defunctionalizeFnDef :: FnDef -> [Specialization] -> [FnDef]
defunctionalizeFnDef fnDef [] = [fnDef]
defunctionalizeFnDef fnDef xs = map (generateSpecialization fnDef) xs

-- | This is where we actually do the main work of defunctionalization
generateSpecialization :: FnDef -> Specialization -> FnDef
generateSpecialization fnDef spec =
  let newName = getDefunName (fnDefName fnDef) spec
  in
  fnDef
    { fnDefName = newName
    , fnDefTypedBranches = go (fnDefTypedBranches fnDef)
    }
  where
    go (Typed ty bs) =
      Typed ty $ map (generateBranchSpecialization ty spec) bs

generateBranchSpecialization :: Type -> Specialization -> FnDefBranch -> FnDefBranch
generateBranchSpecialization ty spec branch = runFreshM $ do
  stringSubst <- makeSpecializationSubst ty branch spec

  let
    nameSubst :: [(ExprName, ExprName)]
    nameSubst = map (both %~ string2Name) stringSubst

    -- -- TODO: Support for lambdas
    -- go :: Expr -> Expr
    -- go e@(App f args) =
    --   case lookup f stringSubst of
    --     Nothing -> e
    --     Just f' -> App f args
    -- go e@(V _) = rename nameSubst e
    -- go e = e

  onFnDefBranch (pure . rename nameSubst) branch

makeSpecializationSubst :: Type -> FnDefBranch -> Specialization -> FreshM [(String, String)]
makeSpecializationSubst ty branch (Specialization fnArgs) = do
  params <- getFnTypedParams ty branch
  pure $ zip (map name2String params)
    $ map getFnArgName fnArgs -- TODO: Support for lambdas

getFnTypedParams :: Type -> FnDefBranch -> FreshM [ExprName]
getFnTypedParams ty (FnDefBranch (PatternMatches bnd)) = do
  (pats0, _) <- unbind bnd
  pure $ map getPatternVar (getFnTyped ty pats0)
  where
    getPatternVar (PatternVar x) = x
    getPatternVar pat = error $ "getPatternVar: " ++ ppr' pat

getFnArgsFromMatches :: Type -> PatternMatches Expr GuardedExpr -> FreshM [ExprName]
getFnArgsFromMatches ty (PatternMatches bnd) = do
  (pats, rhs) <- unbind bnd

  pure $ map getPatternVar $ dropFnTyped ty pats
  where
    getPatternVar (PatternVar n) = n
    getPatternVar _ = error $ "getFnArgsFromMatches.getPatternVar: Not a PatternVar"

-- Remove the function parameters from the pattern matches
updateMatches :: Type -> PatternMatches Expr GuardedExpr -> FreshM (PatternMatches Expr GuardedExpr)
updateMatches ty (PatternMatches bnd) = do
  (pats, rhs) <- unbind bnd

  pure $ PatternMatches $ bind (dropFnTyped ty pats) rhs

-- | Drop elements of the list corresponding to function types
dropFnTyped :: Type -> [a] -> [a]
dropFnTyped = onTyped go
  where
    go (FnType {}) _x = Nothing
    go _            x = Just x

-- | Get elements of the list corresponding to function types
getFnTyped :: Type -> [a] -> [a]
getFnTyped = onTyped go
  where
    go (FnType {}) x = Just x
    go _          _x = Nothing

-- -- | Use elements of the first list on function types and elements of the
-- -- second list on non-function types
-- conditionOnFnTyped :: Type -> [a] -> [a] -> [a]
-- conditionOnFnTyped ty xs0 ys0 =
--   let (argTys, _) = splitFnType ty
--   in
--   go argTys xs0 ys0
--   where
--     go (FnType {} : argTys) (x:xs) ys     = x : go argTys xs ys
--     go (_         : argTys) xs     (y:ys) = y : go argTys xs ys

-- | Apply to elements of the list along with corresponding argument types
onTyped :: (Type -> a -> Maybe b) -> Type -> [a] -> [b]
onTyped f ty xs =
  let (argTys, _) = splitFnType ty
  in
  catMaybes (zipWith f argTys xs)


getDefunType :: Type -> Type
getDefunType (FnType (FnType {}) b) = getDefunType b
getDefunType (FnType a b) = FnType a $ getDefunType b
getDefunType a = a

getDefunName :: String -> Specialization -> String
getDefunName baseName (Specialization fnArgs) =
  getDefunNameString baseName (map getFnArgName fnArgs)

getDefunNameString :: String -> [String] -> String
getDefunNameString baseName fnArgNames =
  concat $ baseName : map ('_':) fnArgNames

getFnDefType :: FnDef -> Type
getFnDefType fnDef =
  let Typed ty _ = fnDefTypedBranches fnDef
  in
  ty

