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
import Unbound.Generics.LocallyNameless.Unsafe
import Unbound.Generics.LocallyNameless.Bind
import Unbound.Generics.LocallyNameless.Name

import Control.Lens

import Debug.Trace

defunctionalizeModule :: PikaModuleElaborated -> PikaModuleElaborated
defunctionalizeModule mod = runFreshM $ do
  let fnDefs = moduleFnDefs mod
      higherOrderFns = filter (isHigherOrder . getFnDefType) fnDefs

  specializations :: [Specialization]
      <-  nub . (++ directivesToSpecializations mod) . concat <$>
         traverse (getSpecializations higherOrderFns) fnDefs

  let specializedDefs = map defunctionalizeFnDef specializations
      newMod =
        deleteOldDefs specializations $
          mod { moduleFnDefs = map (updateWithSpecialization mod) (fnDefs ++ specializedDefs), moduleGenerates = extendGenerates specializations (moduleGenerates mod) }

  pure -- $ trace ("higher order fns = " ++ show higherOrderFns)
    -- $ trace ("specializations = " ++ show specializations)
    $ trace ("higher order fn = " ++ show (head higherOrderFns))
    $ trace (ppr' newMod)
    $ newMod

deleteOldDefs :: [Specialization] -> PikaModuleElaborated -> PikaModuleElaborated
deleteOldDefs specs mod =
  mod
    { moduleFnDefs = foldr go (moduleFnDefs mod) specs
    }
  where
    go spec defs =
      deleteBy check (getSpecializationFnDef spec) defs

    check x y = fnDefName x == fnDefName y

extendGenerates :: [Specialization] -> [String] -> [String]
extendGenerates specs gens = map getDefunName specs <> gens

updateWithSpecialization :: PikaModuleElaborated -> FnDef -> FnDef
updateWithSpecialization mod =
    runFreshM . onFnDef (pure . transform goExpr)
  where
    goExpr :: Expr -> Expr
    goExpr e0@(App (V f) args) = fromMaybe e0 $ do
      fnDef <- lookupFnDef mod (name2String f)
      let ty = getFnDefType fnDef
      if isHigherOrder ty
      then
        let newArgs = getFnTyped ty args
            spec = runFreshM $ makeSpecialization fnDef newArgs -- TODO: Is it correct to runFreshM here?
            newName = getDefunName spec
        in
        Just $ App (V (string2Name newName)) newArgs
      else Nothing
    goExpr e0 = e0

-- | The (concrete) function arguments that a function is called with at
-- a particular call-site
data Specialization = Specialization FnDef [FnArg]
  deriving (Show)

directivesToSpecializations :: PikaModuleElaborated -> [Specialization]
directivesToSpecializations mod = map go directives
  where
    directives = moduleSpecializes mod

    go (name, args) = runFreshM $ do
      let Just fnDef = lookupFnDef mod name
          ty = getFnDefType fnDef
          fnTypeArgs = getFnTyped ty args

      makeSpecialization fnDef fnTypeArgs

instance Eq Specialization where
  Specialization fnDef fnArgs == Specialization fnDef' fnArgs' =
    fnDefName fnDef == fnDefName fnDef' && fnArgs == fnArgs'

data FnArg
  = FnArgName String
  | FnArgLambda String -- An name used internally for the lambda
                (Bind [ExprName] Expr)
  deriving (Show)

getFnArgName :: FnArg -> String
getFnArgName (FnArgName x) = x
getFnArgName (FnArgLambda x _) = x

setFnArgName :: FnArg -> String -> FnArg
setFnArgName (FnArgName _) x = FnArgName x
setFnArgName (FnArgLambda _ y) x = FnArgLambda x y

getSpecializationFnDef :: Specialization -> FnDef
getSpecializationFnDef (Specialization x _) = x

getSpecializations :: [FnDef] -> FnDef -> FreshM [Specialization]
getSpecializations higherOrderFns currFnDef = do
  exprs <- concat . universe <$> getFnDefExprs currFnDef
  catMaybes <$> traverse (getSpecializationFromExpr higherOrderFns) exprs

getSpecializationFromExpr :: [FnDef] -> Expr -> FreshM (Maybe Specialization)
getSpecializationFromExpr higherOrderFns (App (V f) args)
  | not (isFreeName f) = pure Nothing

getSpecializationFromExpr higherOrderFns (App (V f) args) = do
  case find ((== name2String f) . fnDefName) higherOrderFns of
    Nothing -> pure Nothing
    Just higherOrderFn ->
      Just <$> makeSpecialization higherOrderFn (getFnTyped (getFnDefType higherOrderFn) args)

getSpecializationFromExpr _ _ = pure Nothing

defunctionalizeFnDef :: Specialization -> FnDef
defunctionalizeFnDef spec@(Specialization fnDef0@(FnDef name (Typed ty branches)) fnArgs) =
  let (argTys, resTy) = splitFnType (getFnDefType fnDef0)
      newArgTys = dropFnTyped (getFnDefType fnDef0) argTys

      newTy = mkFnType (newArgTys ++ [resTy])

      newBranches = runFreshM $ traverse (defunctionalizeBranch ty fnArgs) branches
      newName = getDefunName spec
      newFnDef = FnDef newName (Typed newTy newBranches)
  in
  trace ("orig = " ++ ppr' fnDef0) $ trace ("newFnDef = " ++ show newFnDef) newFnDef

-- renameFnArgs :: Type -> FnDefBranch -> [FnArg] -> [FnArg]
-- renameFnArgs ty branch fnArgs =
--   let fnTypedParams = runFreshM $ getFnTypedParams ty branch
--   in
--   zipWith setFnArgName fnArgs $ map name2String fnTypedParams

defunctionalizeBranch :: Type -> [FnArg] -> FnDefBranch -> FreshM FnDefBranch
defunctionalizeBranch ty fnArgs branch = do
  -- TODO: Rename fnArgs based on the branch names
  specSubst <- makeSpecializationSubst ty branch fnArgs
  updatePatterns specSubst ty branch

updatePatterns :: SpecSubst -> Type -> FnDefBranch -> FreshM FnDefBranch
updatePatterns specSubst ty (FnDefBranch (PatternMatches bnd)) = do
  (pats, body) <- unbind bnd
  -- let (pats, body) = unsafeUnbind bnd
  let newPats = dropFnTyped ty pats
      afterSubst = substs specSubst body
      -- patVars = map (map mkVar . getNames) newPats
      -- new = concat (conditionOnFnTyped ty (map ((:[]) . snd) specSubst) patVars)
  pure $ trace ("bnd = " ++ show bnd ++ "; body = " ++ ppr' body ++ "; afterSubst = " ++ show afterSubst)
    $ trace ("specSubst = " ++ show specSubst)
    $ FnDefBranch $ PatternMatches $ bind newPats afterSubst
    -- $ FnDefBranch $ PatternMatches $ bind newPats $ substBvs initialCtx new body

makeSpecialization :: FnDef -> [Expr] -> FreshM Specialization
makeSpecialization fnDef =
  fmap (Specialization fnDef) . traverse specializeArg

specializeArg :: Expr -> FreshM FnArg
specializeArg (V x)
  | isFreeName x = pure $ FnArgName (name2String x)
specializeArg (Lambda bnd) = do
  lamName <- freshLambdaName
  pure $ FnArgLambda lamName bnd
specializeArg e = error $ "specializeArg: " ++ ppr' e

-- NOTE: This compares the internal names of lambdas
instance Eq FnArg where
  FnArgName x     == FnArgName y     = x == y
  FnArgLambda x _ == FnArgLambda y _ = x == y
  _ == _ = False

type SpecSubst = [(ExprName, Expr)]

makeSpecializationSubst :: Type -> FnDefBranch -> [FnArg] -> FreshM SpecSubst
makeSpecializationSubst ty branch fnArgs = do
  params <- getFnTypedParams ty branch
  pure $ zip params
    $ map (V . string2Name . getFnArgName) fnArgs -- TODO: Support for lambdas

getFnTypedParams :: Type -> FnDefBranch -> FreshM [ExprName]
getFnTypedParams ty (FnDefBranch (PatternMatches bnd)) = do
  -- (pats0, _) <- unbind bnd
  let B pats0 _ = bnd
  pure $ map getPatternVar (getFnTyped ty pats0)
  where
    getPatternVar (PatternVar x) = x --Fn (name2String x) 0
    getPatternVar pat = error $ "getPatternVar: " ++ ppr' pat

getFnArgsFromMatches :: Type -> PatternMatches Expr GuardedExpr -> FreshM [ExprName]
getFnArgsFromMatches ty (PatternMatches bnd) = do
  (pats, rhs) <- unbind bnd

  pure $ map getPatternVar $ dropFnTyped ty pats
  where
    getPatternVar (PatternVar n) = n
    getPatternVar _ = error $ "getFnArgsFromMatches.getPatternVar: Not a PatternVar"

isHigherOrder :: Type -> Bool
isHigherOrder ty = not . null . getFnTyped ty $ repeat ()

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

-- | Use elements of the first list on function types and elements of the
-- second list on non-function types
conditionOnFnTyped :: Type -> [a] -> [a] -> [a]
conditionOnFnTyped ty xs0 ys0 =
  let (argTys, _) = splitFnType ty
  in
  go argTys xs0 ys0
  where
    go (FnType {} : argTys) (x:xs) ys     = x : go argTys xs ys
    go (_         : argTys) xs     (y:ys) = y : go argTys xs ys
    go []                   []     []     = []

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

getDefunName :: Specialization -> String
getDefunName (Specialization fnDef fnArgs) =
  let baseName = fnDefName fnDef
  in
  getDefunNameString baseName (map getFnArgName fnArgs)

getDefunNameString :: String -> [String] -> String
getDefunNameString baseName fnArgNames =
  concat $ baseName : map ('_':) fnArgNames

getFnDefType :: FnDef -> Type
getFnDefType fnDef =
  let Typed ty _ = fnDefTypedBranches fnDef
  in
  ty

freshLambdaName :: FreshM String
freshLambdaName = do
  x <- fresh (s2n "_lambda_") :: FreshM ExprName
  pure $ name2String x

