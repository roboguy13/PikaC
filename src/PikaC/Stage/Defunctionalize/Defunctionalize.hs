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
extendGenerates specs gens = map (fnDefName . getSpecializationFnDef) specs <> gens
-- extendGenerates specs = concatMap go
--   where
--     go origGenerate =
--       case map getDefunName (filter ((== origGenerate) . fnDefName . getSpecializationFnDef) specs) of
--         [] -> [origGenerate]
--         xs -> xs

updateWithSpecialization :: PikaModuleElaborated -> FnDef -> FnDef
updateWithSpecialization mod =
    runFreshM . onFnDef (pure . goExpr)
  where
    goExpr :: Expr -> Expr
    goExpr e0@(App f args) = fromMaybe e0 $ do
      fnDef <- lookupFnDef mod (name2String f)
      let ty = getFnDefType fnDef
      if isHigherOrder ty
      then
        let newArgs = getFnTyped ty args
            spec = runFreshM $ makeSpecialization fnDef newArgs -- TODO: Is it correct to runFreshM here?
            newName = getDefunName spec
        in
        Just $ App (string2Name newName) newArgs
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

getSpecializationFnDef :: Specialization -> FnDef
getSpecializationFnDef (Specialization x _) = x

getSpecializations :: [FnDef] -> FnDef -> FreshM [Specialization]
getSpecializations higherOrderFns currFnDef = do
  exprs <- concat . universe <$> getFnDefExprs currFnDef
  catMaybes <$> traverse (getSpecializationFromExpr higherOrderFns) exprs

getSpecializationFromExpr :: [FnDef] -> Expr -> FreshM (Maybe Specialization)
getSpecializationFromExpr higherOrderFns (App f args)
  | not (isFreeName f) = pure Nothing

getSpecializationFromExpr higherOrderFns (App f args) = do
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
  in
  FnDef newName (Typed newTy newBranches)

defunctionalizeBranch :: Type -> [FnArg] -> FnDefBranch -> FreshM FnDefBranch
defunctionalizeBranch ty fnArgs branch = do
  specSubst <- makeSpecializationSubst ty branch fnArgs
  updatePatterns specSubst ty branch

updatePatterns :: SpecSubst -> Type -> FnDefBranch -> FreshM FnDefBranch
updatePatterns specSubst ty (FnDefBranch (PatternMatches bnd)) = do
  (pats, body) <- unbind bnd
  let newPats = dropFnTyped ty pats
  pure $ FnDefBranch $ PatternMatches $ bind newPats (instantiate bnd (concat (conditionOnFnTyped ty (map ((:[]) . snd) specSubst) (map (map mkVar . getNames) newPats))))
  -- let afterSubst = overGuardedExpr renameIt body
  --     afterBind = bind (dropFnTyped ty pats) afterSubst
  -- pure $ trace ("subst = " ++ show specSubst ++ "; beforeSubst = " ++ show body ++ ";;;; afterSubst = " ++ show afterSubst ++ "; afterBind = " ++ show afterBind) $ traceShow specSubst $ FnDefBranch $ PatternMatches afterBind
  -- where
  --   renameIt = transform go
  --     where
  --       go e0@(V x) = fromMaybe e0 $ do
  --         y <- lookup (name2String x) specSubst
  --         pure $ trace ("--- y = " ++ show y) $ V $ Fn y 0
  --       go e = e

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

-- getSpecializations :: PikaModuleElaborated -> FnDef -> FreshM [Specialization]
-- getSpecializations = undefined
--
-- defunctionalizeFnDef = undefined



-- NOTE: This compares the internal names of lambdas
instance Eq FnArg where
  FnArgName x     == FnArgName y     = x == y
  FnArgLambda x _ == FnArgLambda y _ = x == y
  _ == _ = False
--
-- -- TODO: Use
-- updateToUseSpecializations :: PikaModuleElaborated -> PikaModuleElaborated
-- updateToUseSpecializations mod =
--   mod { moduleFnDefs = map go (moduleFnDefs mod) }
--   where
--     go fnDef = runFreshM . onFnDef (pure . goExpr fnDef) $ fnDef
--       where
--         goExpr :: FnDef -> Expr -> Expr
--         goExpr fnDef app@(App fn args) =
--           case lookupFnDef mod (name2String fn) of
--             Nothing -> app
--             Just fnDef ->
--               let fnType = getFnDefType fnDef
--               in
--               case findSpecialization fnDef fnType args of
--                 Nothing -> app
--                 Just specialization -> useSpecialization fnType (name2String fn) args specialization
--         goExpr fnDef e = e
--
-- useSpecialization :: Type -> String -> [Expr] -> Specialization -> Expr
-- useSpecialization ty name origArgs specialization =
--   let newArgs = dropFnTyped ty origArgs
--   in
--   App (string2Name (getDefunName name specialization)) newArgs
--
-- -- | Get the function arguments that the given function name is called with
-- -- in the module
-- getSpecializations :: PikaModuleElaborated -> String -> FreshM [Specialization]
-- getSpecializations mod fnName =
--   case lookupFnDef mod fnName of
--     Nothing -> pure []
--     Just fnDef
--       | isHigherOrder (getFnDefType fnDef) ->
--           concat <$> traverse (getSpecializationsFromDef mod fnName) (moduleFnDefs mod)
--     _ -> pure []
--
-- -- |
-- -- @fnName@: The function name we are searching for
-- --
-- -- @fnDef@: The function being searched
-- getSpecializationsFromDef :: PikaModuleElaborated -> String -> FnDef -> FreshM [Specialization]
-- getSpecializationsFromDef mod fnName fnDef = do
--   exprs <- getFnDefExprs fnDef
--   let typedApps = mapMaybe getCandidateApp $ concatMap universe exprs
--       specs = mapMaybe (uncurry (findSpecialization fnDef)) typedApps
--
--   pure $ trace ("name = " ++ fnDefName fnDef ++ "; specs = " ++ show specs ++ "; def = " ++ ppr' fnDef)
--     $ specs
--   where
--     getCandidateApp :: Expr -> Maybe (Type, [Expr])
--     getCandidateApp (App f args) = do
--       if isFreeName f
--       then do
--         ty <- getFnDefType <$> lookupFnDef mod (name2String f)
--         pure (ty, args)
--       else Nothing
--     getCandidateApp _ = Nothing
--
-- -- | We find an @App "fnName" [arg1, ..., argN]@
-- -- and we want to get the specialization that this represents.
-- findSpecialization :: FnDef -> Type -> [Expr] -> Maybe Specialization
-- findSpecialization currentLocation ty exprs =
--   case map (toFnArg currentLocation) $ getFnTyped ty exprs of
--     [] -> Nothing
--     fnArgs -> Just $ Specialization fnArgs
--
-- toFnArg :: FnDef -> Expr -> FnArg
-- toFnArg fnDef (V x) = FnArgName fnDef (name2String x)
-- toFnArg fnDef e = error $ "toFnArg: " ++ ppr' e
--
--
-- defunctionalizeFnDef :: FnDef -> [Specialization] -> [FnDef]
-- defunctionalizeFnDef fnDef [] = [fnDef]
-- defunctionalizeFnDef fnDef xs = map (generateSpecialization fnDef) xs
--
-- -- | This is where we actually do the main work of defunctionalization
-- generateSpecialization :: FnDef -> Specialization -> FnDef
-- generateSpecialization fnDef spec =
--   let newName = getDefunName (fnDefName fnDef) spec
--   in
--   fnDef
--     { fnDefName = newName
--     , fnDefTypedBranches = go (fnDefTypedBranches fnDef)
--     }
--   where
--     go (Typed ty bs) =
--       Typed ty $ map (generateBranchSpecialization ty spec) bs
--
-- generateBranchSpecialization :: Type -> Specialization -> FnDefBranch -> FnDefBranch
-- generateBranchSpecialization ty spec branch = runFreshM $ do
--   stringSubst <- makeSpecializationSubst ty branch spec
--
--   let
--     nameSubst :: [(ExprName, ExprName)]
--     nameSubst = map (both %~ string2Name) stringSubst
--
--     -- -- TODO: Support for lambdas
--     -- go :: Expr -> Expr
--     -- go e@(App f args) =
--     --   case lookup f stringSubst of
--     --     Nothing -> e
--     --     Just f' -> App f args
--     -- go e@(V _) = rename nameSubst e
--     -- go e = e
--
--   onFnDefBranch (pure . rename nameSubst) branch

type SpecSubst = [(ExprName, Expr)]

makeSpecializationSubst :: Type -> FnDefBranch -> [FnArg] -> FreshM SpecSubst
makeSpecializationSubst ty branch fnArgs = do
  params <- getFnTypedParams ty branch
  pure $ zip params
    $ map (V . string2Name . getFnArgName) fnArgs -- TODO: Support for lambdas

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

-- -- Remove the function parameters from the pattern matches
-- updateMatches :: Type -> PatternMatches Expr GuardedExpr -> FreshM (PatternMatches Expr GuardedExpr)
-- updateMatches ty (PatternMatches bnd) = do
--   (pats, rhs) <- unbind bnd
--
--   pure $ PatternMatches $ bind (dropFnTyped ty pats) rhs

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

