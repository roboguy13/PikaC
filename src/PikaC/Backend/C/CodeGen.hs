{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module PikaC.Backend.C.CodeGen
  where

import qualified PikaC.Syntax.PikaCore.Expr as PikaCore
import qualified PikaC.Syntax.PikaCore.FnDef as PikaCore

import PikaC.Syntax.PikaCore.FnDef (FnDef (..), FnDefBranch (..))

import qualified PikaC.Backend.C.Syntax as C
import PikaC.Backend.C.Syntax (CExpr, CLoc, Command)

import PikaC.Backend.C.Monad

-- import PikaC.Backend.C.DataDeps
import PikaC.Syntax.Heaplet
import PikaC.Syntax.Pika.Layout

import PikaC.Ppr
import PikaC.Utils

import Data.Maybe

import Control.Applicative
import Control.Monad

import GHC.Stack

import Debug.Trace

import Unbound.Generics.LocallyNameless.Bind
import Unbound.Generics.LocallyNameless

import Control.Lens

import Control.Arrow

type Outputs = LayoutArg CExpr

codeGenFn :: PikaCore.FnDef -> C.CFunction
codeGenFn fnDef = runFreshM $ do
  let PikaCore.FnName fnName = _fnDefName fnDef

  (inVars0, bnd1) <- unbind $ _fnDefBranches fnDef
  (outVars0, branches) <- unbind bnd1

  inVars <- mapM fresh (map modedNameName inVars0)
  outVars <- mapM fresh (map modedNameName outVars0)

  let outParams = map convertName outVars
      inParams = map convertName inVars

  let params = inParams ++ outParams

  body <- mapM (convertBranch outParams) branches

  pure $ C.CFunction
    { C.cfunctionName = fnName
    , C.cfunctionParams = params
    , C.cfunctionBody = [flattenBranchCmds inParams (zip branches body)]
    }

-- genAsn ::
--   Bind [ModedName PikaCore.Expr] PikaCore.ExprAssertion ->
--   FreshM [C.Command]
-- genAsn bnd = do
--   (vars, asn) <- unbind bnd
--   let allocs = findAllocations (map modedNameName vars) asn
--
--   pure $ map codeGenAllocation allocs
--           ++ map (codeGenPointsTo . convertPointsTo) asn

flattenBranchCmds :: [C.CName] -> [(FnDefBranch, [C.Command])] -> C.Command
flattenBranchCmds _ [] = C.Nop
flattenBranchCmds allNames ((branch, cmds) : rest) =
  C.IfThenElse (C.computeBranchCondition allNames branchNames)
    cmds
    [flattenBranchCmds allNames rest]
  where
    branchNames =
      concatMap (map (string2Name . name2String . PikaCore.getV . locBase . pointsToLhs)) $ _fnDefBranchInputAssertions branch

convertBranch :: [C.CName] -> PikaCore.FnDefBranch -> FreshM [C.Command]
convertBranch outVars branch = do
  body <- convertBranchBody outVars $ _fnDefBranchBody branch
  pure (concatMap (map setupInput) (_fnDefBranchInputAssertions branch)
         ++ body)

-- | Take inputs heaplets like (x+2) :-> v and generate v = *(x+2)
setupInput :: PointsTo PikaCore.Expr -> C.Command
setupInput ((PikaCore.V lhs :+ i) :-> PikaCore.V rhs) =
  C.Let (convertName rhs) (C.V (convertName lhs) :+ i)

convertName :: PikaCore.ExprName -> C.CName
convertName = string2Name . name2String

convertBranchBody :: [C.CName] -> PikaCore.Expr -> FreshM [C.Command]
convertBranchBody outVars = go
  where
    go (PikaCore.V x) = pure [assignVar (getOneVar outVars) x]
    go (PikaCore.LayoutV xs) = -- TODO: Make sure lengths match
        pure $ zipWith assignVar outVars (map PikaCore.getV xs)
    go (PikaCore.IntLit i) =
        pure [assignValue (getOneVar outVars) (C.IntLit i)]
    go (PikaCore.BoolLit b) =
        pure [assignValue (getOneVar outVars) (C.BoolLit b)]
    go (PikaCore.Add x y) = goBin C.Add x y
    go (PikaCore.Sub x y) = goBin C.Sub x y
    go (PikaCore.Equal x y) = goBin C.Equal x y
    go (PikaCore.Not x) =
      pure [assignValue (getOneVar outVars) (C.Not (convertBase x))]
    go (PikaCore.And x y) = goBin C.And x y
    go (PikaCore.WithIn e bnd) = do
      (vars, body) <- unbind bnd
      let modes = map getMode vars
          unmodedVars = map modedNameName vars
      case e of
        PikaCore.V x ->
          let body' = instantiate bnd (zipWith Moded modes [PikaCore.V x])
          in
          go body'

        PikaCore.LayoutV xs ->
          let body' = instantiate bnd (zipWith Moded modes xs)
          in
          go body'

        PikaCore.App (PikaCore.FnName f) args -> do
          body' <- go body

          pure $
            [C.Call f
              (getAppArgs args)
              (map (C.V . convertName) unmodedVars)
            ] ++ body'
        _ -> error $ "Expected variable, variable list or function application in with-in. Found " ++ ppr' e

    go (PikaCore.SslAssertion bnd) = do
      (vars, body) <- unbind bnd
      let modes = map getMode vars

          outVars' :: [Moded PikaCore.Expr]
          outVars' =
            zipWith (\m v -> Moded m (PikaCore.V (string2Name (name2String v))))
              modes
              outVars
      pure (map codeGenPointsTo (instantiate bnd outVars'))

    goBin f x y = 
        pure [assignValue (getOneVar outVars) (f (convertBase x) (convertBase y))]

getAppArgs :: [PikaCore.Expr] -> [C.CExpr]
getAppArgs = concatMap go
  where
    go (PikaCore.LayoutV xs) = map convertBase xs
    go e = [convertBase e]

getOneVar :: HasCallStack => [C.CName] -> C.CName
getOneVar [v] = v
getOneVar vs = error $ "Expected one variable, got " ++ show vs


assignVar :: C.CName -> PikaCore.ExprName -> C.Command
assignVar cv = assignValue cv . C.V . convertName

assignValue :: C.CName -> CExpr -> C.Command
assignValue cv = C.Assign (C.V cv :+ 0)

convertBase :: PikaCore.Expr -> CExpr
convertBase (PikaCore.V x) = C.V $ convertName x
convertBase (PikaCore.IntLit i) = C.IntLit i
convertBase (PikaCore.BoolLit b) = C.BoolLit b
convertBase (PikaCore.Add x y) = C.Add (convertBase x) (convertBase y)
convertBase (PikaCore.Sub x y) = C.Sub (convertBase x) (convertBase y)
convertBase (PikaCore.Equal x y) = C.Equal (convertBase x) (convertBase y)
convertBase (PikaCore.And x y) = C.And (convertBase x) (convertBase y)
convertBase (PikaCore.Not x) = C.Not (convertBase x)
convertBase e = error $ "convertBase: " ++ ppr' e

codeGenPointsTo :: PointsTo PikaCore.Expr -> Command
codeGenPointsTo ((PikaCore.V lhs :+ i) :-> rhs) =
  C.Assign (C.V (convertName lhs) :+ i) (convertBase rhs)
codeGenPointsTo p = error $ "codeGenPointsTo: " ++ ppr' p

-- convertPointsTo :: PointsTo PikaCore.Expr -> PointsTo CExpr
-- convertPointsTo (lhs :-> rhs) =
--   convertLoc lhs :-> convertBase rhs

convertLoc :: Loc PikaCore.Expr -> Loc CExpr
convertLoc (x :+ i) = convertBase x :+ i

codeGenAllocation :: Allocation PikaCore.Expr -> Command
codeGenAllocation (Alloc x sz) =
  C.IntoMalloc (string2Name (name2String x)) sz

-- codeGenPointsTo :: PointsTo CExpr -> Command
-- codeGenPointsTo ((lhs :+ i) :-> rhs) =
--   C.Assign rhs lhs
--   -- C.Assign lhs rhs

-- toCName :: PikaCore.ExprName -> LocName
-- toCName = string2Name . name2String

-- codeGenFn :: PikaCore.FnDef -> C.CFunction
-- codeGenFn fn = runGenC $ do
--   params <- mapM internExprName $ PikaCore._fnDefParams fn
--
--   -- let (firstBranch:_) = PikaCore._fnDefBranches fn
--   -- pointsTos <- mapM convertPointsTo (PikaCore.getPointsToExpr (PikaCore._fnDefBranchBody firstBranch))
--   --
--   -- let allocs = findAllocations params pointsTos
--   -- paramWriter <- writeParams allocs
--
--   body <- go params (PikaCore._fnDefBranches fn)
--
--   pure $
--     C.CFunction
--       { C.cfunctionName = PikaCore._fnDefName fn
--       , C.cfunctionParams = params
--       , C.cfunctionBody = body
--       }
--   where
--     pointsTos = toListOf (traversed . PikaCore.fnDefBranchBody . to PikaCore.getPointsToExpr . traversed) (PikaCore._fnDefBranches fn)
--     possiblyWritten = locBase <$> locsPossiblyWrittenTo pointsTos
--
--     go :: [C.CName] -> [PikaCore.FnDefBranch] -> GenC [Command]
--     go _params [] = pure [C.Nop]
--     go params (x:xs) = do
--       c <- codeGenFnBranch params possiblyWritten fn x =<< go params xs
--       pure [c]
--
-- codeGenFnBranch :: [C.CName] -> [PikaCore.ExprName] -> PikaCore.FnDef -> PikaCore.FnDefBranch -> [Command] -> GenC Command
-- codeGenFnBranch params possiblyWritten fn branch elseCmd = do
--   outputs <- mapM internExprName $ PikaCore._fnDefOutputParams branch
--
--   fnNames <- mapM internExprName $ fastNub $ PikaCore.fnDefInputNames fn
--   branchNames <- mapM internExprName $ fastNub $ PikaCore.fnDefBranchInputNames branch
--
--   let cond = C.computeBranchCondition fnNames branchNames
--
--   pointsTos <- mapM convertPointsTo (PikaCore.getPointsToExpr (PikaCore._fnDefBranchBody branch))
--
--   possiblyWritten' <- mapM internExprName possiblyWritten
--
--   let allocs = findAllocations outputs pointsTos
--       allocNames = map (\(Alloc x _) -> x) allocs
--       -- zeros = C.findSetToZero possiblyWritten' outputs pointsTos
--       zeros = C.findSetToZero possiblyWritten' outputs pointsTos
--
--   (newNames, indirectWriter) <- writeIndirects allocs
--
--   inputsCode <- concat <$> mapM codeGenInputs (PikaCore._fnDefBranchInputAssertions branch)
--
--   let branchBody =
--         rename (zip allocNames newNames) (PikaCore._fnDefBranchBody branch)
--
--   let outputs' = rename (zip allocNames newNames) outputs
--
--   bodyCode <- indirectWriter <$> codeGenExpr outputs' branchBody
--   pure $
--     C.IfThenElse
--       cond
--       (setToZero zeros
--         -- <> map codeGenAllocation allocs
--         <> inputsCode
--         <> bodyCode
--       )
--       elseCmd
--
-- codeGenBase :: HasCallStack =>
--   PikaCore.Expr ->
--   GenC CExpr
-- codeGenBase (PikaCore.V x) = do
--   x' <- internExprName x
--   pure $ C.LocValue (x' :+ 0)
-- codeGenBase e@(PikaCore.LayoutV _) = error $ "codeGenBase: " ++ show e
-- -- codeGenBase (PikaCore.LocValue x) = C.LocValue x
-- codeGenBase (PikaCore.IntLit i) = pure $ C.IntLit i
-- codeGenBase (PikaCore.BoolLit b) = pure $ C.BoolLit b
-- codeGenBase (PikaCore.Add x y) = liftA2 C.Add (codeGenBase x) (codeGenBase y)
-- codeGenBase (PikaCore.Sub x y) = liftA2 C.Sub (codeGenBase x) (codeGenBase y)
-- codeGenBase (PikaCore.Equal x y) = liftA2 C.Equal (codeGenBase x) (codeGenBase y)
-- codeGenBase (PikaCore.Not x) = fmap C.Not (codeGenBase x)
-- codeGenBase (PikaCore.And x y) = liftA2 C.And (codeGenBase x) (codeGenBase y)
-- codeGenBase e = error $ "codeGenBase: " ++ show e
--
-- -- codeGenSimple :: HasCallStack => Outputs -> PikaCore.SimpleExpr -> GenC [Command]
-- --
-- -- codeGenSimple outputs (PikaCore.BaseExpr e) = do
-- --   e' <- codeGenBase e
-- --   pure . codeGenAsn $ connectLayoutArgs outputs [e']
--
-- codeGenBaseCmd :: HasCallStack => Outputs -> PikaCore.Expr -> GenC [Command]
-- codeGenBaseCmd outputs = connectToExpr outputs <=< codeGenBase
--
-- connectToExpr :: Outputs -> CExpr -> GenC [Command]
-- connectToExpr outputs e =
--   pure . codeGenAsn $ connectLayoutArgs outputs [e]
--
-- -- convertAsn :: PikaCore.ExprAssertion -> [PointsTo CExpr]
-- -- convertAsn = undefined
--
-- convertSslAssertion :: HasCallStack =>
--   LayoutArg PikaCore.Expr ->
--   PikaCore.ExprAssertion ->
--   GenC (LayoutArg CExpr, [PointsTo CExpr])
-- convertSslAssertion params body = do
--   body' <- mapM convertPointsTo body
--   params' <- mapM internExprName params
--   pure (params', body')
--
-- freshDestructSslAssertion :: HasCallStack =>
--   LayoutArg PikaCore.Expr ->
--   PikaCore.ExprAssertion ->
--   LayoutArg CExpr ->
--   GenC [PointsTo CExpr]
-- freshDestructSslAssertion origParams origBody newParams = do
--   (genParams, newBody) <- convertSslAssertion origParams origBody
--   pure $ rename (zip genParams newParams) newBody
--
-- connectLayoutArgs :: LayoutArg CExpr -> [CExpr] -> [PointsTo CExpr]
-- connectLayoutArgs xs ys = do
--   zipWith (:->) (map (:+ 0) xs) ys
--
-- codeGenAsn :: [PointsTo CExpr] -> [Command]
-- codeGenAsn = map codeGenPointsTo
--
-- codeGenPointsTo :: PointsTo CExpr -> Command
-- codeGenPointsTo (lhs :-> rhs) =
--   C.Assign lhs rhs
--
-- -- codeGenPointsTo' :: PointsToExpr -> GenC Command
-- -- codeGenPointsTo' (lhs :-> rhs) =
-- --   C.Assign <$> convertLoc lhs <*> codeGenBase (PikaCore.toBase rhs)
--
-- convertLoc :: Loc PikaCore.Expr -> GenC CLoc
-- convertLoc (x :+ i) = do
--   x' <- internExprName x
--   pure (x' :+ i)
--
-- convertPointsTo :: HasCallStack => PikaCore.PointsToExpr -> GenC (PointsTo CExpr)
-- convertPointsTo ((x :+ i) :-> y) = do
--   x' <- internExprName x
--   y' <- codeGenBase y
--   pure ((x' :+ i) :-> y')
--
-- -- | Like codeGenBase, but handle LayoutV's
-- codeGenArg :: PikaCore.Expr -> GenC ([C.CExpr], [Command])
-- codeGenArg (PikaCore.LayoutV vs) = do
--   vs' <- mapM internExprName vs
--   pure (map C.V vs', [])
-- codeGenArg e = do
--   e' <- codeGenBase e
--   pure ([e'], [])
--   -- tempVars <- mapM (\_ -> fresh (string2Name "vv")) xs
--   -- cmds <- concat <$> zipWithM codeGenExpr (map (:[]) tempVars) xs
--
--
-- codeGenExpr :: HasCallStack => Outputs -> PikaCore.Expr -> GenC [Command]
-- codeGenExpr outputs = go
--   where
--     go :: PikaCore.Expr -> GenC [Command]
--     go e | PikaCore.isBase e = codeGenBaseCmd outputs e
--
--     go (PikaCore.LayoutV xs) = do
--       xs' <- mapM internExprName xs
--       pure . codeGenAsn $ connectLayoutArgs outputs (map C.V xs')
--
--     go (PikaCore.App f xs) = do
--       -- tempVars <- mapM (\_ -> fresh (string2Name "vv")) xs
--       -- cmds <- concat <$> zipWithM codeGenExpr (map (:[]) tempVars) xs
--       -- let xs' = map C.V tempVars
--       -- xs' <- mapM codeGenBase xs
--
--       (xs', cmds) <- (concat *** concat) <$> mapAndUnzipM codeGenArg xs
--
--       tempOuts <- mapM fresh outputs
--       pure $
--         -- cmds ++
--         map (`C.IntoMalloc` 1) tempOuts
--           ++
--           [C.Call f xs' (map C.V tempOuts)]
--           ++
--         zipWith (\temp orig -> C.Let orig (temp :+ 0)) tempOuts outputs
--
--     go (PikaCore.WithIn e vars body) = do
--       vars' <- mapM internExprName vars -- TODO: Is this correct?
--       liftA2 (++) (codeGenExpr vars' e) (codeGenExpr outputs body)
--
--     go (PikaCore.SslAssertion params asn0) = do
--       -- let asn = fmap (rename (zip params outputs)) asn0
--       asn <- freshDestructSslAssertion params asn0 outputs
--       pure $ codeGenAsn asn
--
--     go e = error $ "codeGenExpr.go: " ++ show e
--
-- -- codeGenAsn asn =
-- --   -- let sorted = topologicalSortPointsTo asn
-- --   -- in
-- --   map codeGenPointsTo asn --sorted
-- --
-- codeGenInputs :: PikaCore.ExprAssertion -> GenC [Command]
-- codeGenInputs =
--   sequenceA . mapMaybe go
--   where
--     go :: PikaCore.PointsToExpr -> Maybe (GenC Command)
--     go p@(x :-> PikaCore.V y) = Just $ do
--       y' <- internExprName y
--       cmd <- C.Let y' <$> convertLoc x
--       pure cmd
--     go _ = Nothing
--
-- codeGenAllocation :: Allocation CExpr -> Command
-- codeGenAllocation (Alloc x sz) =
--   C.IntoMalloc x sz
-- --
-- setToZero :: [C.CName] -> [Command]
-- setToZero = map go
--   where
--     go x = C.Assign (x :+ 0) (C.IntLit 0)
--
-- -- writeIndirect :: Allocation CExpr -> GenC (C.CName, [Command] -> [Command])
-- -- writeIndirect (Alloc origName sz) = do
-- --   newName <- fresh origName
-- --   pure
-- --     (newName
-- --     ,\cmds -> C.IntoMalloc newName sz
-- --                 : cmds ++
-- --                [C.Let origName (newName :+ 0)
-- --                ]
-- --     )
--
-- getDeclarations :: [Command] -> [C.CName]
-- getDeclarations = mapMaybe go
--   where
--     go (C.IntoMalloc n _) = Just n
--     go (C.Let n _) = Just n
--     go _ = Nothing
--
-- -- getNames :: [C.Command] -> [C.CName]
-- -- getNames = toListOf fv
--
-- -- addDecls :: [C.CName] -> [C.Command] -> [C.Command]
-- -- addDecls params cmds =
-- --   map (C.Decl . string2Name) names ++ cmds
-- --   where
-- --     existingDecls = map name2String $ getDeclarations cmds ++ params
-- --     names = fastNub $ filter p $ map name2String $ getNames cmds
-- --     p x = x `notElem` existingDecls
--
-- freshAllocs :: [Allocation CExpr] -> GenC ([C.CName], [Command])
-- freshAllocs allocs = do
--   freshNames <- mapM (\(Alloc n _) -> fresh n) allocs
--   pure (freshNames, zipWith (\n (Alloc _ sz) -> C.IntoMalloc n sz) freshNames allocs)
--
-- -- | Remove one layer of indirection in preparation for a function
-- -- call that will write.
-- -- Create a fresh name that is a dereferenced version of the
-- -- argument name. Then, after the commands run, write back the contents of the fresh name into the original name.
--
-- writeIndirects :: [Allocation CExpr] -> GenC ([C.CName], [Command] -> [Command])
-- writeIndirects origAllocs = do
--   (freshNames, allocCmds) <- freshAllocs origAllocs
--   let writeBack =
--         zipWith (\(Alloc x _) y -> C.Assign (x :+ 0) (C.V y)) origAllocs freshNames
--   pure
--     (freshNames
--     ,\cmds -> allocCmds ++ cmds ++ writeBack
--     )

-- example :: PikaCore.Expr
-- example =
--   PikaCore.WithIn (PikaCore.App "convertList2" [ PikaCore.V $ s2n "nxt"])
--      (map s2n ["r", "y"])
--       $ PikaCore.SslAssertion (map s2n ["r", "z"])
--           [ (s2n "r" :+ 0) :-> PikaCore.V (s2n "h")
--           , (s2n "r" :+ 1) :-> PikaCore.V (s2n "y")
--           , (s2n "r" :+ 2) :-> PikaCore.V (s2n "w")
--           ]
--
-- exampleAsn :: PikaCore.ExprAssertion
-- exampleAsn = 
--         [ (s2n "r" :+ 0) :-> PikaCore.V (s2n "h")
--         , (s2n "r" :+ 1) :-> PikaCore.V (s2n "y")
--         , (s2n "r" :+ 2) :-> PikaCore.V (s2n "w")
--         ]
--
-- exampleFn :: FnDef
-- exampleFn =
--   FnDef
--     { _fnDefName = "convertList2"
--     , _fnDefParams = map s2n ["x", "w", "r"]
--     , _fnDefBranches =
--         [FnDefBranch
--           { _fnDefOutputParams = map s2n ["w", "r"]
--           , _fnDefBranchInputAssertions = []
--           , _fnDefBranchBody =
--               PikaCore.SslAssertion (map s2n ["r"]) []
--           }
--
--         ,FnDefBranch
--           { _fnDefOutputParams = map s2n ["w", "r"]
--           , _fnDefBranchInputAssertions =
--               [[(s2n "x" :+ 0) :-> PikaCore.V (s2n "h")
--                ,(s2n "x" :+ 1) :-> PikaCore.V (s2n "nxt")
--                ]]
--           , _fnDefBranchBody =
--               PikaCore.WithIn (PikaCore.App "convertList2" [PikaCore.V $ s2n "nxt"])
--                  (map s2n ["r", "b"])
--                   $ PikaCore.SslAssertion (map s2n ["r", "w"])
--                       [ (s2n "r" :+ 0) :-> PikaCore.V (s2n "h")
--                       , (s2n "r" :+ 1) :-> PikaCore.V (s2n "b")
--                       , (s2n "r" :+ 2) :-> PikaCore.V (s2n "w")
--                       ]
--           }
--         ]
--     }
--
