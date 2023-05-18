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

import PikaC.Ppr
import PikaC.Utils

import Data.Maybe
import Control.Applicative

import GHC.Stack

import Debug.Trace

import Unbound.Generics.LocallyNameless.Bind
import Unbound.Generics.LocallyNameless

type Outputs = LayoutArg CExpr

-- toCName :: PikaCore.ExprName -> LocName
-- toCName = string2Name . name2String

codeGenFn :: PikaCore.FnDef -> C.CFunction
codeGenFn fn = runGenC $ do
  params <- mapM internExprName $ PikaCore.fnDefParams fn
  body <- go (PikaCore.fnDefBranches fn)
  pure $
    C.CFunction
      { C.cfunctionName = PikaCore.fnDefName fn
      , C.cfunctionParams = params
      , C.cfunctionBody = body
      }
  where
    go :: [PikaCore.FnDefBranch] -> GenC [Command]
    go [] = pure [C.Nop]
    go (x:xs) = do
      c <- codeGenFnBranch fn x =<< go xs
      pure [c]

codeGenFnBranch :: PikaCore.FnDef -> PikaCore.FnDefBranch -> [Command] -> GenC Command
codeGenFnBranch fn branch elseCmd = do
  cond <- codeGenBase (PikaCore.computeBranchCondition fn branch)
  outputs <- mapM internExprName $ PikaCore.fnDefOutputParams branch

  pointsTos <- mapM convertPointsTo (PikaCore.getPointsToExpr (PikaCore.fnDefBranchBody branch))

  let allocs = findAllocations outputs pointsTos
      zeros = findSetToZero outputs pointsTos

  inputsCode <- concat <$> mapM codeGenInputs (PikaCore.fnDefBranchInputAssertions branch)
  bodyCode <- codeGenExpr outputs (PikaCore.fnDefBranchBody branch)
  pure $
    C.IfThenElse
      cond
      (setToZero zeros
        <> map codeGenAllocation allocs
        <> inputsCode
        <> bodyCode
      )
      elseCmd

codeGenBase :: HasCallStack =>
  PikaCore.Base ->
  GenC CExpr
codeGenBase (PikaCore.V x) = do
  x' <- internExprName x
  pure $ C.LocValue (x' :+ 0)
codeGenBase e@(PikaCore.LayoutV _) = error $ "codeGenBase: " ++ show e
-- codeGenBase (PikaCore.LocValue x) = C.LocValue x
codeGenBase (PikaCore.IntLit i) = pure $ C.IntLit i
codeGenBase (PikaCore.BoolLit b) = pure $ C.BoolLit b
codeGenBase (PikaCore.Add x y) = liftA2 C.Add (codeGenBase x) (codeGenBase y)
codeGenBase (PikaCore.Sub x y) = liftA2 C.Sub (codeGenBase x) (codeGenBase y)
codeGenBase (PikaCore.Equal x y) = liftA2 C.Equal (codeGenBase x) (codeGenBase y)
codeGenBase (PikaCore.Not x) = fmap C.Not (codeGenBase x)
codeGenBase (PikaCore.And x y) = liftA2 C.And (codeGenBase x) (codeGenBase y)

codeGenSimple :: HasCallStack => Outputs -> PikaCore.SimpleExpr -> GenC [Command]
codeGenSimple outputs (PikaCore.BaseExpr (PikaCore.LayoutV xs)) = do
  xs' <- mapM internExprName xs
  pure . codeGenAsn $ connectLayoutArgs outputs (map C.V xs')

codeGenSimple outputs (PikaCore.BaseExpr e) = do
  e' <- codeGenBase e
  pure . codeGenAsn $ connectLayoutArgs outputs [e']

codeGenSimple outputs (PikaCore.WithIn e (B vars body)) = scoped $ do
  vars' <- mapM internExprName vars
  liftA2 (++) (codeGenExpr vars' e) (codeGenSimple outputs body)

codeGenSimple outputs (PikaCore.SslAssertion params asn0) = do
  -- let asn = fmap (rename (zip params outputs)) asn0
  asn <- freshDestructSslAssertion params asn0 outputs
  pure $ codeGenAsn asn

-- convertAsn :: PikaCore.ExprAssertion -> [PointsTo CExpr]
-- convertAsn = undefined

convertSslAssertion ::
  LayoutArg PikaCore.Expr ->
  PikaCore.ExprAssertion ->
  GenC (LayoutArg CExpr, [PointsTo CExpr])
convertSslAssertion params body = scoped $ do
  body' <- mapM convertPointsTo body
  params' <- mapM internExprName params
  pure (params', body')

freshDestructSslAssertion ::
  LayoutArg PikaCore.Expr ->
  PikaCore.ExprAssertion ->
  LayoutArg CExpr ->
  GenC [PointsTo CExpr]
freshDestructSslAssertion origParams origBody newParams = do
  (genParams, newBody) <- convertSslAssertion origParams origBody
  pure $ rename (zip genParams newParams) newBody

connectLayoutArgs :: LayoutArg CExpr -> [CExpr] -> [PointsTo CExpr]
connectLayoutArgs xs ys = do
  zipWith (:->) (map (:+ 0) xs) ys

codeGenAsn :: [PointsTo CExpr] -> [Command]
codeGenAsn = map codeGenPointsTo

codeGenPointsTo :: PointsTo CExpr -> Command
codeGenPointsTo (lhs :-> rhs) =
  C.Assign lhs rhs

-- codeGenPointsTo' :: PointsToExpr -> GenC Command
-- codeGenPointsTo' (lhs :-> rhs) =
--   C.Assign <$> convertLoc lhs <*> codeGenBase (PikaCore.toBase rhs)

convertLoc :: Loc PikaCore.Expr -> GenC CLoc
convertLoc (x :+ i) = do
  x' <- internExprName x
  pure (x' :+ i)

convertPointsTo :: PikaCore.PointsToExpr -> GenC (PointsTo CExpr)
convertPointsTo ((x :+ i) :-> y) = do
  x' <- internExprName x
  y' <- codeGenBase (PikaCore.toBase y)
  pure ((x' :+ i) :-> y')

codeGenExpr :: HasCallStack => Outputs -> PikaCore.Expr -> GenC [Command]
codeGenExpr outputs = go
  where
    go :: PikaCore.Expr -> GenC [Command]
    go (PikaCore.SimpleExpr e) = codeGenSimple outputs e
    go (PikaCore.App f xs) = do
      xs' <- mapM (codeGenBase . PikaCore.toBase) xs
      pure [C.Call f xs' (map C.V outputs)]

-- codeGenAsn asn =
--   -- let sorted = topologicalSortPointsTo asn
--   -- in
--   map codeGenPointsTo asn --sorted
--
codeGenInputs :: PikaCore.ExprAssertion -> GenC [Command]
codeGenInputs =
  sequenceA . mapMaybe go
  where
    go :: PikaCore.PointsToExpr -> Maybe (GenC Command)
    go (x :-> (PikaCore.SimpleExpr (PikaCore.BaseExpr (PikaCore.V y)))) = Just $ do
      y' <- internExprName y
      C.Let y' <$> convertLoc x
    go _ = Nothing
--
-- codeGenPointsTo :: PointsToExpr -> Command
-- codeGenPointsTo (lhs :-> rhs) =
--   C.Assign lhs (codeGenBase rhs)
--
-- codeGenExpr :: HasCallStack => Outputs -> PikaCore.Expr -> [Command]
-- codeGenExpr outputs (PikaCore.SimpleExpr e) =
--   codeGenSimple outputs e
-- codeGenExpr outputs (PikaCore.App f xs) =
--     -- TODO: Translate PikaCore function names into C function names
--   [C.Call f (map (codeGenBase . getBase) xs) (convert outputs)]
--   where
--     getBase (PikaCore.SimpleExpr (PikaCore.BaseExpr x)) = x
--     getBase e = error $ "codeGenExpr.getBase: " ++ show e
--     convert :: Outputs -> [CExpr]
--     convert = map C.V . toList
--
codeGenAllocation :: Allocation CExpr -> Command
codeGenAllocation (Alloc x sz) =
  C.IntoMalloc x sz
--
setToZero :: [C.CName] -> [Command]
setToZero = map go
  where
    go x = C.Assign (x :+ 0) (C.IntLit 0)
--
-- -- sortedCodeGen :: (Ord a, Show a) => Outputs a -> PikaCore.Expr a -> [Command a]
-- -- sortedCodeGen outputs = topologicalSortCommands . codeGenExpr outputs
--
--
--
--
example :: PikaCore.Expr
example =
  PikaCore.SimpleExpr $
  PikaCore.WithIn (PikaCore.App "convertList2" [PikaCore.base $ PikaCore.V $ s2n "nxt"])
    $ bind (map s2n ["r", "y"]) 
      $ PikaCore.SslAssertion (map s2n ["r", "z"])
          [ (s2n "r" :+ 0) :-> PikaCore.base (PikaCore.V (s2n "h"))
          , (s2n "r" :+ 1) :-> PikaCore.base (PikaCore.V (s2n "y"))
          , (s2n "r" :+ 2) :-> PikaCore.base (PikaCore.V (s2n "w"))
          ]

exampleAsn :: PikaCore.ExprAssertion
exampleAsn = 
        [ (s2n "r" :+ 0) :-> PikaCore.base (PikaCore.V (s2n "h"))
        , (s2n "r" :+ 1) :-> PikaCore.base (PikaCore.V (s2n "y"))
        , (s2n "r" :+ 2) :-> PikaCore.base (PikaCore.V (s2n "w"))
        ]

exampleFn :: FnDef
exampleFn =
  FnDef
    { fnDefName = "convertList2"
    , fnDefParams = map s2n ["x", "w", "r"]
    , fnDefBranches =
        [FnDefBranch
          { fnDefOutputParams = map s2n ["r"]
          , fnDefBranchInputAssertions = []
          , fnDefBranchBody =
              PikaCore.SimpleExpr $
              PikaCore.SslAssertion (map s2n ["r"]) []
          }

        ,FnDefBranch
          { fnDefOutputParams = map s2n ["r"]
          , fnDefBranchInputAssertions =
              [[(s2n "x" :+ 0) :-> PikaCore.base (PikaCore.V (s2n "h"))
               ,(s2n "x" :+ 1) :-> PikaCore.base (PikaCore.V (s2n "nxt"))
               ]]
          , fnDefBranchBody =
              PikaCore.SimpleExpr $
              PikaCore.WithIn (PikaCore.App "convertList2" [PikaCore.base . PikaCore.V $ s2n "nxt"])
                $ bind (map s2n ["r", "y"]) 
                  $ PikaCore.SslAssertion (map s2n ["r", "z"])
                      [ (s2n "r" :+ 0) :-> PikaCore.base (PikaCore.V (s2n "h"))
                      , (s2n "r" :+ 1) :-> PikaCore.base (PikaCore.V (s2n "y"))
                      , (s2n "r" :+ 2) :-> PikaCore.base (PikaCore.V (s2n "w"))
                      ]
          }
        ]
    }

