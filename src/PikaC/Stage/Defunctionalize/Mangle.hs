--
-- Turn _ in names to __ to avoid collision with function names
-- generated during defunctionalization.
--

module PikaC.Stage.Defunctionalize.Mangle
  (mangleFnDef)
  where

import PikaC.Syntax.Pika.Expr
import PikaC.Syntax.Pika.FnDef
import PikaC.Syntax.Pika.Pattern
import PikaC.Syntax.Type
import PikaC.Utils

import Unbound.Generics.LocallyNameless

import Control.Lens

mangleFnDef :: FnDef -> FnDef
mangleFnDef def =
  -- rename (zip exprFVs (map mangleName exprFVs)) def
  -- renameFnName (zip exprFVs (map mangleName exprFVs)) def
  runFreshM $ overTypedBranchesM go def
  where
    -- exprFVs :: [ExprName]
    -- exprFVs = toListOf fv def

    fnNames = getFnNamesDef def

    -- -- We use a Maybe here because otherwise we will go into an infinite
    -- -- loop (since there would no longer be a non-bottom fixpoint)
    -- renaming "" = Nothing
    -- renaming x =
    --   if x `elem` fnNames
    --     then Just $ mangleString x
    --     else Nothing

    renaming = map rho1 fnNames
      where
        rho1 x = (x, mangleName x)

    go :: Typed [FnDefBranch] -> FreshM (Typed [FnDefBranch])
    go (Typed t xs) = Typed t <$> traverse goBranch xs

    goBranch :: FnDefBranch -> FreshM FnDefBranch
    goBranch (FnDefBranch b) = FnDefBranch <$> goMatches b

-- rename (map (both %~ convert) rho) . 
    goMatches :: PatternMatches Expr GuardedExpr -> FreshM (PatternMatches Expr GuardedExpr)
    goMatches (PatternMatches bnd) = do
      (pats, body) <- unbind bnd

      let boundVars = concat $ map getNames pats

      pure $ PatternMatches $ bind pats $ overGuardedExpr (rename renaming) body

mangleName :: ExprName -> ExprName
mangleName = string2Name . mangleString . name2String

mangleString :: String -> String
mangleString "" = ""
mangleString ('_':cs) = "__" ++ mangleString cs
mangleString (c  :cs) = c     : mangleString cs

getFnNamesDef :: FnDef -> [ExprName]
getFnNamesDef = runFreshM . typedBranches . fnDefTypedBranches
  where
    typedBranches :: Typed [FnDefBranch] -> FreshM [ExprName]
    typedBranches (Typed _ xs) = concat <$> traverse branch xs

    branch :: FnDefBranch -> FreshM [ExprName]
    branch (FnDefBranch (PatternMatches bnd)) = do
      (_, GuardedExpr e1 e2) <- unbind bnd
      pure $ getFnNames e1 ++ getFnNames e2

