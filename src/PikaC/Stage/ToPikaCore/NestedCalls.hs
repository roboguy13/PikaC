-- | Take function calls in SslAssertions and pull them out
-- into 'with's
{-# LANGUAGE TupleSections #-}

module PikaC.Stage.ToPikaCore.NestedCalls
  (simplifyNestedCalls
  ,simplifyNestedCallsBranch
  )
  where

import PikaC.Syntax.PikaCore.Expr
import PikaC.Syntax.PikaCore.FnDef
import PikaC.Syntax.Heaplet

import Data.Bifunctor

import Control.Lens

import Unbound.Generics.LocallyNameless

simplifyNestedCalls :: Fresh m => FnDef -> m FnDef
simplifyNestedCalls = pure
  -- fnDefBranches.traversed %%~ simplifyNestedCallsBranch

simplifyNestedCallsBranch :: Fresh m => FnDefBranch -> m FnDefBranch
simplifyNestedCallsBranch = pure
--   fnDefBranchBody %%~ rewriteM simplifyOne

-- simplifyOne :: Fresh m => Expr -> m (Maybe Expr)
-- simplifyOne (SslAssertion vars body) = do
--   v <- fresh (string2Name "zz")
--   pure $ do
--     (newAsn, (f, args)) <- setOneCall (V v) body
--     pure $ WithIn
--       (App f args)
--       [v]
--       $ SslAssertion vars newAsn
-- simplifyOne _ = pure Nothing
--
-- setOneCall :: Expr -> ExprAssertion -> Maybe (ExprAssertion, (String, [Expr]))
-- setOneCall newExpr = go Nothing
--   where
--     go found [] = fmap ([], ) found
--     go found@(Just{}) (x:xs) = first (x:) <$> go found xs
--     go Nothing ((p :-> App f args):xs) =
--       first ((p :-> newExpr) :) <$> go (Just (f, args)) xs
--     go found (x:xs) = first (x:) <$> go found xs
--
