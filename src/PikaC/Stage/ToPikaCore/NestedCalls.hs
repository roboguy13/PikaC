-- | Take function calls in SslAssertions and pull them out
-- into 'with's
{-# LANGUAGE TupleSections #-}

module PikaC.Stage.ToPikaCore.NestedCalls
  (simplifyNestedCalls
  -- ,simplifyNestedCallsBranch
  )
  where

import PikaC.Syntax.PikaCore.Expr
import PikaC.Syntax.PikaCore.FnDef
import PikaC.Syntax.Heaplet
import PikaC.Syntax.Pika.Layout

import PikaC.Stage.ToPikaCore.Utils
import PikaC.Stage.ToPikaCore.SimplifyM

import Data.Bifunctor

import Control.Lens

import Unbound.Generics.LocallyNameless

import Debug.Trace

simplifyNestedCalls :: Logger m => FnDef -> SimplifyM m FnDef
simplifyNestedCalls = step "simplifyNestedCalls" $ onFnDef (rewriteM simplifyOne)

simplifyOne :: Fresh m => Expr -> m (Maybe Expr)
simplifyOne (SslAssertion bnd) = do
  (vars, body) <- unbind bnd
  v <- fresh (string2Name "zz")
  pure $ do
    (newAsn, (f, args)) <- setOneCall (V v) body
    pure $ WithIn
      (App f args)
      $ bind [Moded In v] -- TODO: How should the mode work here?
        $ SslAssertion (bind vars newAsn)

simplifyOne _ = pure Nothing

setOneCall :: Expr -> ExprAssertion -> Maybe (ExprAssertion, (FnName, [Expr]))
setOneCall newExpr = go Nothing
  where
    go found [] = fmap ([], ) found
    go found@(Just{}) (x:xs) = first (x:) <$> go found xs
    go Nothing ((p :-> App f args):xs) =
      first ((p :-> newExpr) :) <$> go (Just (f, args)) xs
    go found (x:xs) = first (x:) <$> go found xs

