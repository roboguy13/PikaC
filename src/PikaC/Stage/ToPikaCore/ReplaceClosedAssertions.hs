--
-- A closed assertion is one of the form
--
--      layout { } { x :-> a ** (x+1) :-> b ** ... ** y :-> c ** ... }
--
-- This gets replaced by
--
--      {x, y, ...}
--
-- NOTE: Should this be used on the final result assertion?
--
module PikaC.Stage.ToPikaCore.ReplaceClosedAssertions
  (replaceClosedAssertions)
  where

import PikaC.Syntax.PikaCore.Expr
import PikaC.Syntax.PikaCore.FnDef
import PikaC.Syntax.Heaplet
import PikaC.Syntax.Pika.Layout

import PikaC.Stage.ToPikaCore.Utils
import PikaC.Stage.ToPikaCore.SimplifyM
import PikaC.Utils

import Data.List

import Control.Applicative
import Control.Monad

import Control.Lens

import Unbound.Generics.LocallyNameless

replaceClosedAssertions :: Logger m => Expr -> SimplifyM m Expr
replaceClosedAssertions = step "replaceClosedAssertions" $ rewriteM go

go :: Fresh m => Expr -> m (Maybe Expr)
go (SslAssertion bnd) = do
  (vars, asn) <- unbind bnd
  case vars of
    [] ->
      case asn of
        [] -> pure Nothing --pure $ Just $ IntLit 0
        (_:_) -> pure $ Just $ LayoutV $ map V (lhsNames asn)
    (_:_) -> pure Nothing
go _ = pure Nothing

