--
-- { v } ==> v
--

{-# LANGUAGE LambdaCase #-}

module PikaC.Stage.ToPikaCore.LayoutVOne
  (layoutVOne)
  where

import PikaC.Syntax.PikaCore.Expr
import PikaC.Syntax.PikaCore.FnDef
import PikaC.Syntax.Pika.Layout
import PikaC.Syntax.Heaplet

import PikaC.Stage.ToPikaCore.Utils

import PikaC.Ppr

import Control.Lens

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Unsafe

import Debug.Trace

layoutVOne :: Fresh m => Expr -> m Expr
layoutVOne = rewriteM go

go :: Fresh m => Expr -> m (Maybe Expr)
go = \case
  LayoutV [v] -> pure $ Just v
  SslAssertion bnd -> do
    (v, body) <- unbind bnd
    goBnd body >>= \case
      Just body' ->
        pure $ Just $ SslAssertion $ bind v body'
      Nothing -> pure Nothing
  _ -> pure Nothing

goBnd :: Fresh m => ExprAssertion -> m (Maybe ExprAssertion)
goBnd (x :-> y : rest) =
  go y >>= \case
    Just y' -> pure $ Just (x :-> y' : rest)
    Nothing ->
      goBnd rest >>= \case
        Just rest' -> pure $ Just $ (x :-> y) : rest'
        Nothing -> pure Nothing
goBnd [] = pure Nothing

