{-# LANGUAGE TypeApplications #-}

module PikaC.Stage.ToPikaCore
  where

import qualified PikaC.Syntax.Pika.Expr as Pika
import qualified PikaC.Syntax.Pika.FnDef as Pika

import qualified PikaC.Syntax.PikaCore.Expr as PikaCore
import qualified PikaC.Syntax.PikaCore.FnDef as PikaCore

import PikaC.Syntax.Pika.Layout
import PikaC.Syntax.Type
import PikaC.Syntax.Heaplet

import Unbound.Generics.LocallyNameless

import Data.Void

toPikaCore :: [Layout] -> Pika.FnDef -> PikaCore.FnDef
toPikaCore layouts fn = runFreshM $ do
  params <- concat <$> mapM (generateParams layouts) (argTypes ++ [resultType])
  pure $
    PikaCore.FnDef
      { PikaCore.fnDefName = Pika.fnDefName fn
      , PikaCore.fnDefParams = params
      }
  where
    (argTypes, resultType) = splitFnType (Pika.fnDefType fn)

generateParams :: [Layout] -> Type -> FreshM [LocName]
generateParams layouts IntType = (:[]) <$> fresh (string2Name "i")
generateParams layouts BoolType = (:[]) <$> fresh (string2Name "b")
generateParams _ (TyVar v) = error $ "generateParams: Still has layout type variable " ++ show v
generateParams layouts (TyLayoutName layoutName) =
  let layout = lookupLayout layouts layoutName
  in
  mapM fresh $ layoutParams layout

