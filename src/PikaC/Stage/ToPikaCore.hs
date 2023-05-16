module PikaC.Stage.ToPikaCore
  where

import qualified PikaC.Syntax.Pika.Expr as Pika
import qualified PikaC.Syntax.Pika.FnDef as Pika

import qualified PikaC.Syntax.PikaCore.Expr as PikaCore
import qualified PikaC.Syntax.PikaCore.FnDef as PikaCore

import PikaC.Syntax.Pika.Layout
import PikaC.FreshGen
import PikaC.Syntax.Type

toPikaCore :: [Layout String] -> Pika.FnDef LayoutName String -> PikaCore.FnDef String
toPikaCore layouts fn = runFreshGen $ do
  params <- concat <$> mapM (generateParams layouts) (argTypes ++ [resultType])
  pure $
    PikaCore.FnDef
      { PikaCore.fnDefName = Pika.fnDefName fn
      , PikaCore.fnDefParams = params  
      }
  where
    (argTypes, resultType) = splitFnType (Pika.fnDefType fn)

generateParams :: [Layout String] -> Type LayoutName -> FreshGen [String]
generateParams layouts IntType = (:[]) <$> fresh "i"
generateParams layouts BoolType = (:[]) <$> fresh "b"
generateParams layouts (TyVar layoutName) =
  let layout = lookupLayout layouts layoutName
  in
  mapM (fresh . ("_" <>)) (layoutParams layout)

