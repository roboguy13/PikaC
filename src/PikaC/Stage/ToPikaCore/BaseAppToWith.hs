--
-- f [[0]] ...
--
--      ==>
--
-- with {x} := f [[0]] ... in x
--
module PikaC.Stage.ToPikaCore.BaseAppToWith
  (baseAppToWith)
  where

import PikaC.Syntax.PikaCore.Expr
import PikaC.Syntax.PikaCore.FnDef
import PikaC.Syntax.Heaplet
import PikaC.Syntax.Pika.Layout

import PikaC.Utils
import PikaC.Stage.ToPikaCore.Utils
import PikaC.Stage.ToPikaCore.SimplifyM

import Control.Lens

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Unsafe

baseAppToWith :: Logger m => Expr -> SimplifyM m Expr
baseAppToWith = step "baseAppToWith" go

go :: Fresh m => Expr -> m Expr
go (WithIn app@(App _ [0] _) bnd) =
  let (v, body) = unsafeUnbind bnd
  in
  WithIn <$> plate go app <*> fmap (bind v) (plate go body)

go (App f [0] xs) = do
  n <- fresh (string2Name "b" :: ExprName)
  xs' <- traverse go xs
  pure $ WithIn
    (App f [0] xs')
    (bind [Moded Out n] (V n))

go e0 = plate go e0

