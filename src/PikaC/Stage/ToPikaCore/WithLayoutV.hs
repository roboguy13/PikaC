-- TODO: This doesn't seem to work
module PikaC.Stage.ToPikaCore.WithLayoutV
  (withLayoutV)
  where

import PikaC.Syntax.PikaCore.Expr
import PikaC.Syntax.PikaCore.FnDef
import PikaC.Syntax.Pika.Layout

import PikaC.Stage.ToPikaCore.Utils

import Control.Lens

import Unbound.Generics.LocallyNameless

import Debug.Trace

withLayoutV :: Fresh m => FnDef -> m FnDef
withLayoutV =
  onFnDef (rewriteM withLayoutV'Expr)

withLayoutV'Expr :: Fresh m => Expr -> m (Maybe Expr)
withLayoutV'Expr (WithIn (LayoutV vars) bnd) = do
  (bndVars, e) <- unbind bnd
  pure $ Just $ substs (zip (map modedNameName bndVars) vars) e

withLayoutV'Expr _ = pure Nothing

