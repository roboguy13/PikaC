module PikaC.Backend.Utils
  where

import PikaC.Utils
import Unbound.Generics.LocallyNameless

computeBranchCondition :: (HasVar a, IsBase a) =>
  [Name a] -> [Name a] -> a
computeBranchCondition defNames branchNames =
    go allInputNames
  where
    allInputNames = defNames --fnDefInputNames def

    checkName name =
      if name `elem` branchNames
        then mkNot (mkEqual (mkVar name) (intLit 0))
        else mkEqual (mkVar name) (intLit 0)

    go [] = boolLit True
    go [name] = checkName name
    go (name:rest) =
      mkAnd (checkName name) (go rest)
