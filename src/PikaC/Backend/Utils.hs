module PikaC.Backend.Utils
  where

import PikaC.Utils
import PikaC.Syntax.Pika.Layout
import Unbound.Generics.LocallyNameless

computeBranchCondition :: (HasVar a, IsBase a) =>
  [ModedName a] -> [Name a] -> a
computeBranchCondition defNames branchNames =
    go allInputNames
  where
    -- note defNames has modes still!!!
    allInputNames = defNames --fnDefInputNames def

    checkName name =
      if (modedNameName name) `elem` branchNames
        then mkNot (mkEqual (mkVar (modedNameName name)) (intLit 0))
        else mkEqual (mkVar (modedNameName name)) (intLit 0)

    go [] = boolLit True
    go [name] = checkName name
    go (name:rest) =
      if (getMode name) == In
        then (go rest)
        else mkAnd (checkName name) (go rest)