module PikaC.Backend.Utils
  where

import PikaC.Utils
import PikaC.Syntax.Pika.Layout
import Unbound.Generics.LocallyNameless

computeBranchCondition :: (HasVar a, IsBase a) =>
  [Name a] -> [Name a] -> a
computeBranchCondition defNames =
  computeModedBranchCondition (map (Moded Out) defNames)

computeModedBranchCondition :: (HasVar a, IsBase a) =>
  [ModedName a] -> [Name a] -> a
computeModedBranchCondition defNames branchNames =
    go allInputNames
  where
    -- note defNames has modes still!!!
    allInputNames = defNames --fnDefInputNames def

    checkName name =
      if modedNameName name `elem` branchNames
        then mkNot (mkEqual (mkVar (modedNameName name)) (intLit 0))
        else mkEqual (mkVar (modedNameName name)) (intLit 0)

    checkName' name rest
      | getMode name == In = rest
      | otherwise          = mkAnd (checkName name) rest

    go [] = boolLit True
    go [name] = checkName' name (boolLit True)
    go (name:rest) =
        checkName' name (go rest)
