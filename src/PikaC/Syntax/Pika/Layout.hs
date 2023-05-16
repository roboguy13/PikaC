{-# LANGUAGE ScopedTypeVariables #-}

module PikaC.Syntax.Pika.Layout
  where

import PikaC.Syntax.Heaplet
import PikaC.Syntax.Pika.Pattern
import PikaC.Syntax.Pika.Expr

import Control.Monad.Identity

import Bound

data Layout a =
  Layout
    { layoutName :: String
    , layoutBranches :: [LayoutBranch a]
    , layoutParams :: [a]
    }

-- data LayoutBranch a =
--   LayoutBranch
--     { layoutPattern :: Pattern a
--     , layoutBody :: [LayoutHeaplet a]
--     }

newtype LayoutBranch a =
  LayoutBranch { layoutPattern :: PatternMatch LayoutBody a }

newtype LayoutBody a = LayoutBody [LayoutHeaplet a]

data LayoutHeaplet a
  = LPointsTo (PointsTo Identity a)
  | LApply
      String -- Layout name
      a      -- Pattern variable
      [a]    -- Layout variables

type LayoutName = String

lookupLayout :: [Layout a] -> LayoutName -> Layout a
lookupLayout [] name = error $ "lookupLayout: Cannot find layout " ++ name
lookupLayout (x:xs) name
  | layoutName x == name = x
  | otherwise = lookupLayout xs name

lookupLayoutBranch :: Layout a -> String -> LayoutBranch a
lookupLayoutBranch layout constructor = go $ layoutBranches layout
  where
    go [] = error $ "lookupLayoutBranch: Cannot find branch for constructor " ++ constructor
    go (x:xs)
      | patConstructor (layoutPattern x) == constructor = x
      | otherwise = go xs

-- | Apply layout to a constructor value
applyLayout :: Layout a -> String -> [a] -> [LayoutHeaplet a]
applyLayout layout constructor args =
  let branch = lookupLayoutBranch layout constructor
  in
  case patternMatch (layoutPattern branch) constructor args of
    Just r -> r
    Nothing -> error "applyLayout"

-- | Reduce layout lambda applications
reduceLayoutApps :: forall a. Expr a -> Expr a
reduceLayoutApps = error "TODO: Implement reduceLayoutApps" --go
  where
    go :: Expr a -> Expr a
    go (ApplyLayout (LayoutLambda adt e) layout) = instantiate1 (LayoutTypeArg layout) e
    -- go e = e

