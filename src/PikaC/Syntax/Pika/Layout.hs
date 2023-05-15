module PikaC.Syntax.Pika.Layout
  where

import PikaC.Syntax.Heaplet
import PikaC.Syntax.Pika.Pattern

import Control.Monad.Identity

data Layout a =
  Layout
    { layoutName :: String
    , layoutBranches :: [LayoutBranch a]
    , layoutParams :: [a]
    }

data LayoutBranch a =
  LayoutBranch
    { layoutPattern :: Pattern a
    , layoutBody :: [LayoutHeaplet a]
    }

data LayoutHeaplet a
  = LPointsTo (PointsTo Identity a)
  | LApply
      String -- Layout name
      a      -- Pattern variable
      [a]    -- Layout variables

