module PikaC.Syntax.Pika.Expr
  where

import PikaC.Syntax.Type

data Expr a
  = V a
  | IntLit Int
  | BoolLit Bool
  | LayoutLambda (LayoutConstraint a) (Expr a)
  | ApplyLayout (Expr a) (LayoutArg a)
  | App String [Expr a]
  deriving (Show)

-- | No layout lambdas
-- Precondition: Layout applications in input should be fully reduced
isConcrete :: Expr a -> Bool
isConcrete (V {}) = True
isConcrete (IntLit {}) = True
isConcrete (BoolLit {}) = True
isConcrete (LayoutLambda {}) = False
isConcrete (ApplyLayout {}) = False
isConcrete (App f xs) = all isConcrete xs

