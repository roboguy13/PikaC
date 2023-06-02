{-# LANGUAGE DeriveGeneric #-}

module PikaC.Backend.SuSLik.Syntax
  where

import PikaC.Ppr
import PikaC.Utils

import PikaC.Syntax.Heaplet

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Unsafe

import GHC.Generics

import Data.Validity

type ExprName = Name Expr

data Expr
  = V ExprName
  | IntLit Int
  | BoolLit Bool
  | Add Expr Expr
  | Sub Expr Expr
  | Equal Expr Expr
  | And Expr Expr
  | Not Expr
  deriving (Show, Generic)

instance HasVar Expr where mkVar = V

instance IsBase Expr where
  isVar (V {}) = True
  isVar _ = False

  isLit (IntLit {}) = True
  isLit (BoolLit {}) = True
  isLit _ = False

  intLit = IntLit
  boolLit = BoolLit
  mkNot = Not
  mkEqual = Equal
  mkAnd x (BoolLit True) = x
  mkAnd x y = And x y

data InductivePredicate
  = InductivePredicate
    { _indPredName :: String
    , _indPredBody ::
        Bind [ExprName]
          [PredicateBranch]
    }
  deriving (Show, Generic)

data HeapletS
  = PointsToS (PointsTo Expr)
  | ApplyS String [Expr]
  deriving (Show, Generic)

newtype ExistVar = ExistVar { getExistVar :: ExprName }
  deriving (Show, Generic)

type Assertion =
  Bind [ExistVar] [HeapletS]

data PredicateBranch
  = PredicateBranch
    { _predBranchCond :: Expr
    , _predBranchPure :: Expr
    , _predBranchAssertion :: Assertion
    }
  deriving (Show, Generic)

data FnSig
  = FnSig
    { _fnSigName :: String
    , _fnSigConds ::
        Bind [ExprName] -- Function parameters
          (Bind [ExistVar]
            FnSpec)
    }
  deriving (Show, Generic)

data FnSpec
  = FnSpec
    { _fnSpecPrecond :: [HeapletS]
    , _fnSpecPostcond :: Assertion
    }
  deriving (Show, Generic)

instance Alpha ExistVar
instance Alpha HeapletS
instance Alpha Expr
instance Alpha PredicateBranch
instance Alpha FnSpec

instance Subst Expr Expr where
  isvar (V n) = Just $ SubstName n
  isvar _ = Nothing

instance Subst Expr HeapletS where
instance Subst Expr a => Subst Expr (PointsTo a)
instance Subst Expr a => Subst Expr (Loc a)

instance Ppr Expr where
  ppr (V x) = ppr x
  ppr (IntLit i) = ppr i
  ppr (BoolLit True) = text "true"
  ppr (BoolLit False) = text "false"
  ppr (Add x y) = sep [pprP x, text "+", pprP y]
  ppr (Sub x y) = sep [pprP x, text "-", pprP y]
  ppr (Equal x y) = sep [pprP x, text "==", pprP y]
  ppr (Not x) = text "!" <> pprP x
  ppr (And x y) = sep [pprP x, text "&&", pprP y]

instance IsNested Expr where
  isNested (V _) = False
  isNested (IntLit _) = False
  isNested (BoolLit _) = False
  isNested _ = True

instance Ppr HeapletS where
  ppr (PointsToS p) = ppr p
  ppr (ApplyS f args) =
    text f <> text "(" <> hsep (punctuate (text ",") (map ppr args))  <> text ")"

instance Ppr PredicateBranch where
  ppr branch =
    let purePart rest =
          case _predBranchPure branch of
            BoolLit True -> rest
            p -> ppr p <+> text ";" <+> rest

        (_, asn) = unsafeUnbind $ _predBranchAssertion branch
        heapletsText =
          case asn of
            [] -> text "emp"
            _ -> (sep (punctuate (text " **")
                       (map ppr asn)))
    in
    text "|" <+>
    ppr (_predBranchCond branch)
      <+> text "=>"
      <+>
      nest 2
        (text "{"
          <+> purePart heapletsText
          <+> text "}"
        )

instance Ppr InductivePredicate where
  ppr indPred =
    let (params, branches) = unsafeUnbind (_indPredBody indPred)
    in
    vcat $
      [(text "predicate" <+> text (_indPredName indPred))
        <> text "(" <> hsep (punctuate (text ",") (map ((text "loc" <+>) . ppr) params)) <> text ")"
      ,text "{"
      ]
      ++
        map ppr branches
      ++
      [text "}"]

instance Ppr FnSpec where
  ppr fnSpec =
    let (_, postCond) = unsafeUnbind $ _fnSpecPostcond fnSpec
    in
    vcat
      [ braces (sep (punctuate (text " **") (map ppr (_fnSpecPrecond fnSpec))))
      , braces (sep (punctuate (text " **") (map ppr postCond)))
      ]

instance Ppr FnSig where
  ppr fnSig =
    let (params, bnd) = unsafeUnbind $ _fnSigConds fnSig
        (_, conds) = unsafeUnbind bnd
    in
    vcat
      [ (text "void" <+> text (_fnSigName fnSig))
          <> parens (sep (punctuate (text ",") (map ppr params)))
      , nest 2 (ppr conds)
      , text "{ ?? }"
      ]
    

