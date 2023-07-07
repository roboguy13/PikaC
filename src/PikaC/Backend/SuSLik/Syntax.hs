{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}

module PikaC.Backend.SuSLik.Syntax
  where

import PikaC.Ppr
import PikaC.Utils

import PikaC.Syntax.Heaplet
import PikaC.Syntax.Type
import PikaC.Syntax.Pika.Layout (GhostType (..))

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Unsafe

import GHC.Generics
import GHC.Stack

import Data.Validity

import Debug.Trace

type ExprName = Name Expr

data Expr
  = V ExprName
  | IntLit Int
  | BoolLit Bool
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Equal Expr Expr
  | Lt Expr Expr
  | Le Expr Expr
  | And Expr Expr
  | Not Expr

  -- For the ghost language:
  | EmptySet
  | SingletonSet Expr
  | SetUnion Expr Expr
  deriving (Show, Generic)

getV :: HasCallStack => Expr -> ExprName
getV (V x) = x

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
    , _indPredArgTypes :: [Type]
    , _indPredGhostTypes :: [GhostType]
    , _indPredResultType :: Type
    , _indPredBody ::
        -- Bind [ExprName]
          ([ExprName], [PredicateBranch])
    }
  deriving (Show, Generic)

data HeapletS
  = PointsToS (PointsTo Expr)
  | ReadOnlyPointsToS (PointsTo Expr)
  | ApplyS String [Expr]
  | RecApply String [Expr]
  | BlockS (Name Expr) Int
  deriving (Show, Generic)

newtype ExistVar = ExistVar { getExistVar :: ExprName }
  deriving (Show, Generic)

type Assertion = [HeapletS]
  -- Bind [ExistVar] [HeapletS]

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
    , _fnSigArgTypes :: [Type]
    , _fnSigResultType :: Type
    , _fnSigConds ::
        ([ExprName] -- Function parameters
        ,FnSpec)
        -- Bind [ExprName] -- Function parameters
        --   (Bind [ExistVar]
        --     FnSpec)
    }
  deriving (Show, Generic)

data FnSpec
  = FnSpec
    { _fnSpecPrecond :: [HeapletS]
    , _fnSpecPostcond ::
        (Expr -- Pure part
        ,Assertion)
    }
  deriving (Show, Generic)

pprFnSigPrototype :: FnSig -> Doc
pprFnSigPrototype fnSig =
    let (params, conds) = _fnSigConds fnSig
        argTypes = _fnSigArgTypes fnSig ++ [LayoutId "Unused"] -- [_fnSigResultType fnSig]
        -- conds = unsafeUnbind bnd
    in
    vcat
      [ (text "void" <+> text (_fnSigName fnSig))
          <> parens (sep (punctuate (text ",") (zipWith showParam (map toSuSLikType argTypes) params)))
      , nest 2 (ppr conds)
      ]

instance Alpha ExistVar
instance Alpha HeapletS
instance Alpha Expr
instance Alpha PredicateBranch
instance Alpha FnSpec

instance Subst Expr Expr where
  isvar (V n) = Just $ SubstName n
  isvar _ = Nothing

instance Subst a Expr => Subst a HeapletS

-- instance Subst Expr HeapletS where
-- instance Subst Expr a => Subst Expr (PointsTo a)
-- instance Subst Expr a => Subst Expr (Loc a)

instance Ppr Expr where
  ppr (V x) = ppr x
  ppr (IntLit i) = ppr i
  ppr (BoolLit True) = text "1"
  ppr (BoolLit False) = text "0"
  ppr (Add x y) = sep [pprP x, text "+", pprP y]
  ppr (Mul x y) = sep [pprP x, text "*", pprP y]
  ppr (Sub x y) = sep [pprP x, text "-", pprP y]
  ppr (Equal x y) = sep [pprP x, text "==", pprP y]
  ppr (Lt x y) = sep [pprP x, text "<", pprP y]
  ppr (Le x y) = sep [pprP x, text "<=", pprP y]
  ppr (Not x) = text "not " <> pprP x
  ppr (And x y) = sep [pprP x, text "&&", pprP y]
  ppr EmptySet = text "{}"
  ppr (SingletonSet x) = text "{" <> ppr x <> text "}"
  ppr (SetUnion x y) = pprP x <+> text "++" <+> pprP y

instance IsNested Expr where
  isNested (V _) = False
  isNested (IntLit _) = False
  isNested (BoolLit _) = False
  isNested EmptySet = False
  isNested (Equal {}) = True
  isNested (SetUnion {}) = False
  isNested (SingletonSet {}) = False
  isNested _ = True

instance Ppr HeapletS where
  ppr (PointsToS p) = ppr p
  ppr (ReadOnlyPointsToS (lhs :-> rhs)) =
    ppr lhs <+> text ":=>" <+> ppr rhs
  ppr (BlockS n sz) = text "[" <> text (show n) <> text "," <> text (show sz) <> text "]"
  ppr (RecApply f args) =
    text f <> text "(" <> hsep (punctuate (text ",") (map ppr args))  <> text ")"
  ppr (ApplyS f args) =
    text "func " <> text f <> text "(" <> hsep (punctuate (text ",") (map ppr args))  <> text ")"

instance Ppr PredicateBranch where
  ppr branch =
    let purePart rest =
          case _predBranchPure branch of
            BoolLit True -> rest
            p -> ppr p <+> text ";" <+> rest

        asn = _predBranchAssertion branch
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

toSuSLikType :: Type -> Doc
toSuSLikType IntType = text "int"
toSuSLikType BoolType = text "bool"
toSuSLikType (LayoutId _) = text "loc"
toSuSLikType t = error $  "toSuSLikType: " ++ show t

ghostToSuSLikType :: GhostType -> Doc
ghostToSuSLikType IntGhost = text "int"
ghostToSuSLikType SetGhost = text "set"

showParam :: Doc -> ExprName -> Doc
showParam ty param = ty <+> text (show param)

instance Ppr InductivePredicate where
  ppr indPred =
    let (params, branches) = _indPredBody indPred
        argTypes = _indPredArgTypes indPred -- ++ [TyVar (string2Name "unused")] --[_indPredResultType indPred]
        ghostTypes = _indPredGhostTypes indPred
    in
    vcat $
      [(text "predicate" <+> text (_indPredName indPred))
        <> text "(" <> hsep (punctuate (text ",") (zipWith showParam (map toSuSLikType argTypes ++ map ghostToSuSLikType ghostTypes) params)) <> text ")"
      ,text "{"
      ]
      ++
        map ppr branches
      ++
      [text "}"]

instance Ppr FnSpec where
  ppr fnSpec =
    let (purePart, postCond) = _fnSpecPostcond fnSpec
        purePartDoc =
          case purePart of
            BoolLit True -> mempty
            _ -> ppr purePart <+> text ";"
    in
    vcat
      [ braces (sep (punctuate (text " **") (map ppr (_fnSpecPrecond fnSpec))))
      , braces (purePartDoc <+> sep (punctuate (text " **") (map ppr postCond)))
      ]

instance Ppr FnSig where
  ppr fnSig =
    vcat
      [ pprFnSigPrototype fnSig
      , text "{ ?? }"
      ]
andS :: Expr -> Expr -> Expr
andS (BoolLit True) x = x
andS x (BoolLit True) = x
andS x y = And x y

