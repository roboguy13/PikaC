{-# LANGUAGE LambdaCase #-}

module PikaC.Backend.SuSLik.SuSLang.ToC
  where

import qualified PikaC.Backend.SuSLik.SuSLang.Syntax as SuSLang
import qualified PikaC.Backend.C.Syntax as C

import PikaC.Backend.SuSLik.SuSLang.Syntax
import PikaC.Backend.C.Syntax

import PikaC.Syntax.Heaplet

import Unbound.Generics.LocallyNameless

import Control.Lens

functionToC :: Function -> [CFunction]
functionToC fn = runFreshM $ do
  (params, body) <- unbind $ functionBody fn
  body' <- traverse commandToC body
  pure 
    [CFunction
          { cfunctionName = functionName fn
          , cfunctionBody = body' -- map replaceCalls body'
          , cfunctionParams = map convertName params
          }
       -- ,wrapperFn fn
       ]

replaceCalls :: C.Command -> C.Command
replaceCalls = rewrite $ \case
  e@(C.Call ('_':_) _ _) -> Nothing
  C.Call f args1 args2 -> Just $ C.Call ('_' : f) args1 args2
  _ -> Nothing

-- TODO: Handle multiple result parameters
wrapperFn :: Function -> CFunction
wrapperFn fn = runFreshM $ do
  (params, body) <- unbind $ functionBody fn
  body' <- traverse commandToC body

  outParam <- convertName <$> fresh (last params)
  wrappedOut <- convertName <$> fresh (string2Name ("_" <> name2String outParam))
  wrappedInParams <- map convertName <$> mapM (fresh . string2Name . ("_" <>) . name2String) (init params)
  inParams <- map convertName <$> mapM fresh (init params)

  pure $
    CFunction
      { cfunctionName = functionName fn
      , cfunctionParams = inParams ++ [outParam]
      , cfunctionBody =
          [IntoMalloc 1 wrappedOut
            $ map Decl wrappedInParams
            ++ zipWith (\x y -> C.Assign (C.V x :+ 0) (C.V y)) wrappedInParams inParams
            ++
            [C.Call ("_" <> functionName fn)
              (map C.V wrappedInParams ++ [C.V wrappedOut])
              []
            -- ,C.SimpleAssign (C.V wrappedOut) (C.V wrappedOut)
            ,C.Assign (C.V outParam :+ 0) (C.V wrappedOut)
            ]
          ]
      }

commandToC :: Fresh m => SuSLang.Command -> m C.Command
commandToC (LetMalloc sz bnd) = do
  (var, body) <- unbind bnd
  bodyCmds <- traverse commandToC body
  pure $ IntoMalloc sz (convertName var) bodyCmds

commandToC (SuSLang.Let (LocVal loc) bnd) = do
  (var, body) <- unbind bnd
  bodyCmds <- traverse commandToC body
  pure $ C.Let (convertLoc loc) (convertName var) bodyCmds

commandToC (SuSLang.IfThenElse c t f) = do
  let c' = exprToC c
  t' <- traverse commandToC t
  f' <- traverse commandToC f
  pure $ C.IfThenElse c' t' f'

commandToC (SuSLang.Call f args) =
  let args' = map exprToC args
  in
  pure $ C.Call f args' []

commandToC (SuSLang.Write lhs rhs) =
  let lhs' = convertLoc lhs
      rhs' = exprToC rhs
  in
  pure $ C.Assign lhs' rhs'

commandToC (SuSLang.Assert c rest) = do
  rest' <- mapM commandToC rest
  pure $ C.Assert (exprToC c) rest'

commandToC (SuSLang.Free (SuSLang.V x)) =
  pure $ C.Free (convertName x)

exprToC :: SuSLang.Expr -> CExpr
exprToC (SuSLang.V x) = C.V $ convertName x
exprToC (SuSLang.IntLit i) = C.IntLit i
exprToC (SuSLang.BoolLit b) = C.BoolLit b
exprToC (SuSLang.Add x y) = C.Add (exprToC x) (exprToC y)
exprToC (SuSLang.Mul x y) = C.Mul (exprToC x) (exprToC y)
exprToC (SuSLang.Mod x y) = C.Mod (exprToC x) (exprToC y)
exprToC (SuSLang.Div x y) = C.Div (exprToC x) (exprToC y)
exprToC (SuSLang.Equal x y) = C.Equal (exprToC x) (exprToC y)
exprToC (SuSLang.Not x) = C.Not (exprToC x)
exprToC (SuSLang.Lt x y) = C.Lt (exprToC x) (exprToC y)
exprToC (SuSLang.Le x y) = C.Le (exprToC x) (exprToC y)
exprToC (SuSLang.And x y) = C.And (exprToC x) (exprToC y)
exprToC (SuSLang.LocVal loc) = C.LocValue (convertLoc loc)
exprToC (SuSLang.If c t f) = C.If (exprToC c) (exprToC t) (exprToC f)

convertLoc :: Loc SuSLang.Expr -> Loc CExpr
convertLoc (x :+ i) = exprToC x :+ i

convertName :: Name SuSLang.Expr -> Name CExpr
convertName = string2Name . name2String

