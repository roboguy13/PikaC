module PikaC.Tests.Pika.Test
  where

import           PikaC.Syntax.PikaCore.Expr
import           PikaC.Syntax.PikaCore.FnDef

import           PikaC.Syntax.Pika.Expr (Test (..))

import           PikaC.Syntax.Pika.Layout
import           PikaC.Syntax.Pika.Pattern
import           PikaC.Syntax.Type
import           PikaC.Syntax.Heaplet

import           PikaC.Ppr

import           Unbound.Generics.LocallyNameless
import           Unbound.Generics.LocallyNameless.Unsafe

import qualified PikaC.Backend.C.Syntax as C
import           PikaC.Backend.C.CodeGen
import           PikaC.Backend.C.Monad
import           PikaC.Backend.Utils

import           PikaC.Tests.Pika.Printer

import Debug.Trace

genTestMain :: [Layout Expr] -> [Test Expr] -> C.CFunction
genTestMain layouts tests = runGenC $ do
  mainBody <- concat <$> mapM (runTest layouts) tests
  pure $ C.CFunction
    { C.cfunctionName = "main"
    , C.cfunctionParams = []
    , C.cfunctionBody = mainBody
    }

runTest :: [Layout Expr] -> Test Expr -> GenC [C.Command]
runTest layouts test = do
  testCmds <- runTest' layouts (_testResultType test) (_testExpr test)
  pure $
    C.Printf ("*** Running test " ++ _testName test ++ "\\n") []
    : testCmds

runTest' :: [Layout Expr] -> Type -> Expr -> GenC [C.Command]
runTest' layouts ty e = do
  outVars <- mkOutVars layouts ty
  runTestWithPrinter outVars (getPrinter ty) e

runTestWithPrinter :: [C.CName] -> Printer -> Expr -> GenC [C.Command]
runTestWithPrinter outVars printer e = do
  body <- convertExprCmds outVars e
  pure $
    body ++
    [call printer (map C.V outVars)
    ]

call :: FnName -> [C.CExpr] -> C.Command
call (FnName n) args = C.Call n args []

mkOutVars :: [Layout Expr] -> Type -> GenC [C.CName]
mkOutVars _ IntType = (:[]) <$> fresh (string2Name "i")
mkOutVars _ BoolType = (:[]) <$> fresh (string2Name "b")
mkOutVars layouts (TyVar x) =
  let vs = layoutParamNames $ lookupLayout layouts (name2String x)
  in
  mapM fresh vs

convertExprCmds :: [C.CName] -> Expr -> GenC [C.Command]
convertExprCmds outVars = enterBranchBody (convertBranchBody outVars)

