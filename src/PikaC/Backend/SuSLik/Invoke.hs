module PikaC.Backend.SuSLik.Invoke
  where

import System.Process
import System.Exit

import PikaC.Ppr
import PikaC.Backend.SuSLik.Syntax

suslikStdinOpt :: [String]
suslikStdinOpt = ["--stdin", "true"]

suslikCmd :: String
suslikCmd = "./suslik.sh"

type SuSLikError = String

invokeSuSLik :: [String] -> [InductivePredicate] -> [FnSig] -> FnSig -> IO (Either SuSLikError String)
invokeSuSLik susOpts0 indPreds helperSigs sigToSynth = do
  let susOpts = suslikStdinOpt ++ susOpts0

      indPredCode = vcat (map ppr indPreds)
      helperCode = vcat (map pprFnSigPrototype helperSigs)
      sigToSynthCode = ppr sigToSynth

      suslikCode = render (indPredCode $$ helperCode $$ sigToSynthCode)

  (exitCode, suslikOut, stderrOut) <- readCreateProcessWithExitCode (proc suslikCmd susOpts) suslikCode

  case exitCode of
    ExitSuccess -> pure $ Right suslikOut
    ExitFailure n -> pure $ Left stderrOut

