module PikaC.Backend.SuSLik.Invoke
  where

import System.Process
import System.Exit

import PikaC.Ppr
import PikaC.Backend.SuSLik.Syntax
import PikaC.Backend.SuSLik.SuSLang.Parser
import PikaC.Backend.SuSLik.SuSLang.Syntax as SuSLang
import PikaC.Syntax.ParserUtils

suslikStdinOpt :: [String]
suslikStdinOpt = ["--stdin", "true"]

defaultSuslikOpts :: [String]
defaultSuslikOpts = suslikStdinOpt ++ ["-b","true"]

suslikCmd :: String
suslikCmd = "./suslik.sh"

type SuSLikError = String

invokeSuSLik :: [String] -> [InductivePredicate] -> [FnSig] -> FnSig -> IO (Either SuSLikError SuSLang.Function)
invokeSuSLik susOpts0 indPreds helperSigs sigToSynth = do
  let susOpts = defaultSuslikOpts ++ susOpts0

      indPredCode = vcat (map ppr indPreds)
      helperCode = vcat (map pprFnSigPrototype helperSigs)
      sigToSynthCode = ppr sigToSynth

      suslikCode = render (indPredCode $$ helperCode $$ sigToSynthCode)

  (exitCode, suslikOut, stderrOut) <- readCreateProcessWithExitCode (proc suslikCmd susOpts) suslikCode

  case exitCode of
    ExitSuccess ->
      pure $ Right
        $ parse' parseFunction
          -- Drop the initial 2 lines, which just give the pre- and post-condition
        $ unlines $ drop 2 $ lines $ suslikOut
    ExitFailure n -> pure $ Left stderrOut

