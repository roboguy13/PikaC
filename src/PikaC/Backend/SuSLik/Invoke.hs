module PikaC.Backend.SuSLik.Invoke
  where

import System.Process
import System.Exit

import PikaC.Ppr
import PikaC.Backend.SuSLik.Syntax
import PikaC.Backend.SuSLik.SuSLang.Parser
import PikaC.Backend.SuSLik.SuSLang.Syntax as SuSLang
import PikaC.Syntax.ParserUtils

import Text.Megaparsec (some)

suslikStdinOpt :: [String]
suslikStdinOpt = ["--stdin", "true"]

defaultSuslikOpts :: [String]
defaultSuslikOpts = suslikStdinOpt ++ ["--printSpecs","false","-b","true", "-c", "2"]
-- defaultSuslikOpts = suslikStdinOpt ++ ["--printSpecs","false","-b","true", "-c", "2", "-o","2"]
-- defaultSuslikOpts = suslikStdinOpt ++ ["-b","true", "-c", "2", "-o","2"]

suslikCmd :: String
suslikCmd = "./suslik.sh"

type SuSLikError = String

-- | In milliseconds
timeoutOpt :: Maybe Int -> [String]
timeoutOpt Nothing = []
timeoutOpt (Just millis) = ["--timeout", show millis]

invokeSuSLik :: [String] -> [InductivePredicate] -> [FnSig] -> FnSig -> IO (Either SuSLikError [SuSLang.Function])
invokeSuSLik = invokeSuSLikWithTimeout Nothing

invokeSuSLikWithTimeout :: Maybe Int -> [String] -> [InductivePredicate] -> [FnSig] -> FnSig -> IO (Either SuSLikError [SuSLang.Function])
invokeSuSLikWithTimeout maybeTimeout susOpts0 indPreds helperSigs sigToSynth = do
  let susOpts = defaultSuslikOpts ++ susOpts0 ++ timeoutOpt maybeTimeout

      indPredCode = vcat (map ppr indPreds)
      helperCode = vcat (map pprFnSigPrototype helperSigs)
      sigToSynthCode = ppr sigToSynth

      suslikCode = render (indPredCode $$ helperCode $$ sigToSynthCode)

  -- putStrLn suslikCode
  (exitCode, suslikOut, stderrOut) <- readCreateProcessWithExitCode (proc suslikCmd susOpts) suslikCode

  case exitCode of
    ExitSuccess -> do
      -- putStrLn suslikOut
      pure $ Right $ parse' (some parseFunction) suslikOut
    ExitFailure n -> pure $ Left stderrOut

