module Main
  where
import PikaC.Benchmark.Benchmark
import Criterion
import Criterion.Main

import Control.DeepSeq
import Control.Monad

import Data.Maybe (catMaybes)

performSanityCheck :: Bool
performSanityCheck = False

main :: IO ()
main = do
  benchmarks <- parseBenchmarks benchmarkConfigPath

  putStrLn "\ncompiling benchmarks..."

  let compiledBenchmarks0 = map compileBenchmark benchmarks

  compiledBenchmarks0 `deepseq` putStrLn "...benchmarks compiled."
  putStrLn ""

  putStrLn "synthing benchmarks..."
  synthedBenchmarks <- traverse synthBenchmark compiledBenchmarks0
  putStrLn "...benchmarks synthed."
  putStrLn ""

  when performSanityCheck $ do
    putStrLn "comparing C benchmark outputs with Haskell benchmark outputs..."
    runCBenchmarks SanityCheck CUnoptimized HaskellUnoptimized (catMaybes synthedBenchmarks)
    putStrLn "...benchmark outputs match."

  putStrLn ""
  putStrLn "generating C benchmarks..."
  cbenchResultsUnoptimized <- runCBenchmarks NoDiff CUnoptimized HaskellUnoptimized (catMaybes synthedBenchmarks)
  cbenchResultsOptimized <- runCBenchmarks NoDiff CO3 HaskellO2 (catMaybes synthedBenchmarks)
  putStrLn "...C benchmarks generated."

  let printCBenchResults = do
        putStrLn "-- Unoptimized --"
        putStrLn $ cBenchmarkToLaTeX $ cbenchResultsUnoptimized
        putStrLn ""
        putStrLn "-- C with -O3 and Haskell with -O2 --"
        putStrLn $ cBenchmarkToLaTeX $ cbenchResultsOptimized

  printCBenchResults
  putStrLn ""

  (compiledBenchmarks, results) <- runBenchmarks benchmarks

  putStrLn ""
  putStrLn $ toLaTeX results
  putStrLn ""
  putStrLn ""
  printCBenchResults

