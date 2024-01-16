module Main
  where

import PikaC.Benchmark.Benchmark
import Criterion
import Criterion.Main

import Control.DeepSeq

import Data.Maybe (catMaybes)

main :: IO ()
main = do
  benchmarks <- parseBenchmarks benchmarkConfigPath

  putStrLn "compiling benchmarks..."

  let compiledBenchmarks0 = map compileBenchmark benchmarks

  compiledBenchmarks0 `deepseq` putStrLn "...benchmarks compiled."

  putStrLn "synthing benchmarks..."
  synthedBenchmarks <- traverse synthBenchmark compiledBenchmarks0
  putStrLn "...benchmarks synthed."

  putStrLn "generating C benchmarks..."
  cbenchResults <- runCBenchmarks (catMaybes synthedBenchmarks)
  putStrLn "...C benchmarks generated."

  putStrLn $ cBenchmarkToLaTeX $ cbenchResults


  (compiledBenchmarks, results) <- runBenchmarks benchmarks
  putStrLn $ toLaTeX results
  -- print results
  -- defaultMain [runBenchmarks benchmarks]

