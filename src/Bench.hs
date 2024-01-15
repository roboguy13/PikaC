module Main
  where

import PikaC.Benchmark.Benchmark
import Criterion
import Criterion.Main

main :: IO ()
main = do
  benchmarks <- parseBenchmarks benchmarkConfigPath
  results <- runBenchmarks benchmarks
  putStrLn $ toLaTeX results
  -- defaultMain [runBenchmarks benchmarks]

