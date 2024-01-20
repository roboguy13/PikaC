# Quick start

To run the tests:

    $ ./run-tests.sh

To run the benchmarks:

    $ ./run-benchmarks.sh

There are two sets of benchmarks. Both will be run by this command. They are:

1. The generated C vs GHC Haskell comparison
2. The Pika compile time vs SuSLik synthesis time comparison

Two graphs will be produced for the first set: one graph for list benchmarks and one for non-list benchmarks. For both benchmarks
the data are presented as a LaTeX table printed to standard output. There is information about each benchmark printed while the suite
is running and these tables are printed at the end.

# Details

`run-tests.sh` will automatically run `cabal test --test-show-detail=streaming`.

To run a specific test, for example `even`:

    $ cabal test --test-show-detail=streaming --test-options="-p /even/"

