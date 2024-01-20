# Quick start

To run the tests:

    $ ./run-tests.sh

To run the benchmarks:

    $ ./run-benchmarks

# Details

`run-tests.sh` will automatically run `cabal test --test-show-detail=streaming`.

To run a specific test, for example `even`:

    $ cabal test --test-show-detail=streaming --test-options="-p /even/"

