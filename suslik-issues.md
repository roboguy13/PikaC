All of the tests work when using the backend that translates directly to C. Most of the tests do not work when translating to SuSLik.

| Test name | Status |
| --------- | ------ |
| add1Head  | Incorrect |
| anagram   | Does not synthesize |
| append    | Does not synthesize |
| fact      | Internal error in translator |
| filterLt  | Incorrect |
| heap | Does not synthesize |
| leftList | Incorrect |
| mapAdd | Working |
| maximum | First two tests work, third test incorrect |
| reverse | Does not synthesize |
| set | Does not synthesize |
| sum | Internal error in translator |
| take | Does not synthesize |
| treeSize | Internal error in translator |

# Instructions

To run a test using SuSLik:

    ./pika.sh tests/add1Head.pika --run-suslang-tests

To show the generated SuSLik specification (without showing the C translation from the Pika compiler), run:

    ./pika.sh tests/add1Head.pika --no-c

To run a test using the generated C code:

    ./pika.sh tests/add1Head.pika --run-tests

For further options:

    ./pika --help

