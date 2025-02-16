# Tests

## Cram Test

Cram tests are in `compiler/cram_test/**`. An example is in `simple_arithmetic.t`. These tests build the program with the `c0` executable and run them with expected output.

These are meant to test the whole compiler pipeline.

## Filetests

Similar to cram tests, but more domain specific. Haven't implemeneted these yet. I plan to use `dune` rule generation to generate these.

## Unit Tests

Unit tests are in `compiler/test`. Most tests use [ppx_expect](https://github.com/janestreet/ppx_expect)