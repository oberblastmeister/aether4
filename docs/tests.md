# Tests

## Cram Test

Cram tests are in `compiler/cram_test/**`. An example is in `simple_arithmetic.t`. These tests build the program with the `c0` executable and run them with expected output.

These are meant to test the whole compiler pipeline.

Each `<dir>.t` directory defines a testing directory. This directory contains a `run.t` file that executes the compiler on the file, and diffs checks if the output was different.

## Filetests

Similar to cram tests, but more domain specific. Haven't implemeneted these yet. I plan to use `dune` rule generation to generate these.

Run a specific test like this:
```sh
dune build @@compiler/filetests/filetests/run/first
```

The `@@` means non recursive, i.e. exactly this path.

The path is in `run` because we use this subdir when doing `dynamic_include`.


## Unit Tests

Unit tests are in `compiler/test`. Most tests use [ppx_expect](https://github.com/janestreet/ppx_expect)