export BENCHMARKS_RUNNER=TRUE
export BENCH_LIB=bench
exec dune exec -- ./main.exe -fork -run-without-cross-library-inlining "$@"