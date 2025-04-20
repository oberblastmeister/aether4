dev:
	opam switch create -y . 5.2.1 --deps-only --with-test --with-doc
	opam install -y dune merlin ocamlformat utop ocaml-lsp-server patdiff

deps:
	opam install -y . --deps-only

deps_opt:
	opam switch create . ocaml-variants.5.2.1+options ocaml-option-flambda --deps-only

dev_fuzz:
	opam switch create -y . ocaml-variants.5.2.1+options ocaml-option-afl --deps-only --with-test --with-doc

watch:
	dune test --watch --terminal-persistence=clear-on-rebuild
