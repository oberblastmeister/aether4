(lang dune 3.17)
(using directory-targets 0.1)
(cram enable)

(name aether4)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name aether4)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  dune
  (core
   (= v0.17.1))
  (core_kernel
   (= v0.17.0))
  (core_unix
   (= v0.17.0))
  (ppx_deriving
   (= 6.0.3))
  (ppx_jane
   (= v0.17.0))
  (alcotest
   (= 1.7.0))
  (algaeff
   (= 2.0.0))
  (asai
   (= 0.3.1))
  (bwd
   (= 2.3.0))
  (zarith
   (= 1.14))
  (cmdliner
   (= 1.3.0))
  (eio
   (= 1.2))
  (eio_main
   (= 1.2))
  (digestif
   (= 1.2.0))
  (base64
   (= 3.5.1))
  menhir
  )
 (tags
  ("add topics" "to describe" your project))
  )

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
