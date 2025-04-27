(lang dune 3.17)
(using directory-targets 0.1)
(cram enable)

; https://stackoverflow.com/questions/70665781/how-to-use-flambda-with-dune-in-ocaml
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
  (ocaml
   (= 5.2.1))
  dune
  (core
   (= v0.17.1))
  (core_bench
   (= v0.17.0))
  (core_kernel
   (= v0.17.0))
  (core_unix
   (= v0.17.0))
  ; needed by cmd
  stdio
  spawn
  (containers
   (= 3.15))
  (containers-data
    (= 3.15))
  (ppx_deriving
   (= 6.0.3))
  (ppx_jane
   (= v0.17.0))
  (ppx_partial
   (= 1.0))
  (zarith
   (= 1.14))
  (digestif
   (= 1.2.0))
  (base64
   (= 3.5.1))
  menhir
  (shexp (= v0.17.0))
  )
 (tags
  ("add topics" "to describe" your project))
  )

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
