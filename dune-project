(lang dune 3.15)

(name flatcalc)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name flatcalc)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  (ppx_jane
   (>= v0.17.0))
  (base_quickcheck
   (>= v0.17.0))
  (base
   (>= v0.17.1))
  ocaml
  dune)
 (tags
  (topics "to describe" your project)))