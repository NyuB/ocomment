(lang dune 3.7)

(name ocomment)

(generate_opam_files true)

(source
 (github NyuB/ocomment))

(authors "Brice Decaestecker")

(maintainers "Brice Decaestecker")

(license LICENSE)

(documentation https://github.com/NyuB/ocomment)

(package
 (name ocomment)
 (synopsis "Comments checksum verifier")
 (description "Keep your comment up to date with code changes")
 (depends 
  (ocaml (>= 5.0))
  dune
  sexplib
  ppx_sexp_conv
  (alcotest :with-test)
  (ocamlformat :with-test)
  (ppx_expect :with-test))
 (tags
  (documentation ci)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
