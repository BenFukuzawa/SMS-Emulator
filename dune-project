(lang dune 3.0)

(generate_opam_files true)

(maintainers "TODO")

(authors "TODO")

(license MIT)

(package
 (name sandbox)
 (allow_empty)
 (synopsis "A blank OCaml project in the Jane Street style")
 (depends
  (ocaml
   (>= 5.2.0))
  dune
  (wasm_of_ocaml-compiler
   (>= 6.0.1))
  ocaml-lsp-server
  ocamlformat
  (odoc :with-doc)
  core
  stdint
  ppx_jane
  (expect_test_helpers_core :with-test)))
