(lang dune 3.16)
(name ocaml_snippet)
(version 0.1.0)

(generate_opam_files true)

(package
 (name ocaml_snippet)
 (depends
  (cmdliner (>= 1.3.0))
  (ppxlib (>= 0.32.1))
  (yojson (>= 2.2.2))
 ;  (alcotest (and :with-test (>= 1.7.0)))
  ))
