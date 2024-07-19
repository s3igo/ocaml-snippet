(lang dune 3.16)
(name ocaml_snippet)

(generate_opam_files true)

(package
 (name ocaml_snippet)
 (depends
  (ppxlib (>= 0.32.1))
  (yojson (>= 2.2.2))))
