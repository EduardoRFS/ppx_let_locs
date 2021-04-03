(lang dune 2.8)

(generate_opam_files true)

(package
 (name ppx_let_locs)
 (depends
  (ocaml
   (and
    (>= "4.12.0")
    (< "4.13.0")))
  reason
  ppxlib))
