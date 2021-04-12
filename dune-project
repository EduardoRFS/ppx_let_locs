(lang dune 2.8)

(name ppx_let_locs)

(generate_opam_files true)

(package
 (name ppx_let_locs)
 (depends
  (ocaml
   (and
    (>= "4.08.0")
    (< "4.13.0")))
  reason
  ppxlib
  ppx_optcomp
  ocaml-migrate-types))
