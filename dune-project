(lang dune 2.8)

(name ppx_let_locs)

(generate_opam_files true)

(package
 (name ppx_let_locs)
 (depends
  (ocaml
   (or
    (and
     (>= "4.12.0")
     (< "4.13.0"))
    (and
     (>= "4.10.0")
     (< "4.11.0"))))
  reason
  ppxlib
  ppx_optcomp))
