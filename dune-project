(lang dune 2.8)

(generate_opam_files true)

(package
 (name ppx_locs)
 (depends
  (ocaml
   (= "4.12"))
  reason
  ppxlib))
