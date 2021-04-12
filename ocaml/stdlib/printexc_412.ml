include Printexc

let use_printers: exn -> string option =
  fun exn -> 
    match () with
    | () [@if ocaml_version >= (4, 09, 0)] -> use_printers exn
    | () [@if ocaml_version < (4, 09, 0)] -> None
[@@ocaml.warning "-27"]
