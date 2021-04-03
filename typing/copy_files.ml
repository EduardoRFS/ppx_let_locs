let (major, minor) = 
  match String.split_on_char '.' Sys.ocaml_version with
  | major :: minor :: _ -> (major, minor)
  | _ -> assert false
let version = major ^ minor
let files =
  List.concat_map
    (fun str -> [
      (Printf.sprintf "%s_%s.ml" str version), str ^ ".ml";
      (Printf.sprintf "%s_%s.mli" str version), str ^ ".mli"
    ])
    ["typeclass"; "typecore"; "typemod"]

let () =
  List.iter
    (fun (from, to_) -> 
      assert (Sys.command (Printf.sprintf "cp %s %s" from to_) == 0))
  files
