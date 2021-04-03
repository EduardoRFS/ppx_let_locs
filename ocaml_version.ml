let (major, minor) = 
  match String.split_on_char '.' Sys.ocaml_version with
  | major :: minor :: _ -> (major, minor)
  | _ -> assert false
let version = major ^ minor
let () = print_string version
