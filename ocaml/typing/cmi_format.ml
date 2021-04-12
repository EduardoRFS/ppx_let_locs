(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Fabrice Le Fessant, INRIA Saclay                     *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Misc

type pers_flags =
  | Rectypes
  | Alerts of alerts
  | Opaque
  | Unsafe_string

type error =
  | Not_an_interface of filepath
  | Wrong_version_interface of filepath * string
  | Corrupted_interface of filepath

exception Error of error

(* these type abbreviations are not exported;
   they are used to provide consistency across
   input_value and output_value usage. *)
type signature = Types.signature_item list
type flags = pers_flags list
type header = modname * signature

type cmi_infos = {
    cmi_name : modname;
    cmi_sign : signature;
    cmi_crcs : crcs;
    cmi_flags : flags;
}

let input_cmi ic =
  let (name, sign) = (input_value ic : header) in
  let crcs = (input_value ic : crcs) in
  let flags = (input_value ic : flags) in
  {
      cmi_name = name;
      cmi_sign = sign;
      cmi_crcs = crcs;
      cmi_flags = flags;
    }

(*
let read_cmi filename =
  let ic = open_in_bin filename in
  try
    let buffer =
      really_input_string ic (String.length Config.cmi_magic_number)
    in
    if buffer <> Config.cmi_magic_number then begin
      close_in ic;
      let pre_len = String.length Config.cmi_magic_number - 3 in
      if String.sub buffer 0 pre_len
          = String.sub Config.cmi_magic_number 0 pre_len then
      begin
        let msg =
          if buffer < Config.cmi_magic_number then "an older" else "a newer" in
        raise (Error (Wrong_version_interface (filename, msg)))
      end else begin
        raise(Error(Not_an_interface filename))
      end
    end;
    let cmi = input_cmi ic in
    close_in ic;
    cmi
  with End_of_file | Failure _ ->
      close_in ic;
      raise(Error(Corrupted_interface(filename)))
    | Error e ->
      close_in ic;
      raise (Error e)
*)

module Of_ocaml = struct
  open! Migrate_types

  let copy_signature (signature: Ocaml_common.Types.signature): Types.signature =
    (* TODO: improve this by a lot *)
    let module Transformations = struct
      let signature: Ocaml_common.Types.signature = Obj.magic signature
      [%%if ocaml_version < (4, 09, 0)]
      let signature = Migrate_408_409.copy_signature (Obj.magic signature)
      [%%endif]
      [%%if ocaml_version < (4, 10, 0)]
      let signature = Migrate_409_410.copy_signature (Obj.magic signature)
      [%%endif]
      [%%if ocaml_version < (4, 11, 0)]
      let signature = Migrate_410_411.copy_signature (Obj.magic signature)
      [%%endif]
      [%%if ocaml_version < (4, 12, 0)]
      let signature = Migrate_411_412.copy_signature (Obj.magic signature)
      [%%endif]
      let signature: Types.signature = Obj.magic signature
    end in
    Transformations.signature

  let copy_flags: Ocaml_common.Cmi_format.pers_flags -> pers_flags = Obj.magic
  let copy_cmi_infos Ocaml_common.Cmi_format.{ cmi_name; cmi_sign; cmi_crcs; cmi_flags } = 
    {
      cmi_name;
      cmi_sign = copy_signature cmi_sign;
      cmi_crcs;
      cmi_flags = List.map copy_flags cmi_flags;
    }
end
let read_cmi filename =
  let open Ocaml_common in
  Of_ocaml.copy_cmi_infos (Cmi_format.read_cmi filename)

let output_cmi filename oc cmi =
(* beware: the provided signature must have been substituted for saving *)
  output_string oc Config.cmi_magic_number;
  output_value oc ((cmi.cmi_name, cmi.cmi_sign) : header);
  flush oc;
  let crc = Digest.file filename in
  let crcs = (cmi.cmi_name, Some crc) :: cmi.cmi_crcs in
  output_value oc (crcs : crcs);
  output_value oc (cmi.cmi_flags : flags);
  crc

(* Error report *)

open Format

let report_error ppf = function
  | Not_an_interface filename ->
      fprintf ppf "%a@ is not a compiled interface"
        Location.print_filename filename
  | Wrong_version_interface (filename, older_newer) ->
      fprintf ppf
        "%a@ is not a compiled interface for this version of OCaml.@.\
         It seems to be for %s version of OCaml."
        Location.print_filename filename older_newer
  | Corrupted_interface filename ->
      fprintf ppf "Corrupted compiled interface@ %a"
        Location.print_filename filename

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
