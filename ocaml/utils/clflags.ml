include Ocaml_common.Clflags

let locations = ref true                (* -d(no-)locations *)

let insn_sched_default = true
let insn_sched = ref insn_sched_default (* -[no-]insn-sched *)

let with_runtime = ref true;;         (* -with-runtime *)

let function_sections = ref false      (* -function-sections *)

let output_complete_executable = ref false

and plugin = ref false                  (* -plugin ... *)

type 'a env_reader = {
  parse : string -> 'a option;
  print : 'a -> string;
  usage : string;
  env_var : string;
}

let color = ref None (* -color *)

let color_reader = {
  parse = (function
    | "auto" -> Some Misc.Color.Auto
    | "always" -> Some Misc.Color.Always
    | "never" -> Some Misc.Color.Never
    | _ -> None);
  print = (function
    | Misc.Color.Auto -> "auto"
    | Misc.Color.Always -> "always"
    | Misc.Color.Never -> "never");
  usage = "expected \"auto\", \"always\" or \"never\"";
  env_var = "OCAML_COLOR";
}

let error_style = ref None (* -error-style *)

let error_style_reader = {
  parse = (function
    | "contextual" -> Some Misc.Error_style.Contextual
    | "short" -> Some Misc.Error_style.Short
    | _ -> None);
  print = (function
    | Misc.Error_style.Contextual -> "contextual"
    | Misc.Error_style.Short -> "short");
  usage = "expected \"contextual\" or \"short\"";
  env_var = "OCAML_ERROR_STYLE";
}

(* This is used by the -save-ir-after option. *)
module Compiler_ir = struct
  type t = Linear

  let all = [
    Linear;
  ]

  let extension t =
    let ext =
    match t with
      | Linear -> "linear"
    in
    ".cmir-" ^ ext

  (** [extract_extension_with_pass filename] returns the IR whose extension
      is a prefix of the extension of [filename], and the suffix,
      which can be used to distinguish different passes on the same IR.
      For example, [extract_extension_with_pass "foo.cmir-linear123"]
      returns [Some (Linear, "123")]. *)
  let extract_extension_with_pass filename =
    let ext = Filename.extension filename in
    let ext_len = String.length ext in
    if ext_len <= 0 then None
    else begin
      let is_prefix ir =
        let s = extension ir in
        let s_len = String.length s in
        s_len <= ext_len && s = String.sub ext 0 s_len
      in
      let drop_prefix ir =
        let s = extension ir in
        let s_len = String.length s in
        String.sub ext s_len (ext_len - s_len)
      in
      let ir = List.find_opt is_prefix all in
      match ir with
      | None -> None
      | Some ir -> Some (ir, drop_prefix ir)
    end
end

(* This is used by the -stop-after option. *)
module Compiler_pass = struct
  (* If you add a new pass, the following must be updated:
     - the variable `passes` below
     - the manpages in man/ocaml{c,opt}.m
     - the manual manual/manual/cmds/unified-options.etex
  *)
  type t = Parsing | Typing | Scheduling | Emit

  let to_string = function
    | Parsing -> "parsing"
    | Typing -> "typing"
    | Scheduling -> "scheduling"
    | Emit -> "emit"

  let of_string = function
    | "parsing" -> Some Parsing
    | "typing" -> Some Typing
    | "scheduling" -> Some Scheduling
    | "emit" -> Some Emit
    | _ -> None

  let rank = function
    | Parsing -> 0
    | Typing -> 1
    | Scheduling -> 50
    | Emit -> 60

  let passes = [
    Parsing;
    Typing;
    Scheduling;
    Emit;
  ]
  let is_compilation_pass _ = true
  let is_native_only = function
    | Scheduling -> true
    | Emit -> true
    | _ -> false

  let enabled is_native t = not (is_native_only t) || is_native
  let can_save_ir_after = function
    | Scheduling -> true
    | _ -> false

  let available_pass_names ~filter ~native =
    passes
    |> List.filter (enabled native)
    |> List.filter filter
    |> List.map to_string

  let compare a b =
    compare (rank a) (rank b)

  let to_output_filename t ~prefix =
    match t with
    | Scheduling -> prefix ^ Compiler_ir.(extension Linear)
    | _ -> Misc.fatal_error "Not supported"

  let of_input_filename name =
    match Compiler_ir.extract_extension_with_pass name with
    | Some (Linear, _) -> Some Emit
    | None -> None
end

let stop_after = ref None (* -stop-after *)

let should_stop_after pass =
  if Compiler_pass.(rank Typing <= rank pass) && !print_types then true
  else
    match !stop_after with
    | None -> false
    | Some stop -> Compiler_pass.rank stop <= Compiler_pass.rank pass

let save_ir_after = ref []

let should_save_ir_after pass =
  List.mem pass !save_ir_after

let set_save_ir_after pass enabled =
  let other_passes = List.filter ((<>) pass) !save_ir_after in
  let new_passes =
    if enabled then
      pass :: other_passes
    else
      other_passes
  in
  save_ir_after := new_passes

(* This function is almost the same as [Arg.parse_expand], except
   that [Arg.parse_expand] could not be used because it does not take a
   reference for [arg_spec].*)
let parse_arguments argv f msg =
  try
    let argv = ref argv in
    let current = ref 0 in
    Arg.parse_and_expand_argv_dynamic current argv arg_spec f msg
  with
  | Arg.Bad msg -> Printf.eprintf "%s" msg; exit 2
  | Arg.Help msg -> Printf.printf "%s" msg; exit 0
