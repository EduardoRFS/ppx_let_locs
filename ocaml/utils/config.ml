#2 "utils/config.mlp"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The main OCaml version string has moved to ../VERSION *)
let version = Sys.ocaml_version

let standard_library_default = "/usr/local/lib/ocaml"

let standard_library =
  try
    Sys.getenv "OCAMLLIB"
  with Not_found ->
  try
    Sys.getenv "CAMLLIB"
  with Not_found ->
    standard_library_default

let ccomp_type = "cc"
let c_compiler = "gcc"
let c_output_obj = "-o "
let c_has_debug_prefix_map = true
let as_has_debug_prefix_map = true
let ocamlc_cflags = "-O2 -fno-strict-aliasing -fwrapv -fPIC "
let ocamlc_cppflags = "-D_FILE_OFFSET_BITS=64 -D_REENTRANT "
(* #7678: ocamlopt uses these only to compile .c files, and the behaviour for
          the two drivers should be identical. *)
let ocamlopt_cflags = "-O2 -fno-strict-aliasing -fwrapv -fPIC "
let ocamlopt_cppflags = "-D_FILE_OFFSET_BITS=64 -D_REENTRANT "
let bytecomp_c_libraries = "-lm -ldl  -lpthread "
(* bytecomp_c_compiler and native_c_compiler have been supported for a
   long time and are retained for backwards compatibility.
   For programs that don't need compatibility with older OCaml releases
   the recommended approach is to use the constituent variables
   c_compiler, ocamlc_cflags, ocamlc_cppflags etc., directly.
*)
let bytecomp_c_compiler =
  c_compiler ^ " " ^ ocamlc_cflags ^ " " ^ ocamlc_cppflags
let native_c_compiler =
  c_compiler ^ " " ^ ocamlopt_cflags ^ " " ^ ocamlopt_cppflags
let native_c_libraries = "-lm -ldl "
let native_pack_linker = "ld -r -o "
let ranlib = "ranlib"
let ar = "ar"
let mkdll, mkexe, mkmaindll =
  (* @@DRA Cygwin - but only if shared libraries are enabled, which we
     should be able to detect? *)
  if Sys.os_type = "Win32" then
    try
      let flexlink =
        let flexlink = Sys.getenv "OCAML_FLEXLINK" in
        let f i =
          let c = flexlink.[i] in
          if c = '/' then '\\' else c in
        (String.init (String.length flexlink) f) ^ " " in
      flexlink ^ "",
      flexlink ^ " -exe -link \"-Wl,-E\"",
      flexlink ^ " -maindll"
    with Not_found ->
      "gcc -shared", "gcc -O2 -fno-strict-aliasing -fwrapv -Wall -Wdeclaration-after-statement -fno-common -fexcess-precision=standard -fno-tree-vrp -ffunction-sections  -Wl,-E", "gcc -shared"
  else
    "gcc -shared", "gcc -O2 -fno-strict-aliasing -fwrapv -Wall -Wdeclaration-after-statement -fno-common -fexcess-precision=standard -fno-tree-vrp -ffunction-sections  -Wl,-E", "gcc -shared"

let flambda = false
let with_flambda_invariants = false
let safe_string = true
let default_safe_string = true
let windows_unicode = 0 != 0
let supports_shared_libraries = true

let flat_float_array = true

let function_sections = true
let afl_instrument = false

let exec_magic_number = "Caml1999X029"
    (* exec_magic_number is duplicated in runtime/caml/exec.h *)
and cmi_magic_number = "Caml1999I029"
and cmo_magic_number = "Caml1999O029"
and cma_magic_number = "Caml1999A029"
and cmx_magic_number =
  if flambda then
    "Caml1999y029"
  else
    "Caml1999Y029"
and cmxa_magic_number =
  if flambda then
    "Caml1999z029"
  else
    "Caml1999Z029"
and ast_impl_magic_number = "Caml1999M029"
and ast_intf_magic_number = "Caml1999N029"
and cmxs_magic_number = "Caml1999D029"
and cmt_magic_number = "Caml1999T029"
and linear_magic_number = "Caml1999L029"

let interface_suffix = ref ".mli"

let max_tag = 245
(* This is normally the same as in obj.ml, but we have to define it
   separately because it can differ when we're in the middle of a
   bootstrapping phase. *)
let lazy_tag = 246

let max_young_wosize = 256
let stack_threshold = 256 (* see runtime/caml/config.h *)
let stack_safety_margin = 60

let architecture = "amd64"
let model = "default"
let system = "linux"

let asm = "as"
let asm_cfi_supported = true
let with_frame_pointers = false
let profinfo = false
let profinfo_width = 0

let ext_exe = ""
let ext_obj = ".o"
let ext_asm = ".s"
let ext_lib = ".a"
let ext_dll = ".so"

let host = "x86_64-pc-linux-gnu"
let target = "x86_64-pc-linux-gnu"

let default_executable_name =
  match Sys.os_type with
    "Unix" -> "a.out"
  | "Win32" | "Cygwin" -> "camlprog.exe"
  | _ -> "camlprog"

let systhread_supported = true;;

let flexdll_dirs = [];;

type configuration_value =
  | String of string
  | Int of int
  | Bool of bool

let configuration_variables =
  let p x v = (x, String v) in
  let p_int x v = (x, Int v) in
  let p_bool x v = (x, Bool v) in
[
  p "version" version;
  p "standard_library_default" standard_library_default;
  p "standard_library" standard_library;
  p "ccomp_type" ccomp_type;
  p "c_compiler" c_compiler;
  p "ocamlc_cflags" ocamlc_cflags;
  p "ocamlc_cppflags" ocamlc_cppflags;
  p "ocamlopt_cflags" ocamlopt_cflags;
  p "ocamlopt_cppflags" ocamlopt_cppflags;
  p "bytecomp_c_compiler" bytecomp_c_compiler;
  p "native_c_compiler" native_c_compiler;
  p "bytecomp_c_libraries" bytecomp_c_libraries;
  p "native_c_libraries" native_c_libraries;
  p "native_pack_linker" native_pack_linker;
  p "ranlib" ranlib;
  p "architecture" architecture;
  p "model" model;
  p_int "int_size" Sys.int_size;
  p_int "word_size" Sys.word_size;
  p "system" system;
  p "asm" asm;
  p_bool "asm_cfi_supported" asm_cfi_supported;
  p_bool "with_frame_pointers" with_frame_pointers;
  p "ext_exe" ext_exe;
  p "ext_obj" ext_obj;
  p "ext_asm" ext_asm;
  p "ext_lib" ext_lib;
  p "ext_dll" ext_dll;
  p "os_type" Sys.os_type;
  p "default_executable_name" default_executable_name;
  p_bool "systhread_supported" systhread_supported;
  p "host" host;
  p "target" target;
  p_bool "flambda" flambda;
  p_bool "safe_string" safe_string;
  p_bool "default_safe_string" default_safe_string;
  p_bool "flat_float_array" flat_float_array;
  p_bool "function_sections" function_sections;
  p_bool "afl_instrument" afl_instrument;
  p_bool "windows_unicode" windows_unicode;
  p_bool "supports_shared_libraries" supports_shared_libraries;

  p "exec_magic_number" exec_magic_number;
  p "cmi_magic_number" cmi_magic_number;
  p "cmo_magic_number" cmo_magic_number;
  p "cma_magic_number" cma_magic_number;
  p "cmx_magic_number" cmx_magic_number;
  p "cmxa_magic_number" cmxa_magic_number;
  p "ast_impl_magic_number" ast_impl_magic_number;
  p "ast_intf_magic_number" ast_intf_magic_number;
  p "cmxs_magic_number" cmxs_magic_number;
  p "cmt_magic_number" cmt_magic_number;
  p "linear_magic_number" linear_magic_number;
]

let print_config_value oc = function
  | String s ->
      Printf.fprintf oc "%s" s
  | Int n ->
      Printf.fprintf oc "%d" n
  | Bool p ->
      Printf.fprintf oc "%B" p

let print_config oc =
  let print (x, v) =
    Printf.fprintf oc "%s: %a\n" x print_config_value v in
  List.iter print configuration_variables;
  flush oc;
;;

let config_var x =
  match List.assoc_opt x configuration_variables with
  | None -> None
  | Some v ->
      let s = match v with
        | String s -> s
        | Int n -> Int.to_string n
        | Bool b -> string_of_bool b
      in
      Some s

let merlin = false
