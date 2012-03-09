(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

(* $Id: config.ml,v 1.2 1999/02/15 15:00:33 pessaux Exp $ *)

let version = "2.01"

let standard_library =
  try
    Sys.getenv "CAMLLIB"
  with Not_found ->
    "/usr/local/lib/ocaml"

let standard_runtime = "/usr/local/bin/ocamlrun"
let bytecomp_c_compiler = "gcc "
let native_c_compiler = "gcc "
let native_partial_linker = "ld -r "
let c_libraries = "-lcurses -lm"
let ranlib = "ranlib"

let exec_magic_number = "Caml1999X004"
and cmi_magic_number = "Caml1999I004"
and cmo_magic_number = "Caml1999O004"
and cma_magic_number = "Caml1999A004"
and cmx_magic_number = "Caml1999Y006"
and cmxa_magic_number = "Caml1999Z006"
and ast_impl_magic_number = "Caml1999M005"
and ast_intf_magic_number = "Caml1999N005"

let load_path = ref ([] : string list)

let interface_suffix = ref ".mli"

let max_tag = 248
let max_young_wosize = 256

let architecture = "i386"
let model = "default"
let system = "linux_elf"

let ext_obj = ".o"
let ext_asm = ".s"
let ext_lib = ".a"
