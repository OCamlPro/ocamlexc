(***********************************************************************)
(*                                                                     *)
(*                           Ocamlexc                                  *)
(*                                                                     *)
(*        Francois Pessaux, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)




val pp_module_type: Format.formatter -> Typedtree.module_type -> unit
val pp_signature: Format.formatter -> Typedtree.signature_item list -> unit
val pp_toplevel_toplevel: Format.formatter ->
  Typedtree.toplevel_phrase * Typedtree.signature_item list -> unit
