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




val pp_module_type: Printcontext.print_context -> Format.formatter ->
                    Typedtree.module_type -> unit
val pp_signature: Printcontext.print_context -> Format.formatter ->
                  Typedtree.signature_item list -> unit
