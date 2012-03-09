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




val force_vars_print: bool ref
val pp_ml_type: Printcontext.print_context -> Format.formatter ->
                Typecore.ml_type_expr -> unit
val pp_ml_type_scheme: Printcontext.print_context -> Format.formatter ->
                       Typecore.ml_types_scheme -> unit
val pp_phi_type: Printcontext.print_context -> Format.formatter ->
                 Typecore.phi_expr -> unit
val pp_type_declaration: Printcontext.print_context -> Format.formatter ->
  (Ident.t * Typedtree.type_declaration) -> unit
