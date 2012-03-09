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




val pp_toplevel_phrase: Format.formatter -> Scopedtree.toplevel_phrase -> unit
val pp_type_declaration: Format.formatter ->
                         (Ident.t * Scopedtree.type_declaration) -> unit
val pp_module_expr: Format.formatter -> Scopedtree.module_expr -> unit
