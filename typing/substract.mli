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




exception Unused_pattern

val substract_ml_type: Typedtree.envt -> Typecore.ml_type_expr ->
                Scopedtree.pattern ->
                (Ident.t * Typecore.ml_type_expr) list * Typecore.ml_type_expr
