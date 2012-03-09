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


val subst_module_expr_forward: (Subst.t -> Scopedtree.module_expr ->
  Scopedtree.module_expr) ref

val subst_expression: Subst.t -> Scopedtree.expression -> Scopedtree.expression
val subst_pattern: Subst.t -> Scopedtree.pattern -> Scopedtree.pattern
(*
val subst_ml_type_expr: Subst.t -> Typecore.ml_type_expr ->
                        Typecore.ml_type_expr
val subst_ml_types_scheme: Subst.t -> Typecore.ml_types_scheme ->
                           Typecore.ml_types_scheme
*)
val subst_type_declaration: Subst.t -> Scopedtree.type_declaration ->
                            Scopedtree.type_declaration
val subst_exception_declaration: Subst.t -> Scopedtree.exception_declaration ->
  Scopedtree.exception_declaration
