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



val scope_module_expression: Envscope.t -> Parsetree.module_expr ->
                             Scopedtree.module_expr * Envscope.t

val scope_structure: Envscope.t -> Parsetree.structure ->
                                   Scopedtree.structure * Envscope.t
