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


exception Or_pattern_must_not_bind

val scope_expression: Envscope.t -> Parsetree.expression ->
                      Scopedtree.expression

val scope_let_definition: Envscope.t -> Asttypes.rec_flag ->
                          (Parsetree.pattern * Parsetree.expression) list ->
                          (Scopedtree.pattern * Scopedtree.expression) list
			  * Envscope.t

val scope_type_declaration: Envscope.t -> Ident.t ->
                            Parsetree.type_declaration ->
                            Scopedtree.type_declaration * Envscope.t

val scope_core_type: Envscope.t -> Parsetree.core_type -> Scopedtree.core_type
val scope_annoted_core_type: Envscope.t -> Parsetree.annoted_core_type ->
                                                   Scopedtree.annoted_core_type

val scope_pattern: Envscope.t -> Parsetree.pattern ->
                   Scopedtree.pattern * Envscope.t

val scope_module_expression_forward:
    (Envscope.t -> Parsetree.module_expr ->
      Scopedtree.module_expr * Envscope.t) ref
