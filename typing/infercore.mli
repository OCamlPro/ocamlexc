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




val infer_module_expr_forward:
  (Typedtree.envt ->
  Scopedtree.module_expr ->
  Typedtree.module_expr * (Ident.t * Typedtree.module_expr) list) ref

val find_module_expr_exception_forward:
  (Typecore.phi_expr -> Typedtree.module_expr -> unit) ref



type policy = Fixed | Extensible
type variable_mapping = {
  vm_policy : policy ;
  mutable vm_ml : (string * Typecore.ml_type_expr) list ;
  mutable vm_rho : (string * Typecore.row_variable) list ;
  mutable vm_pre : (string * Typecore.simple_presence) list
  }
val new_mapping: policy -> string list -> unit
val release_vars_mapping: unit -> unit
val current_vars_mapping: unit -> variable_mapping


exception Constructor_arity_error of Path.t
exception Unbound_type_variable of string
exception Type_arity_error of Path.t
exception Bad_labels_number
exception Field_not_mutable of Path.t
exception Invalid_equiv_in_variant_type



val core_type_to_ml_type_expr:
    Typedtree.envt -> Scopedtree.core_type -> Typecore.ml_type_expr
val infer_pattern: Typecore.ml_type_expr -> Typedtree.envt ->
                   Scopedtree.pattern ->
                   (Ident.t * Typecore.ml_type_expr) list * Typedtree.pattern
val infer_expression: Typedtree.envt -> Scopedtree.expression ->
                      Typedtree.expression
val infer_let_definition: Typedtree.envt ->
                          Asttypes.rec_flag ->
                          (Scopedtree.pattern * Scopedtree.expression) list ->
                          (Ident.t * Typecore.value_binding) list *
			   (Typedtree.pattern * Typedtree.expression) list

val infer_type_declaration:
  Typedtree.envt ->
  Scopedtree.type_declaration ->
  (Ident.t * Typecore.ml_type_expr * Typedtree.constructor_arity) list *
  (Ident.t * Asttypes.mutable_flag * Typecore.ml_type_expr * int) list *
  (Typecore.ml_type_expr option * Typecore.ml_type_expr list)

val infer_primitive: Typedtree.envt -> Scopedtree.annoted_value_description ->
                     Typecore.ml_type_expr

val infer_exception: Typedtree.envt -> Ident.t ->
                     Scopedtree.core_type option ->
                Typedtree.constructor_description


type mu_indicator = Mu_off | Mu_on
val mu_flag: mu_indicator ref
type mu_definition
type mu_sub_problem
type mu_problem = mu_sub_problem list


val mu_problem: mu_problem ref
val solve_mu_problem: unit -> mu_problem
val make_final_links:
  mu_sub_problem list ->
  (Ident.t * Typecore.value_binding) list ->
  (Ident.t * Typecore.ml_types_scheme) list

exception Expansion_required_in_primitive of Path.t
