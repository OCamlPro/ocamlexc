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




type error =
  | Unbound_value of Path.t
  | Unbound_type of Path.t
  | Unbound_module of Path.t
  | Unbound_label of Path.t
  | Unbound_constructor of Path.t
  | Unbound_component of string

exception Type_error of error

val empty_env: unit -> Typedtree.envt
val add_value: Ident.t -> Typecore.ml_types_scheme ->
               Typedtree.envt -> Typedtree.envt
val add_type: Ident.t -> Typedtree.type_declaration ->
              Typedtree.envt -> Typedtree.envt
val add_module: Ident.t -> Typedtree.module_type ->
                Typedtree.envt -> Typedtree.envt
val add_constructor: Ident.t -> Typedtree.constructor_description ->
                     Typedtree.envt -> Typedtree.envt
val add_spec: Typedtree.signature_item -> Typedtree.envt -> Typedtree.envt

val find_value: Path.t -> Typedtree.envt -> Typecore.ml_type_expr
val find_type: Path.t -> Typedtree.envt -> Typedtree.type_declaration
val find_module: Path.t -> Typedtree.envt -> Typedtree.module_type
val find_constructor: Path.t -> Typedtree.envt ->
                      Typedtree.constructor_description
val find_label: Path.t -> Typedtree.envt -> Typedtree.label_description

val global_type_env: Typedtree.envt ref
val load_types: unit -> unit

val get_constructor_real_name: Path.t -> Typedtree.envt -> string

(* For mu rule *)
val add_value_rec: Ident.t ->
                   (Typecore.ml_types_scheme * Typecore.occurrence list ref) ->
                   Typedtree.envt -> Typedtree.envt
val append: Typedtree.envt -> Typedtree.envt -> Typedtree.envt
val add_value_binding: Ident.t -> Typecore.value_binding ->
                       Typedtree.envt -> Typedtree.envt
val get_raw_value_binding: Ident.t -> Typedtree.envt -> Typecore.value_binding
