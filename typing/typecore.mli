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



type 'a marker =
  | Notseen
  | Seen of int ref
  | Aliased of 'a
  | Abbreviated of string
  | Seennotrec


(* Must be repr'ed *)
type ml_type_expr = {
  mutable mte_desc: ml_type_desc ;
  mutable mte_level: int ;
  mutable mte_link: ml_type_expr option ;
  mutable mte_info: ml_type_expr marker ;
  mutable mte_position: int ;
  mutable mte_empty: bool
  }

and ml_type_desc =
  | Tvar
  | Tarrow of ml_type_expr * ml_type_expr * phi_expr
  | Ttuple of ml_type_expr list
  | Tconstr of Path.t ref * ml_type_expr list * phi_expr

(* Must be repr'ed *)
and phi_expr = {
  mutable phi_value: row ;
  mutable phi_print: bool ;
  mutable phi_empty: bool }

and row =
  | Pbottom
  | Pexplicit of (phi_elem list * row_variable)

and phi_elem =
  | Constant of string * simple_presence
  | Param of (string * ml_type_expr)
  | Record of (string * ml_type_expr) list

and row_variable = {
     mutable row_level: int ;
     mutable row_value: phi_expr option ;
     mutable row_info: row_variable marker ;
     mutable row_position: int
   }

(* Must be repr'ed *)
and simple_presence = {
  mutable pre_desc: simple_presence_desc ;
  mutable pre_level: int ;
  mutable pre_link: simple_presence option ;
  mutable pre_info: simple_presence marker ;
  mutable pre_position: int
  } 

and simple_presence_desc =
  | Prpre
  | Prvar
;;

type ml_types_scheme = {
  mutable mts_body : ml_type_expr ;
  mutable mts_count : int }


exception Conflict of ml_type_expr * ml_type_expr
exception Non_regular_datatype of ml_type_expr * ml_type_expr

(* Type normalization *)
val ml_type_repr: ml_type_expr -> ml_type_expr
val phi_repr: phi_expr -> phi_expr
val presence_repr: simple_presence -> simple_presence

(* Type unification *)
val unify_ml_type: ml_type_expr -> ml_type_expr -> unit
val unify_phi_type: phi_expr -> phi_expr -> unit
val unify_presence: simple_presence -> simple_presence -> unit

(* Type scheme management *)
val specialize_ml_type: ml_types_scheme -> ml_type_expr
val specialize_ml_type2: ml_types_scheme -> ml_type_expr list ->
                         (ml_type_expr * ml_type_expr list)
val generalize_ml_type: ml_type_expr -> ml_types_scheme
val generalize_ml_type2: ml_type_expr -> ml_type_expr list ->
                         (ml_types_scheme * ml_type_expr list)
val trivial_ml_type_scheme: ml_type_expr -> ml_types_scheme

(* Type expression creation *)
val type_constr: Path.t ref -> ml_type_expr list -> phi_expr -> ml_type_expr
val type_int: int -> ml_type_expr
val type_int_unknown: unit -> ml_type_expr
val type_char: char -> ml_type_expr
val type_char_unknown: unit -> ml_type_expr
val type_string: string -> ml_type_expr
val type_string_unknown: unit -> ml_type_expr
val type_float: string -> ml_type_expr
val type_float_unknown: unit -> ml_type_expr
val type_unit: unit -> ml_type_expr
val type_bool: bool -> ml_type_expr
val type_bool_unknown: unit -> ml_type_expr
val type_exception: string -> ml_type_expr option -> ml_type_expr
val type_exception_unknown: unit -> ml_type_expr
val type_tuple: ml_type_expr list -> ml_type_expr
val type_arrow: ml_type_expr -> ml_type_expr -> phi_expr -> ml_type_expr
val type_array: ml_type_expr -> ml_type_expr
val type_variable: unit -> ml_type_expr

(* Approximations creation *)
val empty_phi: unit -> phi_expr
val bottom_phi: unit -> phi_expr
val constant_phi_pre: string -> phi_expr
val constant_phi_variable: string -> phi_expr
val param_phi: string -> ml_type_expr -> phi_expr
val record_phi: (string * ml_type_expr) list -> phi_expr

(* Approximations elements creation. Used by subtraction *)
val constant_phielem_variable: string -> phi_elem

(* Presence creation. Used to convert annoted syntax to type term *)
val presence_present: unit -> simple_presence
val presence_variable: unit -> simple_presence

val row_variable: unit -> row_variable

(* Binding level management *)
val generic_level: int
val begin_definition: unit -> unit
val end_definition: unit -> unit

(* Exception-type <-> approximation injection/extraction *)
val uncaught_exception: ml_type_expr -> phi_expr
val caught_exception: phi_expr -> ml_type_expr

val circularize: (Path.t ref * ml_type_expr) list -> ml_type_expr -> unit

val clean_ml_type: ml_type_expr -> unit
val clean_phi_type: phi_expr -> unit

(* *********** *)
(* For mu rule *)
(* *********** *)
val copying_generalization: ml_type_expr -> ml_types_scheme
val type_scheme_equal: ml_types_scheme -> ml_types_scheme -> bool

type occurrence = {
  (* level = level auquel on a cree l'occurrence *)
  occ_level : int ;
  (* Type trouve pour l'occurrence *)
  occ_type : ml_type_expr
  }

type value_binding =
 | Regular_ident of ml_types_scheme
 | Mu_ident of ml_types_scheme * occurrence list ref


val specialize_ml_type_with_level: int -> ml_types_scheme -> ml_type_expr
val get_current_binding_level: unit -> int
val copying_generalization_with_level: int -> ml_type_expr -> ml_types_scheme
val generalize_ml_type_with_level: int -> ml_type_expr -> ml_types_scheme
