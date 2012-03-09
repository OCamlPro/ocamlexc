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



val get_compil_units_to_load: unit -> Ident.t list ;;


type error =
  | Unbound_value of string
  | Unbound_type of string
  | Unbound_module of string
  | Unbound_label of string

exception Scope_error of error


type t
val empty: t
val append: t -> t -> t
val enter_value: string -> t -> t
val enter_type: string -> t -> t
val enter_module: string -> t -> t -> t
val enter_label: string -> t -> t

val value_longident: Longident.t -> t -> Path.t
val type_longident: Longident.t -> t -> Path.t
val module_longident: Longident.t -> t -> Path.t
val label_longident: Longident.t -> t -> Path.t

(* Used to retrieve ident from string when scoping *)
val retrieve_scoped_value: string -> t -> Ident.t
val retrieve_scoped_type: string -> t -> Ident.t
val retrieve_scoped_label: string -> t -> Ident.t
val retrieve_scoped_module: string -> t -> Ident.t

val global_scope_env: t ref
val load_scopes: unit -> unit

val handle_open_directive: t -> Path.t -> t
val diff: t -> t -> t

val mod_longident_evt: Longident.t -> t -> t
