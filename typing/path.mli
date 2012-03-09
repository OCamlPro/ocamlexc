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




type t =
  | Pident of Ident.t
  | Pdot of t * string

val equal: t -> t -> bool

val pp_path: Format.formatter -> t -> unit
val pp_path_debug: Format.formatter -> t -> unit
val end_name: t -> string
