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




type t
val create: string -> t
val create_global: string -> t
val name: t -> string
val stamp: t -> int
val equal: t -> t -> bool
type 'a tbl
val emptytbl: 'a tbl
val add: t -> 'a -> 'a tbl -> 'a tbl
val find: t -> 'a tbl -> 'a
val append:  'a tbl -> 'a tbl -> 'a tbl

val int_ident: t
val char_ident: t
val string_ident: t
val float_ident: t
val unit_ident: t
val bool_ident: t
val exn_ident: t
val array_ident: t
val list_ident: t
val format_ident: t

val nil_ident: t
val cons_ident: t
val failure_ident: t
val invarg_ident: t
val void_ident: t
val eof_ident: t
val out_of_mem_ident: t
val stack_over_ident: t
val match_failure_ident: t

val true_ident: t
val false_ident: t
val not_found_ident: t
val division_by_zero_ident: t
val sys_error_ident: t
