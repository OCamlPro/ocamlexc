(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*            Francois Pessaux, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id: asttypes.mli,v 1.3 1999/02/15 15:00:07 pessaux Exp $ *)

(* Auxiliary a.s.t. types used by parsetree and typedtree. *)

type constant =
    Const_int of int
  | Const_char of char
  | Const_string of string
  | Const_float of string

type rec_flag = Nonrecursive | Recursive

type direction_flag = Upto | Downto

type private_flag = Private | Public

type mutable_flag = Immutable | Mutable

type virtual_flag = Virtual | Concrete

val string_of_constant: constant -> string
