(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id: longident.ml,v 1.3 1999/03/04 17:42:49 pessaux Exp $ *)

type t =
    Lident of string
  | Ldot of t * string


let rec flat accu = function
    Lident s -> s :: accu
  | Ldot (lid, s) -> flat (s :: accu) lid


let flatten lid = flat [] lid
