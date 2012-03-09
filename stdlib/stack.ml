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

(* $Id: stack.ml,v 1.1 1998/08/19 13:29:12 pessaux Exp $ *)

type 'a t = { mutable c : 'a list }

exception Empty

let create () = { c = [] }

let clear s = s.c <- []

let push x s = s.c <- x :: s.c

let pop s =
  match s.c with
    hd::tl -> s.c <- tl; hd
  | []     -> raise Empty

let length s = List.length s.c

let iter f s = List.iter f s.c
