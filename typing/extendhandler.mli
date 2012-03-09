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


val extend_toplevel_phrase_handler: Parsetree.toplevel_phrase ->
                                    Parsetree.toplevel_phrase

val extend_structure_handler: Parsetree.structure -> Parsetree.structure
