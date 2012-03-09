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




val split3: ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
val sub: 'a -> 'a list -> 'a list
val map3:  ('a -> 'b -> 'c -> 'd) -> 'a list -> 'b list -> 'c list -> 'd list
exception Filterskip
val try_filter: ('a -> 'b) -> 'a list -> 'b list
