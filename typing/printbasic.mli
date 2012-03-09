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




val pp_longident: Format.formatter -> Longident.t -> unit
val pp_constant: Format.formatter -> Asttypes.constant -> unit
val pp_rec_flag: Format.formatter -> Asttypes.rec_flag -> unit
val pp_direction_flag: Format.formatter -> Asttypes.direction_flag -> unit
val pp_mutable_flag: Format.formatter -> Asttypes.mutable_flag -> unit
val pp_private_flag: Format.formatter -> Asttypes.private_flag -> unit
val pp_virtual_flag: Format.formatter -> Asttypes.virtual_flag -> unit

val iter_pp: string -> (Format.formatter -> 'a -> unit) ->
             Format.formatter -> 'a list -> unit
