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



val forget_pvars: bool ref
val pp_ml_type: Format.formatter -> Typecore.ml_type_expr -> unit
val pp_ml_type_scheme: Format.formatter -> Typecore.ml_types_scheme -> unit
val pp_phi_type: Format.formatter -> Typecore.phi_expr -> unit
val pp_type_declaration: Format.formatter ->
                         (Ident.t * Typedtree.type_declaration) -> unit

