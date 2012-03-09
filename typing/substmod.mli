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




val subst_module_expr: Subst.t -> Scopedtree.module_expr ->
                       Scopedtree.module_expr
val subst_structure: Subst.t -> Scopedtree.structure -> Scopedtree.structure
