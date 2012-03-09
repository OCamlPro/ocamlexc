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




let scope_directive_argument scenv = function
  | Parsetree.Pdir_none -> Scopedtree.Pdir_none
  | Parsetree.Pdir_string str -> Scopedtree.Pdir_string str
  | Parsetree.Pdir_int i -> Scopedtree.Pdir_int i
  | Parsetree.Pdir_ident longident ->
      let scoped_longident = Envscope.module_longident longident scenv in
      Scopedtree.Pdir_ident scoped_longident
;;


(* scope_toplevel_phrase: Envscope.t -> Parsetree.toplevel_phrase ->  *)
(*                            Scopedtree.toplevel_phrase * Envscope.t *)
(* Scope a toplevel phrase and return the <extended scoping environment> *)
let scope_toplevel_phrase scenv = function
  | Parsetree.Ptop_def structure ->
      let (scoped_structure, new_scenv) =
	                      Modscope.scope_structure scenv structure in
      (Scopedtree.Ptop_def scoped_structure,
       new_scenv)
  | Parsetree.Ptop_dir (dn, da) ->
      (Scopedtree.Ptop_dir (dn, (scope_directive_argument scenv da)),
       scenv)
;;
