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




(* Used when substituing in functors type. *)
(* Warning : this makes a copy not an in-place modification but because *)
(* there's no sharing (no types), we don't matter.                      *)
let rec subst_module_expr subst mod_expr =
 { Scopedtree.pmod_desc =
            subst_module_expr_desc subst mod_expr.Scopedtree.pmod_desc ;
   Scopedtree.pmod_loc = mod_expr.Scopedtree.pmod_loc }



and subst_module_expr_desc subst = function
  | Scopedtree.Pmod_ident path -> Scopedtree.Pmod_ident (Subst.path path subst)
  | Scopedtree.Pmod_structure structure ->
      Scopedtree.Pmod_structure (subst_structure subst structure)
  | Scopedtree.Pmod_functor (arg_name, body_expr) ->
      Scopedtree.Pmod_functor (arg_name, subst_module_expr subst body_expr)
  | Scopedtree.Pmod_apply (mod_expr0, mod_expr1) ->
      Scopedtree.Pmod_apply (subst_module_expr subst mod_expr0,
			     subst_module_expr subst mod_expr1)


and subst_structure subst structure =
  List.map (subst_structure_item subst) structure



and subst_structure_item subst str_item =
 { Scopedtree.pstr_desc =
        subst_structure_item_desc subst str_item.Scopedtree.pstr_desc ;
   Scopedtree.pstr_loc = str_item.Scopedtree.pstr_loc } 



and subst_structure_item_desc subst = function
  | Scopedtree.Pstr_eval expression ->
      Scopedtree.Pstr_eval (Substcore.subst_expression subst expression)
  | Scopedtree.Pstr_value (rec_flag, bindings) ->
      let bindings' =
	List.map (fun (pat, expr) ->
	           let pat' = Substcore.subst_pattern subst pat in
		   let expr' = Substcore.subst_expression subst expr in
		   (pat', expr'))
	         bindings in
      Scopedtree.Pstr_value (rec_flag, bindings')
  | Scopedtree.Pstr_primitive (_, _) as whatever ->
      (* Let the user manage itself expansions and all stuff in primitives *)
      whatever
  | Scopedtree.Pstr_type ty_decls ->
      let ty_decls' =
	List.map (fun (ty_name, ty_decl) ->
	           (ty_name, Substcore.subst_type_declaration subst ty_decl))
	         ty_decls in
      Scopedtree.Pstr_type ty_decls'
  | Scopedtree.Pstr_exception (exn_id, exn_decl) ->
      let exn_decl' = Substcore.subst_exception_declaration subst exn_decl in
      Scopedtree.Pstr_exception (exn_id, exn_decl')
  | Scopedtree.Pstr_module (mod_name, mod_expr) ->
      Scopedtree.Pstr_module (mod_name, subst_module_expr subst mod_expr)
  | Scopedtree.Pstr_modtype -> Scopedtree.Pstr_modtype
  | Scopedtree.Pstr_open _ as whatever -> whatever
;;


Substcore.subst_module_expr_forward := subst_module_expr ;;
