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


let subst_module_expr_forward = ref (fun _ -> assert false) ;;


let rec subst_core_type subst core_ty =
  { Scopedtree.ptyp_desc =
           subst_core_type_desc subst core_ty.Scopedtree.ptyp_desc ;
    Scopedtree.ptyp_loc = core_ty.Scopedtree.ptyp_loc }



and subst_core_type_desc subst = function
  | Scopedtree.Ptyp_any as whatever -> whatever
  | Scopedtree.Ptyp_var _ as whatever -> whatever
  | Scopedtree.Ptyp_arrow (neg, pos) ->
      Scopedtree.Ptyp_arrow (subst_core_type subst neg,
			    subst_core_type subst pos)
  | Scopedtree.Ptyp_tuple args ->
      Scopedtree.Ptyp_tuple (List.map (subst_core_type subst) args)
  | Scopedtree.Ptyp_constr (path, args) ->
      (* XXX Not sure *)
      let path' = ref (Subst.path !path subst) in
      let args' = List.map (subst_core_type subst) args in
      Scopedtree.Ptyp_constr (path', args')
  | Scopedtree.Ptyp_alias (cty, alias_name) ->
      Scopedtree.Ptyp_alias (subst_core_type subst cty, alias_name)


and subst_core_field_type subst core_field_type =
  let core_field_desc' =
    subst_core_field_desc subst core_field_type.Scopedtree.pfield_desc in
  { Scopedtree.pfield_desc = core_field_desc' ;
    Scopedtree.pfield_loc = core_field_type.Scopedtree.pfield_loc }


and subst_core_field_desc subst = function
  | Scopedtree.Pfield (name, core_type) ->
      Scopedtree.Pfield (name, subst_core_type subst core_type)
  | Scopedtree.Pfield_var as whatever -> whatever
;;



let rec subst_expression subst expr =
 { Scopedtree.pexp_desc =
            subst_expression_desc subst expr.Scopedtree.pexp_desc ;
   Scopedtree.pexp_loc = expr.Scopedtree.pexp_loc }



and subst_expression_desc subst = function
    Scopedtree.Pexp_ident path ->
      Scopedtree.Pexp_ident (Subst.path path subst)
  | Scopedtree.Pexp_constant _ as whatever -> whatever
  | Scopedtree.Pexp_let (rec_flag, bindings, expr) ->
      let bindings' =
	List.map (fun (pat, expr) ->
	           let pat' = subst_pattern subst pat in
		   let expr' = subst_expression subst expr in
		   (pat', expr'))
	         bindings in
      let expr' = subst_expression subst expr in
      Scopedtree.Pexp_let (rec_flag, bindings', expr')
  | Scopedtree.Pexp_function bindings ->
      let bindings' =
	List.map (fun (pat, expr) ->
	           let pat' = subst_pattern subst pat in
		   let expr' = subst_expression subst expr in
		   (pat', expr'))
	         bindings in
      Scopedtree.Pexp_function bindings'
  | Scopedtree.Pexp_apply (expr, exprs) ->
      let expr' = subst_expression subst expr in
      let exprs' = List.map (subst_expression subst) exprs in
      Scopedtree.Pexp_apply (expr', exprs')
  | Scopedtree.Pexp_match (expr, bindings) ->
      let expr' = subst_expression subst expr in
      let bindings' =
	List.map (fun (pat, expr) ->
	           let pat' = subst_pattern subst pat in
		   let expr' = subst_expression subst expr in
		   (pat', expr'))
	         bindings in
      Scopedtree.Pexp_match (expr', bindings') 
  | Scopedtree.Pexp_try (expr, bindings) ->
      let expr' = subst_expression subst expr in
      let bindings' =
	List.map (fun (pat, expr) ->
	           let pat' = subst_pattern subst pat in
		   let expr' = subst_expression subst expr in
		   (pat', expr'))
	         bindings in
      Scopedtree.Pexp_try (expr', bindings') 
  | Scopedtree.Pexp_tuple exprs ->
      let exprs' = List.map (subst_expression subst) exprs in
      Scopedtree.Pexp_tuple exprs'
  | Scopedtree.Pexp_construct (path, expr_opt) ->
      let expr_opt' = (match expr_opt with None -> None
                        | Some e -> Some (subst_expression subst e)) in
      Scopedtree.Pexp_construct (Subst.path path subst, expr_opt')
  | Scopedtree.Pexp_record (fields, expr_opt) ->
      let fields' =
	List.map (fun (path, expr) ->
	           let path' = Subst.path path subst in
		   let expr' = subst_expression subst expr in
		   (path', expr'))
	         fields in
      let expr_opt' = (match expr_opt with None -> None
                        | Some e -> Some (subst_expression subst e)) in
      Scopedtree.Pexp_record (fields', expr_opt')
  | Scopedtree.Pexp_field (expr, path) ->
      Scopedtree.Pexp_field (subst_expression subst expr,
			     Subst.path path subst)
  | Scopedtree.Pexp_setfield (expr0, path, expr1) ->
      Scopedtree.Pexp_setfield (subst_expression subst expr0,
				Subst.path path subst,
				subst_expression subst expr1)
  | Scopedtree.Pexp_array exprs ->
      let exprs' = List.map (subst_expression subst) exprs in
      Scopedtree.Pexp_array exprs'
  | Scopedtree.Pexp_ifthenelse (expr0, expr1, expr_opt) ->
      let expr0' = subst_expression subst expr0 in
      let expr1' = subst_expression subst expr1 in
      let expr_opt' = (match expr_opt with None -> None
                        | Some e -> Some (subst_expression subst e)) in
      Scopedtree.Pexp_ifthenelse (expr0', expr1', expr_opt')
  | Scopedtree.Pexp_sequence (expr0, expr1) ->
      Scopedtree.Pexp_sequence (subst_expression subst expr0,
				subst_expression subst expr1)
  | Scopedtree.Pexp_while (expr0, expr1) ->
      Scopedtree.Pexp_while (subst_expression subst expr0,
				subst_expression subst expr1)
  | Scopedtree.Pexp_for (index_name, expr0, expr1, dir_flag, expr2) ->
      let expr0' = subst_expression subst expr0 in
      let expr1' = subst_expression subst expr1 in
      let expr2' = subst_expression subst expr2 in
      Scopedtree.Pexp_for (index_name, expr0', expr1', dir_flag, expr2')
  | Scopedtree.Pexp_constraint (expr, ctyopt0, ctyopt1) ->
      let expr' = subst_expression subst expr in
      let cty0opt' = (match ctyopt0 with None -> None
		      | Some cty0 -> Some (subst_core_type subst cty0)) in
      let cty1opt' = (match ctyopt1 with None -> None
		      | Some cty1 -> Some (subst_core_type subst cty1)) in
      Scopedtree.Pexp_constraint (expr', cty0opt', cty1opt')
  | Scopedtree.Pexp_when (expr0, expr1) ->
      Scopedtree.Pexp_when  (subst_expression subst expr0,
			     subst_expression subst expr1)
  | Scopedtree.Pexp_letmodule (id, mod_expr, expr) ->
      Scopedtree.Pexp_letmodule (id,
				 !subst_module_expr_forward subst mod_expr,
				 subst_expression subst expr)



and subst_pattern subst pat =
  { Scopedtree.ppat_desc = subst_pattern_desc subst pat.Scopedtree.ppat_desc ;
    Scopedtree.ppat_loc = pat.Scopedtree.ppat_loc }



and subst_pattern_desc subst = function
  | Scopedtree.Ppat_any as whatever -> whatever
  | Scopedtree.Ppat_var _ as whatever -> whatever
  | Scopedtree.Ppat_alias (pat, alias_name) ->
      Scopedtree.Ppat_alias (subst_pattern subst pat, alias_name)
  | Scopedtree.Ppat_constant _ as whatever -> whatever
  | Scopedtree.Ppat_tuple pats ->
      Scopedtree.Ppat_tuple (List.map (subst_pattern subst) pats)
  | Scopedtree.Ppat_construct (path, pat_opt) ->
      let pat_opt' = (match pat_opt with None -> None
                      | Some p -> Some (subst_pattern subst p)) in
      Scopedtree.Ppat_construct (Subst.path path subst, pat_opt')
  | Scopedtree.Ppat_record fields ->
      let fields' = List.map (fun (path, pat) ->
	                        let path' = Subst.path path subst in
			        let pat' = subst_pattern subst pat in
 			        (path', pat'))
                             fields in
      Scopedtree.Ppat_record fields'
  | Scopedtree.Ppat_array pats ->
      Scopedtree.Ppat_array (List.map (subst_pattern subst) pats)
  | Scopedtree.Ppat_or (pat1, pat2) ->
      Scopedtree.Ppat_or (subst_pattern subst pat1, subst_pattern subst pat2)
  | Scopedtree.Ppat_constraint (pat, corety) ->
      let pat' = subst_pattern subst pat in
      let corety' = subst_core_type subst corety in
      Scopedtree.Ppat_constraint (pat', corety')
;;




let subst_type_kind subst = function
  | Scopedtree.Ptype_abstract as whatever -> whatever
  | Scopedtree.Ptype_variant constructors ->
      (* constructors : (string * core_type list) list *)
      let constructors' =
	List.map (fun (cstr_name, core_tys) ->
	            (cstr_name, List.map (subst_core_type subst) core_tys))
	            constructors in
      Scopedtree.Ptype_variant constructors'
  | Scopedtree.Ptype_record fields ->
      (* fields : (string * Asttypes.mutable_flag * core_type) list *)
      let fields' =
	List.map (fun (field_name, mut_flg, core_ty) ->
	             (field_name, mut_flg, subst_core_type subst core_ty))
	         fields in
      Scopedtree.Ptype_record fields'
;;



let subst_type_declaration subst ty_decl =
  let cstrs' = List.map (fun (ct1, ct2, loc) ->
                           let ct1' = subst_core_type subst ct1 in
                           let ct2' = subst_core_type subst ct2 in
			   (ct1', ct2', loc))
                        ty_decl.Scopedtree.ptype_cstrs in
  let manifest' = (match ty_decl.Scopedtree.ptype_manifest with None -> None
                   | Some core_ty -> Some (subst_core_type subst core_ty)) in
 { Scopedtree.ptype_params = ty_decl.Scopedtree.ptype_params ;
   Scopedtree.ptype_cstrs = cstrs' ;
   Scopedtree.ptype_kind =
                 subst_type_kind subst ty_decl.Scopedtree.ptype_kind ;
   Scopedtree.ptype_manifest = manifest' ;
   Scopedtree.ptype_loc = ty_decl.Scopedtree.ptype_loc ;
   (* XXX Not sure *)
   Scopedtree.ptype_path =
          ref (Subst.path !(ty_decl.Scopedtree.ptype_path) subst) }
;;



let subst_exception_declaration subst = function
  | None -> None
  | Some corety ->
      let corety' = subst_core_type subst corety in
      Some corety'
;;
