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



let scope_module_expression_forward = ref (fun _ _ -> assert false) ;;



(* Used to raise an error when Or_pats bind variables *)
exception Or_pattern_must_not_bind ;;



(* val scope_type_declaration:                             *)
(*  Envscope.t -> Ident.t -> Parsetree.type_declaration -> *)
(*                Scopedtree.type_declaration * Envscope.t *)
(* Scope a type declaration. Assumes that type name of this declaration *)
(* is already binded in the environment to treat recursive definitions. *)
(* Return the <extended scoping environment> including bindings for     *)
(* potential constructors found in the declaration.                     *)
let rec scope_type_declaration scenv type_ident tydecl =
  let scoped_cstrs = List.map (fun (ct1, ct2, loc) ->
                                let ct1' = scope_core_type scenv ct1 in
				let ct2' = scope_core_type scenv ct2 in
				(ct1', ct2', loc))
                              tydecl.Parsetree.ptype_cstrs in
  let (scoped_kind, new_scenv) =
                         scope_kind scenv tydecl.Parsetree.ptype_kind in
  let scoped_manifest = (match tydecl.Parsetree.ptype_manifest with
                     | None -> None
		     (* We scope in old envt because if a type is manifest *)
                     (* it can't also be variant so new_scenv = scenv      *)
                     | Some core_ty -> Some (scope_core_type scenv core_ty)) in
  let scoped_declaration =
    { Scopedtree.ptype_params = tydecl.Parsetree.ptype_params ;
      Scopedtree.ptype_cstrs = scoped_cstrs ;
      (* Tells if type is abstract, variant or record *)
      Scopedtree.ptype_kind = scoped_kind ;
      (* Tells wether type is indeed an abbrev *)
      Scopedtree.ptype_manifest = scoped_manifest ;
      Scopedtree.ptype_loc = tydecl.Parsetree.ptype_loc ;
      (* The ident assign to this type name is already in the environment *)
      (* (for recursive types) so we must get it, make a path from it and *)
      (* make a ref on this path. This path is the one that MUST be used  *)
      (* everywhere we build a type expression with this type constructor *)
      (* name.                                                            *)
      Scopedtree.ptype_path = ref (Path.Pident type_ident) } in
  (scoped_declaration, new_scenv)



(* val scope_kind:                                                          *)
(*   Envscope.t -> Parsetree.type_kind -> Scopedtree.type_kind * Envscope.t *)
(* A kind specifiy if the type declared is: *)
(*    - an abstract type                    *)
(*    - a variant type (with constructors)  *)
(*    - a record                            *)
(* Return the scoped kind and the <extended> scoping environment *)
(* including identifiers induced by the type declaration         *)
and scope_kind scenv = function
  | Parsetree.Ptype_abstract ->
      (Scopedtree.Ptype_abstract, scenv)
  | Parsetree.Ptype_variant constructors ->
      (* Scope constructors, add them to the environment and get it *)
      let (scoped_constructors, new_scenv) =
	                      scope_constructors scenv constructors in
      (Scopedtree.Ptype_variant scoped_constructors,
       new_scenv)
  | Parsetree.Ptype_record fields ->
      let (scoped_fields, new_scenv) = scope_record_fields scenv fields in
      (Scopedtree.Ptype_record scoped_fields,
       new_scenv)



(* val scope_record_fields:                                                  *)
(*  Envscope.t ->                                                            *)
(*  (string * Asttypes.mutable_flag * Parsetree.core_type) list ->           *)
(* (string * Asttypes.mutable_flag * Scopedtree.core_type) list * Envscope.t *)
(* Scope fields type of a record and add the fields names in *)
(* the scoping environment                                   *)
and scope_record_fields scenv = function
 | [] -> ([], scenv)
 | (lblname, mut_flag, core_ty) :: rem ->
     let scenv' = Envscope.enter_label lblname scenv in
     (* Retrieve label ident from string *)
     let lbl_id = Envscope.retrieve_scoped_label lblname scenv' in
     let scoped_core_ty = scope_core_type scenv core_ty in
     let (scoped_rem, scenv'') = scope_record_fields scenv' rem in
     ((lbl_id, mut_flag, scoped_core_ty) :: scoped_rem, scenv'')



(* val scope_constructors:                                      *)
(*    Envscope.t -> (string * Parsetree.core_type list) list -> *)
(*    (Ident.t * Scopedtree.core_type list) list * Envscope.t    *)
(* Scope constructors args of a variant type declaration and add the *)
(* constructors names in the scoping environment                     *)
and scope_constructors scenv = function
  | [] -> ([], scenv)
  | (cname, cargs) :: rem ->
      (* To simplify, we enter the constructor as a value *)
      let scenv' = Envscope.enter_value cname scenv in
      (* Retrieve the freshly inserted ident for constructor name *)
      let cname_ident = Envscope.retrieve_scoped_value cname scenv' in
      let scoped_args = List.map (scope_core_type scenv) cargs in
      let (scoped_rem, scenv'') = scope_constructors scenv' rem in
      ((cname_ident, scoped_args) :: scoped_rem, scenv'')



(* val scope_core_type: Envscope.t -> Parsetree.core_type -> *)
(*                      Scopedtree.core_type                 *)
and scope_core_type scenv core_ty =
 { Scopedtree.ptyp_desc =
             scope_core_type_desc scenv core_ty.Parsetree.ptyp_desc ;
   Scopedtree.ptyp_loc = core_ty.Parsetree.ptyp_loc }



(* val scope_core_type_desc : Envscope.t -> Parsetree.core_type_desc -> *)
(*                                          Scopedtree.core_type_desc   *)
and scope_core_type_desc scenv = function
  | Parsetree.Ptyp_any -> Scopedtree.Ptyp_any
  | Parsetree.Ptyp_var varname -> Scopedtree.Ptyp_var varname
  | Parsetree.Ptyp_arrow (core_ty0, core_ty1) ->
      Scopedtree.Ptyp_arrow (scope_core_type scenv core_ty0,
			     scope_core_type scenv core_ty1)
  | Parsetree.Ptyp_tuple core_tys ->
      Scopedtree.Ptyp_tuple (List.map (scope_core_type scenv) core_tys)
  | Parsetree.Ptyp_constr (lident, core_tys) ->
      let scoped_ident = Envscope.type_longident lident scenv in
      let scoped_core_tys = List.map (scope_core_type scenv) core_tys in
      (* The reference scoped_ident is temporary. It will be     *)
      (* replaced during expansion and checking by the reference *)
      (* that must be common to all type expression using this   *)
      (* type constructor name.                                  *)
      Scopedtree.Ptyp_constr (ref scoped_ident, scoped_core_tys)
  | Parsetree.Ptyp_object _ -> failwith "OBJECTS NOT IMPLEMENTED"
  | Parsetree.Ptyp_class (_, _) -> failwith "OBJECTS NOT IMPLEMENTED"
  | Parsetree.Ptyp_alias (core_ty, alias_name) ->
      let scoped_core_ty = scope_core_type scenv core_ty in
      Scopedtree.Ptyp_alias (scoped_core_ty, alias_name)



(* val scope_core_field_type: Envscope.t -> Parsetree.core_field_type -> *)
(*                                          Scopedtree.core_field_type   *)
and scope_core_field_type scenv fty =
 { Scopedtree.pfield_desc =
       scope_core_field_desc scenv fty.Parsetree.pfield_desc ;
   Scopedtree.pfield_loc = fty.Parsetree.pfield_loc }



(* val scope_core_field_desc: Envscope.t -> Parsetree.core_field_desc -> *)
(*                                          Scopedtree.core_field_desc   *)
and scope_core_field_desc scenv = function
  | Parsetree.Pfield (fname, core_ty) ->
      let scoped_core_ty = scope_core_type scenv core_ty in
      Scopedtree.Pfield (fname, scoped_core_ty)
  | Parsetree.Pfield_var -> Scopedtree.Pfield_var
;;



(* Check if a pattern contains variables. Useful to avoid Or_pats *)
(* to bind variable, that is forbidden ni Objective Caml.         *)
let rec pattern_contains_var pat =
 pattern_desc_contains_var pat.Parsetree.ppat_desc



and pattern_desc_contains_var = function
  | Parsetree.Ppat_any -> false
  | Parsetree.Ppat_var _ -> true
  | Parsetree.Ppat_alias (_, _) -> true
  | Parsetree.Ppat_constant _ -> false
  | Parsetree.Ppat_tuple patts -> List.exists pattern_contains_var patts
  | Parsetree.Ppat_construct (_, pat_opt, _) ->
      (match pat_opt with None -> false | Some p -> pattern_contains_var p)
  | Parsetree.Ppat_record fields ->
      List.exists (fun (_, p) -> pattern_contains_var p) fields
  | Parsetree.Ppat_array patts -> List.exists pattern_contains_var patts
  | Parsetree.Ppat_or (pat0, pat1) ->
      pattern_contains_var pat0 || pattern_contains_var pat1
  | Parsetree.Ppat_constraint (pat, _) -> pattern_contains_var pat
;;



(* val scope_expression: Envscope.t -> Parsetree.expression -> *)
(*                       Scopedtree.expression                 *)
(* Scope an expression *)
let rec scope_expression scenv expr =
  { Scopedtree.pexp_desc = scope_expression_desc
                                     scenv
                                     expr.Parsetree.pexp_desc ;
    Scopedtree.pexp_loc = expr.Parsetree.pexp_loc }



(* val scope_let_definition: Envscope.t -> Asttypes.rec_flag ->              *)
(*            (Parsetree.pattern * Parsetree.expression) list ->             *)
(*            (Scopedtree.pattern * Scopedtree.expression) list * Envscope.t *)
(* Scope bindings of a let rec or non rec definition              *)
(* Return the <new> scoping environment including bindings due to *)
(* patterns found in the 'let'.                                   *)
and scope_let_definition scenv rec_flag bindings =
  match rec_flag with
   | Asttypes.Nonrecursive ->
       (* Non recursive let bindings *)
       let (scoped_bindings, scenv_extensions) =
	 List.split (
 	     List.map (fun (pattern, expr) ->
	     (* scope_pattern returns the scoped pattern     *)
               (* and an extension for the scoping environment *)
               let (scoped_pattern, scenv_ext) = scope_pattern scenv pattern in
	       (* We must scope expression in the old env (no rec) *)
	       let scoped_expr = scope_expression scenv expr in
	       (* Return new environment *)
	       ((scoped_pattern, scoped_expr), scenv_ext))
         bindings) in
       (* We build the extended environment *)
       let new_scenv = List.fold_left
                        (fun env_accu env -> Envscope.append env env_accu)
                        scenv
	                scenv_extensions in
       (scoped_bindings, new_scenv)
   | Asttypes.Recursive ->
       (* Recursive let bindings *)
       (* We first scope all patterns *)
       let (scoped_patterns, scenv_extensions) =
	 List.split (
	     List.map (fun (pattern, _) -> scope_pattern scenv pattern)
	              bindings) in
       (* We build the extended environment *)
       let new_scenv = List.fold_left
                         (fun env_accu env -> Envscope.append env env_accu)
                             scenv
	                     scenv_extensions in
       (* We scope binded expressions in this environment *)
       let scoped_exprs =
	     List.map (fun (_, expr) -> scope_expression new_scenv expr)
	              bindings in
       let scoped_bindings = List.combine scoped_patterns scoped_exprs in
       (scoped_bindings, new_scenv)



(* val scope_expression_desc:                                               *)
(*    Envscope.t -> Parsetree.expression_desc -> Scopedtree.expression_desc *)
(* Scope an expression description *)
and scope_expression_desc scenv = function
  | Parsetree.Pexp_ident longident ->
      Scopedtree.Pexp_ident (Envscope.value_longident longident scenv)
  | Parsetree.Pexp_constant constant ->
      Scopedtree.Pexp_constant constant
  | Parsetree.Pexp_let (rec_flag, bindings, expr) ->
      let (scoped_bindings, new_scenv) =
	       scope_let_definition scenv rec_flag bindings in
      let scoped_expr = scope_expression new_scenv expr in
      Scopedtree.Pexp_let (rec_flag, scoped_bindings, scoped_expr)
  | Parsetree.Pexp_function bindings ->
      let scoped_bindings =
         List.map
           (fun (pattern, body) ->
              let (scoped_pattern, scenv_ext) = scope_pattern scenv pattern in
              let new_scenv = Envscope.append scenv_ext scenv in
              let scoped_body = scope_expression new_scenv body in
              (scoped_pattern, scoped_body))
           bindings in
      Scopedtree.Pexp_function scoped_bindings
  | Parsetree.Pexp_apply (expr0, exprs) ->
      let scoped_expr0 = scope_expression scenv expr0 in
      let scoped_exprs = List.map (scope_expression scenv) exprs in
      Scopedtree.Pexp_apply (scoped_expr0, scoped_exprs)
  | Parsetree.Pexp_match (expr, bindings) ->
      let scoped_expr = scope_expression scenv expr in
      let scoped_bindings =
         List.map
           (fun (pattern, body) ->
              let (scoped_pattern, scenv_ext) = scope_pattern scenv pattern in
              let new_scenv = Envscope.append scenv_ext scenv in
              let scoped_body = scope_expression new_scenv body in
              (scoped_pattern, scoped_body))
           bindings in
      Scopedtree.Pexp_match (scoped_expr, scoped_bindings)
  | Parsetree.Pexp_try (expr, bindings) ->
      let scoped_expr = scope_expression scenv expr in
      let scoped_bindings =
         List.map
           (fun (pattern, body) ->
              let (scoped_pattern, scenv_ext) = scope_pattern scenv pattern in
              let new_scenv = Envscope.append scenv_ext scenv in
              let scoped_body = scope_expression new_scenv body in
              (scoped_pattern, scoped_body))
           bindings in
      Scopedtree.Pexp_try (scoped_expr, scoped_bindings)
  | Parsetree.Pexp_tuple exprs ->
      let scoped_exprs = List.map (scope_expression scenv) exprs in
      Scopedtree.Pexp_tuple scoped_exprs
  | Parsetree.Pexp_construct (longident, expr_opt, _) ->
      let scoped_longident = Envscope.value_longident longident scenv in
      let scoped_opt_expr = (match expr_opt with None -> None
                             | Some e -> Some (scope_expression scenv e)) in
      Scopedtree.Pexp_construct (scoped_longident, scoped_opt_expr)
  | Parsetree.Pexp_record (fields, expr_opt) ->
      let scoped_fields = List.map
         (fun (lident, expr) ->
	   let scoped_lident = Envscope.label_longident lident scenv in
	   let scoped_expr = scope_expression scenv expr in
	   (scoped_lident, scoped_expr))
         fields in
      let scoped_opt_expr = (match expr_opt with None -> None
                             | Some e -> Some (scope_expression scenv e)) in
      Scopedtree.Pexp_record (scoped_fields, scoped_opt_expr)
  | Parsetree.Pexp_field (expr, longident) ->
      let scoped_expr = scope_expression scenv expr in
      let scoped_longident = Envscope.label_longident longident scenv in
      Scopedtree.Pexp_field (scoped_expr, scoped_longident)
  | Parsetree.Pexp_setfield (expr0, longident, expr1) ->
      let scoped_expr0 = scope_expression scenv expr0 in
      let scoped_longident = Envscope.label_longident longident scenv in
      let scoped_expr1 = scope_expression scenv expr1 in
      Scopedtree.Pexp_setfield (scoped_expr0, scoped_longident, scoped_expr1)
  | Parsetree.Pexp_array exprs ->
      let scoped_exprs = List.map (scope_expression scenv) exprs in
      Scopedtree.Pexp_array scoped_exprs
  | Parsetree.Pexp_ifthenelse (expr0, expr1, expr2_opt) ->
      let scoped_expr0 = scope_expression scenv expr0 in
      let scoped_expr1 = scope_expression scenv expr1 in
      let scoped_expr2_opt = (match expr2_opt with None -> None
                              | Some e -> Some (scope_expression scenv e)) in
      Scopedtree.Pexp_ifthenelse (scoped_expr0, scoped_expr1, scoped_expr2_opt)
  | Parsetree.Pexp_sequence (expr0, expr1) ->
      let scoped_expr0 = scope_expression scenv expr0 in
      let scoped_expr1 = scope_expression scenv expr1 in
      Scopedtree.Pexp_sequence (scoped_expr0, scoped_expr1)
  | Parsetree.Pexp_while (expr0, expr1) ->
      let scoped_expr0 = scope_expression scenv expr0 in
      let scoped_expr1 = scope_expression scenv expr1 in
      Scopedtree.Pexp_while (scoped_expr0, scoped_expr1)
  | Parsetree.Pexp_for (index_name, expr0, expr1, dir_flag, expr2) ->
      let scoped_expr0 = scope_expression scenv expr0 in
      let scoped_expr1 = scope_expression scenv expr1 in
      (* Scope loop body in environment extended by the index *)
      let scenv' = Envscope.enter_value index_name scenv in
      let scoped_expr2 = scope_expression scenv' expr2 in
      (* Retrieve the newly created ident to put it in *)
      (* the scoped Pexp_for node.                     *)
      let index_ident = Envscope.retrieve_scoped_value index_name scenv' in
      Scopedtree.Pexp_for (index_ident, scoped_expr0, scoped_expr1,
                           dir_flag, scoped_expr2)
  | Parsetree.Pexp_constraint (expr, core_ty_opt1, core_ty_opt2) ->
      begin
      match (core_ty_opt1, core_ty_opt2) with
       | (Some ct, None) ->
	   let scoped_expr = scope_expression scenv expr in
           let scoped_core_ty_opt1 = Some (scope_core_type scenv ct) in
	   Scopedtree.Pexp_constraint (scoped_expr, scoped_core_ty_opt1,
				       None)
       | (_, _) ->
           (* We don't yet handle substyping constraints. *)
	   failwith "SUBTYPING NOT IMPLEMENTED"
      end
  | Parsetree.Pexp_when (expr0, expr1) ->
      let scoped_expr0 = scope_expression scenv expr0 in
      let scoped_expr1 = scope_expression scenv expr1 in
      Scopedtree.Pexp_when (scoped_expr0, scoped_expr1)
  | Parsetree.Pexp_send (_, _) -> failwith "OBJECTS NOT IMPLEMENTED"
  | Parsetree.Pexp_new _ -> failwith "OBJECTS NOT IMPLEMENTED"
  | Parsetree.Pexp_setinstvar (_, _) -> failwith "OBJECTS NOT IMPLEMENTED"
  | Parsetree.Pexp_override _ -> failwith "OBJECTS NOT IMPLEMENTED"
  | Parsetree.Pexp_letmodule (mod_name, mod_expr, expr) ->
      let (scoped_mod_expr, rec_scenv) =
	!scope_module_expression_forward scenv mod_expr in
      let new_scenv = Envscope.enter_module mod_name rec_scenv scenv in
      let scoped_expr = scope_expression new_scenv expr in
      let mod_ident = Envscope.retrieve_scoped_module mod_name new_scenv in
      Scopedtree.Pexp_letmodule (mod_ident, scoped_mod_expr, scoped_expr)



(* val scope_pattern: Envscope.t -> Parsetree.pattern -> *)
(*                    Scopedtree.pattern * Envscope.t    *)
(* Returns the scoped pattern and an <extension> for the scoping environment *)
and scope_pattern scenv pat =
  let (scoped_pat_desc, scenv_ext) =
                   scope_pattern_descr scenv pat.Parsetree.ppat_desc in
  ({ Scopedtree.ppat_desc = scoped_pat_desc ;
     Scopedtree.ppat_loc = pat.Parsetree.ppat_loc },
   scenv_ext)



(* val scope_pattern_descr: Envscope.t ->                                  *)
(*          Parsetree.pattern_desc -> Scopedtree.pattern_desc * Envscope.t *)
and scope_pattern_descr scenv = function
  | Parsetree.Ppat_any -> (Scopedtree.Ppat_any, Envscope.empty)
  | Parsetree.Ppat_var var_name ->
      let scenv' = Envscope.enter_value var_name Envscope.empty in
      (* Retrieve the newly created ident to put it in *)
      (* the scoped Ppat_var node.                     *)
      let var_ident = Envscope.retrieve_scoped_value var_name scenv' in
      (Scopedtree.Ppat_var var_ident, scenv')
  | Parsetree.Ppat_alias (pat, alias_name) ->
      let (scoped_pat, scenv_ext) = scope_pattern scenv pat in
      let scenv_ext2 = Envscope.enter_value alias_name scenv_ext in
      (* Retrieve ident from string *)
      let alias_ident = Envscope.retrieve_scoped_value alias_name scenv_ext2 in
      (Scopedtree.Ppat_alias (scoped_pat, alias_ident), scenv_ext2 )
  | Parsetree.Ppat_constant constant ->
      (Scopedtree.Ppat_constant constant, Envscope.empty)
  | Parsetree.Ppat_tuple patts ->
      let (scoped_patts, scenv_exts) = List.split (
	List.map (scope_pattern scenv) patts) in
      (* Build the new envt extension as the union of all sub-extensions *)
      let new_scenv_ext =
	List.fold_left Envscope.append Envscope.empty scenv_exts in
      (Scopedtree.Ppat_tuple scoped_patts, new_scenv_ext)
  | Parsetree.Ppat_construct (longident, pat_opt, _) ->
      let scoped_longident = Envscope.value_longident longident scenv in
      let (scoped_pat_opt, scenv_ext) = (match pat_opt with
                            | None -> (None, Envscope.empty)
                            | Some p -> let (p', e') = scope_pattern scenv p in
                                        (Some p', e')) in
      (Scopedtree.Ppat_construct (scoped_longident, scoped_pat_opt), scenv_ext)
  | Parsetree.Ppat_record fields ->
      let (scoped_fields, scenv_exts) = List.split (
	List.map (fun (lident, pat) ->
	           let scoped_lident = Envscope.label_longident lident scenv in
                   let (scoped_pat, scenv_ext) = scope_pattern scenv pat in
                   ((scoped_lident, scoped_pat), scenv_ext))
	         fields) in
      (* Build the new envt extension as the union of all sub-extensions *)
      let new_scenv_ext =
	List.fold_left Envscope.append Envscope.empty scenv_exts in
      (Scopedtree.Ppat_record scoped_fields, new_scenv_ext)
  | Parsetree.Ppat_array patts ->
      let (scoped_patts, scenv_exts) = List.split (
	List.map (scope_pattern scenv) patts) in
      (* Build the new envt extension as the union of all sub-extensions *)
      let new_scenv_ext =
	List.fold_left Envscope.append Envscope.empty scenv_exts in
      (Scopedtree.Ppat_array scoped_patts, new_scenv_ext)
  | Parsetree.Ppat_or (pat0, pat1) ->
      (* Check that this Or_pat does not bind variables *)
      if pattern_contains_var pat0 || pattern_contains_var pat1 then
	raise Or_pattern_must_not_bind ;
      let (scoped_pat0, scenv_ext0) = scope_pattern scenv pat0 in
      let (scoped_pat1, scenv_ext1) = scope_pattern scenv pat1 in
      (Scopedtree.Ppat_or (scoped_pat0, scoped_pat1),
       Envscope.append scenv_ext1 scenv_ext0)
  | Parsetree.Ppat_constraint (pat, core_ty) ->
      let (scoped_pat, scenv_ext) = scope_pattern scenv pat in
      (* No use to use the extension for scoping core_type constraint *)
      (* because sets of idents for types and patterns are disjoint.  *)
      let scoped_core_ty = scope_core_type scenv core_ty in
      (Scopedtree.Ppat_constraint (scoped_pat, scoped_core_ty),
       scenv_ext)
;;



let rec scope_annoted_core_type scenv act =
  { Scopedtree.patyp_desc =
           scope_annoted_core_type_desc scenv act.Parsetree.patyp_desc ;
    Scopedtree.patyp_loc = act.Parsetree.patyp_loc }



and scope_annoted_core_type_desc scenv = function
  | Parsetree.Patyp_var var -> Scopedtree.Patyp_var var
  | Parsetree.Patyp_arrow (act1, ann, act2) ->
      Scopedtree.Patyp_arrow (scope_annoted_core_type scenv act1,
			      scope_annotation_expr scenv ann,
			      scope_annoted_core_type scenv act2)
  | Parsetree.Patyp_tuple args ->
      Scopedtree.Patyp_tuple (List.map (scope_annoted_core_type scenv) args)
  | Parsetree.Patyp_constr (ident, args, ann) ->
      let scoped_ident = Envscope.type_longident ident scenv in
      let scoped_args = List.map (scope_annoted_core_type scenv) args in
      let scoped_ann = scope_annotation_expr scenv ann in
      (* The reference scoped_ident is temporary. It will be     *)
      (* replaced during expansion and checking by the reference *)
      (* that must be common to all type expression using this   *)
      (* type constructor name.                                  *)
      Scopedtree.Patyp_constr (ref scoped_ident, scoped_args, scoped_ann)



and scope_annotation_expr scenv = function
  | Parsetree.Pann_bottom -> Scopedtree.Pann_bottom
  | Parsetree.Pann_explicit (annotation_elems, rvname) ->
      let scoped_annotation_elems =
	 List.map (scope_annotation_elem_expr scenv) annotation_elems in
      Scopedtree.Pann_explicit (scoped_annotation_elems,  rvname)



and scope_annotation_elem_expr scenv = function
  | Parsetree.Pelem_constant (v, presence) ->
      Scopedtree.Pelem_constant (v, scope_presence_expr presence)
  | Parsetree.Pelem_param (v, param) ->
      let scoped_param = scope_annoted_core_type scenv param in
      Scopedtree.Pelem_param (v, scoped_param)
  | Parsetree.Pelem_record fields ->
      let fields' = List.map
	               (fun (n, act) -> (n, scope_annoted_core_type scenv act))
	               fields in
      Scopedtree.Pelem_record (fields')



and scope_presence_expr = function
  | Parsetree.Prexpr_pre -> Scopedtree.Prexpr_pre
  | Parsetree.Prexpr_var name -> Scopedtree.Prexpr_var name
;;


