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




let new_var_name =
  let cpt = ref 0 in
  (function () ->
    incr cpt ;
    ("_" ^ (string_of_int !cpt)))
 ;;



let rec is_pattern_absorbant pattern =
 match pattern.Parsetree.ppat_desc with
  | Parsetree.Ppat_any -> true
  | Parsetree.Ppat_var _ -> true
  | Parsetree.Ppat_alias (pat, _) -> is_pattern_absorbant pat
  | Parsetree.Ppat_constant _ -> false
  | Parsetree.Ppat_tuple arg_pats ->
      List.for_all is_pattern_absorbant arg_pats
  | Parsetree.Ppat_construct (_, _, _) -> false
  | Parsetree.Ppat_record labels ->
      List.for_all (fun (_, pat) -> is_pattern_absorbant pat) labels
  | Parsetree.Ppat_array _ -> false
  | Parsetree.Ppat_or (pat1, pat2) ->
      is_pattern_absorbant pat1 && is_pattern_absorbant pat2
  | Parsetree.Ppat_constraint (pat, _) -> is_pattern_absorbant pat
;;




(* Creates an expression binding "x -> raise x" *)
let create_raise_expr () =
  let vname = new_var_name () in
  (* The pattern expression... *)
  let p = { Parsetree.ppat_desc = Parsetree.Ppat_var vname ;
	    Parsetree.ppat_loc = Location.none } in
  (* The "raise" expression *)
  let e_raise = { Parsetree.pexp_desc =
		           Parsetree.Pexp_ident (Longident.Lident "raise") ;
		  Parsetree.pexp_loc = Location.none } in
  (* The "x" expression *)
  let e_x = { Parsetree.pexp_desc =
	                   Parsetree.Pexp_ident (Longident.Lident vname) ;
	      Parsetree.pexp_loc = Location.none } in
  (* The "apply" expression *)
  let e_apply = { Parsetree.pexp_desc = Parsetree.Pexp_apply (e_raise, [e_x]) ;
		  Parsetree.pexp_loc = Location.none } in
  (p, e_apply)
;;



let rec extend_try_bindings lst =
  (* The boolean 'found_absorbant' tells us if we already found *)
  (* a totally absorbant case. In this case, it's not useful to *)
  (* add a trash case. That's just only avoid making the source *)
  (* grow for nothing.                                          *)
  let rec rec_extend found_absorbant = function
    | [] -> if found_absorbant then []
	    else
	      (* No absorbant case found so we must add one *)
	      [ create_raise_expr () ]
    | (p, e) :: rem ->
	let e' = extend_expression_handler e in
	(* Pattern is absorbant if Any or Var or Alias (rec) *)
	(* or Or (rec) and not guarded                       *)
	let found = is_pattern_absorbant p &&
	  (match e.Parsetree.pexp_desc with Parsetree.Pexp_when (_, _) -> false
            | _ -> true) in
	(p, e') :: rec_extend (found_absorbant || found) rem

  in rec_extend false lst



and extend_expression_handler expr =
  { Parsetree.pexp_desc =
            extend_expression_desc_handler expr.Parsetree.pexp_desc ;
    Parsetree.pexp_loc = expr.Parsetree.pexp_loc }



and extend_expression_desc_handler = function
  | Parsetree.Pexp_ident _ as whatever -> whatever
  | Parsetree.Pexp_constant _ as whatever -> whatever
  | Parsetree.Pexp_let (rec_flag, bindings, expr) ->
      let bindings' = List.map (fun (p, e) -> (p, extend_expression_handler e))
	                       bindings in
      let expr' = extend_expression_handler expr in
      Parsetree.Pexp_let (rec_flag, bindings', expr')
  | Parsetree.Pexp_function bindings ->
      let bindings' = List.map (fun (p, e) -> (p, extend_expression_handler e))
	                       bindings in
      Parsetree.Pexp_function bindings'
  | Parsetree.Pexp_apply (expr, exprs) ->
      let expr' = extend_expression_handler expr in
      let exprs' = List.map extend_expression_handler exprs in
      Parsetree.Pexp_apply (expr', exprs')
  | Parsetree.Pexp_match (expr, bindings) ->
      let expr' = extend_expression_handler expr in
      let bindings' = List.map (fun (p, e) -> (p, extend_expression_handler e))
	                       bindings in
      Parsetree.Pexp_match (expr', bindings')
  | Parsetree.Pexp_try (expr, bindings) ->
      let expr' = extend_expression_handler expr in
      let bindings' = extend_try_bindings bindings in
      Parsetree.Pexp_try (expr', bindings')
  | Parsetree.Pexp_tuple exprs ->
      let exprs' = List.map extend_expression_handler exprs in
    Parsetree.Pexp_tuple exprs'
  | Parsetree.Pexp_construct (lident, expr_opt, b) ->
      let expr_opt' = (match expr_opt with None -> None
                       | Some e -> Some (extend_expression_handler e)) in
      Parsetree.Pexp_construct (lident, expr_opt', b)
  | Parsetree.Pexp_record (labels, expr_opt) ->
      let labels' = List.map (fun (l, e) -> (l, extend_expression_handler e))
	                     labels in
      let expr_opt' = (match expr_opt with None -> None
                       | Some e -> Some (extend_expression_handler e)) in
      Parsetree.Pexp_record (labels', expr_opt')
  | Parsetree.Pexp_field (expr, lident) ->
      let expr' = extend_expression_handler expr in
      Parsetree.Pexp_field (expr', lident)
  | Parsetree.Pexp_setfield (expr0, lident, expr1) ->
      let expr0' = extend_expression_handler expr0 in
      let expr1' = extend_expression_handler expr1 in
      Parsetree.Pexp_setfield (expr0', lident, expr1')
  | Parsetree.Pexp_array exprs ->
      let exprs' = List.map extend_expression_handler exprs in
      Parsetree.Pexp_array exprs'
  | Parsetree.Pexp_ifthenelse (expr0, expr1, expr2_opt) ->
      let expr0' = extend_expression_handler expr0 in
      let expr1' = extend_expression_handler expr1 in
      let expr2_opt' = (match expr2_opt with None -> None
                       | Some e -> Some (extend_expression_handler e)) in
      Parsetree.Pexp_ifthenelse (expr0', expr1', expr2_opt')
  | Parsetree.Pexp_sequence (expr0, expr1) ->
      let expr0' = extend_expression_handler expr0 in
      let expr1' = extend_expression_handler expr1 in
      Parsetree.Pexp_sequence (expr0', expr1')
  | Parsetree.Pexp_while (expr0, expr1) ->
      let expr0' = extend_expression_handler expr0 in
      let expr1' = extend_expression_handler expr1 in
      Parsetree.Pexp_while (expr0', expr1')
  | Parsetree.Pexp_for (index, expr0, expr1, dir_flg, expr2) ->
      let expr0' = extend_expression_handler expr0 in
      let expr1' = extend_expression_handler expr1 in
      let expr2' = extend_expression_handler expr2 in
      Parsetree.Pexp_for (index, expr0', expr1', dir_flg, expr2')
  | Parsetree.Pexp_constraint (expr0, cty_opt0, cty_opt1) ->
      let expr0' = extend_expression_handler expr0 in
      Parsetree.Pexp_constraint (expr0', cty_opt0, cty_opt1)
  | Parsetree.Pexp_when (expr0, expr1) ->
      let expr0' = extend_expression_handler expr0 in
      let expr1' = extend_expression_handler expr1 in
      Parsetree.Pexp_when (expr0', expr1')
  | Parsetree.Pexp_send (expr, methname) ->
      let expr' = extend_expression_handler expr in
      Parsetree.Pexp_send (expr', methname)
  | Parsetree.Pexp_new _ as whatever -> whatever
  | Parsetree.Pexp_setinstvar (instv, expr) ->
      let expr' = extend_expression_handler expr in
      Parsetree.Pexp_setinstvar (instv, expr')
  | Parsetree.Pexp_override bindings ->
      let bindings' = List.map (fun (n, e) -> (n, extend_expression_handler e))
	                       bindings in
      Parsetree.Pexp_override bindings'
  | Parsetree.Pexp_letmodule (str, mod_expr, expr) ->
      let mod_expr' = extend_module_expr_handler mod_expr in
      let expr' = extend_expression_handler expr in
      Parsetree.Pexp_letmodule (str, mod_expr', expr')



and extend_module_expr_handler mod_expr =
  { Parsetree.pmod_desc =
           extend_module_expr_desc_handler mod_expr.Parsetree.pmod_desc ;
    Parsetree.pmod_loc = mod_expr.Parsetree.pmod_loc }



and extend_module_expr_desc_handler = function
  | Parsetree.Pmod_ident _ as whatever -> whatever
  | Parsetree.Pmod_structure structure ->
      Parsetree.Pmod_structure
             (List.map extend_structure_item_handler structure)
  | Parsetree.Pmod_functor (arg_name, mty, mod_expr) ->
      let mod_expr' = extend_module_expr_handler mod_expr in
      Parsetree.Pmod_functor (arg_name, mty, mod_expr')
  | Parsetree.Pmod_apply (mod_expr0, mod_expr1) ->
      let mod_expr0' = extend_module_expr_handler mod_expr0 in
      let mod_expr1' = extend_module_expr_handler mod_expr1 in
      Parsetree.Pmod_apply (mod_expr0', mod_expr1')
  | Parsetree.Pmod_constraint (mod_expr, mty) ->
      let mod_expr' = extend_module_expr_handler mod_expr in
      Parsetree.Pmod_constraint (mod_expr', mty)


and extend_structure_item_handler stritem =
  { Parsetree.pstr_desc =
       extend_structure_item_desc_handler stritem.Parsetree.pstr_desc ;
    Parsetree.pstr_loc = stritem.Parsetree.pstr_loc }


and extend_structure_item_desc_handler = function
  | Parsetree.Pstr_eval expr ->
      Parsetree.Pstr_eval (extend_expression_handler expr)
  | Parsetree.Pstr_value (rec_flg, bindings) ->
      let bindings' = List.map (fun (n, e) ->
	                          (n, extend_expression_handler e)) bindings in
      Parsetree.Pstr_value (rec_flg, bindings')
  | Parsetree.Pstr_primitive (_, _) as whatever -> whatever
  | Parsetree.Pstr_type _ as whatever -> whatever
  | Parsetree.Pstr_exception (_, _) as whatever -> whatever
  | Parsetree.Pstr_module (mod_name, mod_expr) ->
      let mod_expr' = extend_module_expr_handler mod_expr in
      Parsetree.Pstr_module (mod_name, mod_expr')
  | Parsetree.Pstr_modtype (_, _) as whatever -> whatever
  | Parsetree.Pstr_open _ as whatever -> whatever
  | Parsetree.Pstr_class _ -> failwith "OBJECTS NOT IMPLEMENTED"
  | Parsetree.Pstr_class_type _ -> failwith "OBJECTS NOT IMPLEMENTED"
;;



let extend_structure_handler structure =
 List.map extend_structure_item_handler structure
;;



(* extend_toplevel_phrase_handler: Parsetree.toplevel_phrase -> *)
(*                                 Parsetree.toplevel_phrase    *)
(* Add a trash case in handlers is no already exists. *)
let extend_toplevel_phrase_handler = function
  | Parsetree.Ptop_def structure ->
      Parsetree.Ptop_def (extend_structure_handler structure)
  | Parsetree.Ptop_dir (_, _) as whatever -> whatever
;;
