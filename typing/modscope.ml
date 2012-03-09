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




(* scope_value_description: Envscope.t -> Parsetree.value_description -> *)
(*                          Scopedtree.value_description                 *)
let scope_value_description scenv vd =
 { Scopedtree.pval_type =
         Corescope.scope_core_type scenv vd.Parsetree.pval_type ;
   Scopedtree.pval_prim = vd.Parsetree.pval_prim }
;;



(* scope_annoted_value_description: Envscope.t ->                           *)
(*                                  Parsetree.annoted_value_description ->  *)
(*                                  Scopedtree.annoted_value_description    *)
let scope_annoted_value_description scenv vd =
 { Scopedtree.paval_type =
         Corescope.scope_annoted_core_type scenv vd.Parsetree.paval_type ;
   Scopedtree.paval_prim = vd.Parsetree.paval_prim ;
   Scopedtree.paval_explaination =
      List.map (fun (n, aty) ->
	            (n, Corescope.scope_annoted_core_type scenv aty))
               vd.Parsetree.paval_explaination }
;;



(* scope_module_expression: Envscope.t -> Parsetree.module_expr -> *)
(*                          Scopedtree.module_expr * Envscope.t    *)
let rec scope_module_expression scenv modexpr =
 let (scoped_desc, scenv') =
   scope_module_expression_desc scenv modexpr.Parsetree.pmod_desc in
  ({ Scopedtree.pmod_desc = scoped_desc ;
     Scopedtree.pmod_loc = modexpr.Parsetree.pmod_loc },
   scenv')



(* scope_module_expression_desc: Envscope.t -> Parsetree.module_expr_desc -> *)
(*                               Scopedtree.module_expr_desc * Envscope.t    *)
(* Scopes a module expression description. Returns this description once *)
(* scoped. Also returns an environment representing components present   *)
(* in this module expression. This will be used for "open" directive.    *)
and scope_module_expression_desc scenv = function
  | Parsetree.Pmod_ident longident ->
      (Scopedtree.Pmod_ident (Envscope.module_longident longident scenv),
       Envscope.mod_longident_evt longident scenv)
  | Parsetree.Pmod_structure structure ->
      let (scoped_structure, scenv') = scope_structure scenv structure in
      (* Just get the components added by the module expression *)
      (* Because we are given a new complete scope env, and we just need *)
      (* bindings added by this module expression, we want to take just  *)
      (* the part this module expression really added.                   *)
      let rec_scenv = Envscope.diff scenv' scenv in
      (Scopedtree.Pmod_structure scoped_structure,
       rec_scenv)
  | Parsetree.Pmod_functor (arg_name, _, mod_expr_body) ->
      (* Add the argument of the functor in the environment *)
      let scenv' = Envscope.enter_module arg_name Envscope.empty scenv in
      let (scoped_mod_expr_body, rec_scenv) = scope_module_expression scenv'
                                                         mod_expr_body in
      (* Retrieve ident given to the module name *)
      let arg_ident = Envscope.retrieve_scoped_module arg_name scenv' in
      (* Forget signature of the argument *)
      (Scopedtree.Pmod_functor (arg_ident, scoped_mod_expr_body),
       rec_scenv (* Components of the result are those of the func's body *))
  | Parsetree.Pmod_apply (mod_expr0, mod_expr1) ->
      let (scoped_mod_expr0, rec_scenv) =
	  scope_module_expression scenv mod_expr0 in
      let (scoped_mod_expr1, _) = scope_module_expression scenv mod_expr1 in
      (Scopedtree.Pmod_apply (scoped_mod_expr0, scoped_mod_expr1),
       rec_scenv (* Components of the result are those of the functor*))
  | Parsetree.Pmod_constraint (mod_expr, _) ->
      (* Forget constraint *)
      let (scoped_mod_expr, rec_scenv) =
	scope_module_expression scenv mod_expr in
      (scoped_mod_expr.Scopedtree.pmod_desc,
       rec_scenv)




(* val scope_structure: Envscope.t -> Parsetree.structure ->            *)
(*                                    Scopedtree.structure * Envscope.t *)
(* Return the scoped structure and the scope environment <extended> with *)
(* bindings for this structure                                           *)
and scope_structure scenv = function
  | [] -> ([], scenv)
  | item :: rem ->
      (* scope_structure_item return the scoped item and the scope    *)
      (* environment <extended> with bindings for this structure item *)
      let (scoped_item, new_scenv) = scope_structure_item scenv item in
      let (scoped_rem, new_scenv') = scope_structure new_scenv rem in
      ((scoped_item :: scoped_rem), new_scenv')



(* val scope_structure_item: Envscope.t ->                                   *)
(*        Parsetree.structure_item -> Scopedtree.structure_item * Envscope.t *)
(* Return the scoped item and the scope environment <extended> with *)
(* bindings for this structure item                                 *)
and scope_structure_item scenv item =
  let (scoped_item_desc, new_scenv) = scope_structure_item_descr
                                              scenv item.Parsetree.pstr_desc in
  let scoped_structure_item =
    { Scopedtree.pstr_desc = scoped_item_desc ;
      Scopedtree.pstr_loc = item.Parsetree.pstr_loc } in
  (scoped_structure_item, new_scenv)



(* val scope_structure_item_descr: Envscope.t ->                             *)
(*                               Parsetree.structure_item_desc ->            *)
(*                               Scopedtree.structure_item_desc * Envscope.t *)
(* Return the scoped item_descr and the scope environment *)
(* <extended> with bindings for this structure item       *)
and scope_structure_item_descr scenv = function
  | Parsetree.Pstr_eval expr ->
      (* Induce no scope binding *)
      (Scopedtree.Pstr_eval (Corescope.scope_expression scenv expr),
       scenv)
  | Parsetree.Pstr_value (rec_flag, bindings) ->
      let (scoped_bindings, new_scenv) =
	Corescope.scope_let_definition scenv rec_flag bindings in
      (Scopedtree.Pstr_value (rec_flag, scoped_bindings),
       new_scenv)
  | Parsetree.Pstr_primitive (prim_name, value_descr) ->
      let new_scenv = Envscope.enter_value prim_name scenv in
      (* Retrieve the ident from the name. Needed because inscoped *)
      (* tree, we use Ident.t instead of simple string for names.  *)
      let prim_ident = Envscope.retrieve_scoped_value prim_name new_scenv in
      let scoped_item = Scopedtree.Pstr_primitive (prim_ident,
			  scope_annoted_value_description scenv value_descr) in
      (scoped_item, new_scenv)
  | Parsetree.Pstr_type types ->
      (* Build the extended environment including types names scoped *)
      let scenv_with_names =
	 List.fold_left
	    (fun accu_env (ty_name, _) -> Envscope.enter_type ty_name accu_env)
	    scenv types in
      let (scoped_types, new_envsc) =
	           scope_type_declaration_components scenv_with_names types in
      (Scopedtree.Pstr_type scoped_types, new_envsc)
  | Parsetree.Pstr_exception (exc_name, exc_declaration) ->
      (* We must enter the exception name as a value *)
      let new_envsc = Envscope.enter_value exc_name scenv in
      (* Scope arguments in old env. Because types and values are disjoint *)
      (* for args scoping, new_envsc = scenv. So don't matter.             *)
      let scoped_exc_declaration =
       (* Let's transform arguments list to a tuple if there are several *)
       (* arguments. If there's none, then we say None. If there's only  *)
       (* one, we say Some of (this only argument).                      *)
       (match exc_declaration with
         | [] -> None
         | [only_one] -> Some (Corescope.scope_core_type scenv only_one)
         | _ ->
             let many_scoped =
	       List.map (Corescope.scope_core_type scenv) exc_declaration in
	     Some { Scopedtree.ptyp_desc = Scopedtree.Ptyp_tuple many_scoped ;
		    Scopedtree.ptyp_loc = Location.none }) in
      (* Retrieve ident form exception name *)
      let exc_ident = Envscope.retrieve_scoped_value exc_name new_envsc in
      (Scopedtree.Pstr_exception (exc_ident, scoped_exc_declaration),
       new_envsc)
  | Parsetree.Pstr_module (mod_name, module_expr) ->
      (* Get the scoped module expression and the scoping envt corresponding *)
      (* to components found in this module expression (for later opening)   *)
      let (scoped_module_expr, rec_scenv) =
	scope_module_expression scenv module_expr in
      let new_scenv = Envscope.enter_module mod_name rec_scenv scenv in
      (* Retrieve module name transformed to ident *)
      let mod_ident = Envscope.retrieve_scoped_module mod_name new_scenv in
      (Scopedtree.Pstr_module (mod_ident, scoped_module_expr),
       new_scenv)
  | Parsetree.Pstr_modtype (_, _) ->
      (Scopedtree.Pstr_modtype, scenv)
  | Parsetree.Pstr_open lident ->
      let scoped_lident = Envscope.module_longident lident scenv in
      let scenv' = Envscope.handle_open_directive scenv scoped_lident in
      (Scopedtree.Pstr_open (scoped_lident),
       scenv')
  | Parsetree.Pstr_class _ -> failwith "OBJECTS NOT IMPLEMENTED"
  | Parsetree.Pstr_class_type _ -> failwith "OBJECTS NOT IMPLEMENTED"


(* val scope_type_declaration_components: Envscope.t ->                     *)
(*                            (string * Parsetree.type_declaration) list -> *)
(*                (Ident.t * Scopedtree.type_declaration) list * Envscope.t *)
(* Return the scoped type component and the scope environment           *)
(* <extended> with bindings for this component. Assume that the         *)
(* type name is already binded in the environment (for recursive types) *)
and scope_type_declaration_components scenv = function
  | [] -> ([], scenv)
  | (ty_name, decl) :: rem ->
      (* Retrieve ident from string *)
      let type_ident = Envscope.retrieve_scoped_type ty_name scenv in
      let (scoped_tydecl, new_scenv) =
                      Corescope.scope_type_declaration scenv type_ident decl in
      let (scoped_rem, new_scenv') =
	                    scope_type_declaration_components new_scenv rem in
      ((type_ident, scoped_tydecl) :: scoped_rem,
       new_scenv')
;;


Corescope.scope_module_expression_forward :=  scope_module_expression ;;
