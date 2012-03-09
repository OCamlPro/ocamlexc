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




open Format ;;



let rec pp_core_type ppf core_ty =
  pp_core_type_desc ppf core_ty.Parsetree.ptyp_desc



(* Used everywhere, let compute it once for all... *)
and iter_pp_core_type_comma core_tys = Printbasic.iter_pp ", "
                                           pp_core_type core_tys
and iter_pp_core_type_star core_tys = Printbasic.iter_pp " * "
                                           pp_core_type core_tys
and iter_pp_string strlst = Printbasic.iter_pp " "
                                    (fun ppf s -> fprintf ppf "%s" s) strlst


and pp_core_type_desc ppf = function
  | Parsetree.Ptyp_any -> fprintf ppf "any"
  | Parsetree.Ptyp_var var_name -> fprintf ppf "'%s" var_name
  | Parsetree.Ptyp_arrow (core_ty1, core_ty2) ->
      fprintf ppf "%a -> %a" pp_core_type core_ty1 pp_core_type core_ty2
  | Parsetree.Ptyp_tuple core_tys ->
      fprintf ppf "(%a)" iter_pp_core_type_star core_tys
  | Parsetree.Ptyp_constr (lident, core_tys) ->
      Printbasic.pp_longident ppf lident ;
      if core_tys <> [] then fprintf ppf " of (%a)"
                                  iter_pp_core_type_star core_tys ;
  | Parsetree.Ptyp_object _ -> failwith "OBJECTS NOT IMPLEMENTED"
  | Parsetree.Ptyp_class (_, _) -> failwith "OBJECTS NOT IMPLEMENTED"
  | Parsetree.Ptyp_alias (core_ty, alias_name) ->
      fprintf ppf "%a as '%s" pp_core_type core_ty alias_name



and pp_core_field_type ppf core_f_ty =
  pp_core_field_desc ppf core_f_ty.Parsetree.pfield_desc



and pp_core_field_desc ppf = function
  | Parsetree.Pfield (field_name, core_ty) ->
      fprintf ppf "%s : %a" field_name pp_core_type core_ty
  | Parsetree.Pfield_var ->
      fprintf ppf "Pfield_var"
;;



let rec pp_pattern ppf pat =
  pp_pattern_desc ppf pat.Parsetree.ppat_desc



(* Used everywhere, let compute it once for all... *)
and iter_pp_pattern pats = Printbasic.iter_pp ", " pp_pattern pats



and pp_pattern_desc ppf = function
  | Parsetree.Ppat_any -> fprintf ppf "_"
  | Parsetree.Ppat_var var_name -> fprintf ppf "%s" var_name
  | Parsetree.Ppat_alias (pat, alias_name) ->
      fprintf ppf "%a as %s" pp_pattern pat alias_name
  | Parsetree.Ppat_constant constant ->
      Printbasic.pp_constant ppf constant
  | Parsetree.Ppat_tuple pats ->
      let iter_pp_pattern = Printbasic.iter_pp "," pp_pattern in
      fprintf ppf "(%a)" iter_pp_pattern pats
  | Parsetree.Ppat_construct (lident, pat_opt, _) ->
      Printbasic.pp_longident ppf lident ;
      (match pat_opt with None -> ()
       | Some p -> fprintf ppf " (%a)" pp_pattern p)
  | Parsetree.Ppat_record rec_fields ->
      let iter_pp_rec_fields = Printbasic.iter_pp ";"
	                         (fun ppf (lident, pat) ->
				      fprintf ppf "%a : %a"
				                Printbasic.pp_longident lident
				                pp_pattern pat) in
      fprintf ppf "{%a}" iter_pp_rec_fields rec_fields
  | Parsetree.Ppat_array pats ->
      let iter_pp_pattern = Printbasic.iter_pp "; " pp_pattern in
      fprintf ppf "[|%a|]" iter_pp_pattern pats
  | Parsetree.Ppat_or (pat0, pat1) ->
      fprintf ppf "%a | %a" pp_pattern pat0 pp_pattern pat1
  | Parsetree.Ppat_constraint (pat, core_ty) ->
      fprintf ppf "%a : %a" pp_pattern pat pp_core_type core_ty
;;



let rec pp_expression ppf expr =
  pp_expression_desc ppf expr.Parsetree.pexp_desc



and iter_pp_expression exprs = Printbasic.iter_pp " " pp_expression exprs



(* Print 'let-bindings' *)
and pp_let_bindings bindings =
  Printbasic.iter_pp " and "
      (fun ppf (pat, expr) ->
	  fprintf ppf "%a = %a" pp_pattern pat pp_expression expr)
      bindings



(* Print 'fun-bindings' *)
and pp_fun_bindings bindings =
  Printbasic.iter_pp "| "
      (fun ppf (pat, expr) ->
	  fprintf ppf "%a -> %a" pp_pattern pat pp_expression expr)
      bindings



and pp_expression_desc ppf = function
  | Parsetree.Pexp_ident lident ->
      Printbasic.pp_longident ppf lident
  | Parsetree.Pexp_constant constant ->
      Printbasic.pp_constant ppf constant
  | Parsetree.Pexp_let (rec_flag, bindings, expr) ->
      fprintf ppf "let %a%a in %a" Printbasic.pp_rec_flag rec_flag
                                    pp_let_bindings bindings
                                    pp_expression expr
  | Parsetree.Pexp_function bindings ->
      fprintf ppf "function %a" pp_fun_bindings bindings
  | Parsetree.Pexp_apply (expr0, exprs) ->
      let iter_pp_expression = Printbasic.iter_pp " " pp_expression in
      fprintf ppf "%a %a" pp_expression expr0 iter_pp_expression exprs
  | Parsetree.Pexp_match (expr, bindings) ->
      fprintf ppf "match %a with %a" pp_expression expr
                                     pp_fun_bindings bindings
  | Parsetree.Pexp_try (expr, bindings) ->
      fprintf ppf "try %a with %a" pp_expression expr
                                     pp_fun_bindings bindings
  | Parsetree.Pexp_tuple exprs ->
      let iter_pp_expression = Printbasic.iter_pp ", " pp_expression in
      fprintf ppf "(%a)" iter_pp_expression exprs
  | Parsetree.Pexp_construct (lident, expr_opt, _) ->
      Printbasic.pp_longident ppf lident ;
      (match expr_opt with None -> ()
       | Some e -> fprintf ppf " (%a)" pp_expression e)
  | Parsetree.Pexp_record (rec_fields, expr_opt) ->
      (match expr_opt with None -> ()
       | Some e -> fprintf ppf "(%a) with " pp_expression e) ;
      let iter_pp_rec_fields = Printbasic.iter_pp ";"
	                         (fun ppf (lident, expr) ->
				      fprintf ppf "%a : %a"
				                Printbasic.pp_longident lident
				                pp_expression expr) in
      fprintf ppf "{%a}" iter_pp_rec_fields rec_fields
  | Parsetree.Pexp_field (expr, lident) ->
      fprintf ppf "(%a).%a" pp_expression expr Printbasic.pp_longident lident
  | Parsetree.Pexp_setfield (expr0, lident, expr1) ->
      fprintf ppf "(%a).%a <- %a"
               pp_expression expr0 Printbasic.pp_longident lident
               pp_expression expr1
  | Parsetree.Pexp_array exprs ->
      let iter_pp_expression = Printbasic.iter_pp "; " pp_expression in
      fprintf ppf "[|%a|]" iter_pp_expression exprs
  | Parsetree.Pexp_ifthenelse (expr0, expr1, expr2_opt) ->
      fprintf ppf "if %a then %a" pp_expression expr0 pp_expression expr1 ;
      (match expr2_opt with None -> ()
       | Some e -> fprintf ppf " else %a" pp_expression e)
  | Parsetree.Pexp_sequence (expr0, expr1) ->
      fprintf ppf "%a ; %a" pp_expression expr0 pp_expression expr1
  | Parsetree.Pexp_while  (expr0, expr1) ->
      fprintf ppf "while %a do %a done"
                  pp_expression expr0 pp_expression expr1
  | Parsetree.Pexp_for (index_name, expr0, expr1, dir_flag, expr2) ->
      fprintf ppf "for %s = %a %a %a do %a done"
                              index_name
                              pp_expression expr0
                              Printbasic.pp_direction_flag dir_flag
                              pp_expression expr1
                              pp_expression expr2
  | Parsetree.Pexp_constraint (expr, core_ty_opt1, core_ty_opt2) ->
      fprintf ppf "%a" pp_expression expr ;
      (match core_ty_opt1 with None -> ()
       | Some ct -> fprintf ppf " : %a" pp_core_type ct) ;
      (match core_ty_opt2 with None -> ()
       | Some ct -> fprintf ppf " : %a" pp_core_type ct)
  | Parsetree.Pexp_when (expr0, expr1) ->
      fprintf ppf "%a when %a" pp_expression expr0 pp_expression expr1
  | Parsetree.Pexp_send (_, _) -> failwith "OBJECTS NOT IMPLEMENTED"
  | Parsetree.Pexp_new _ -> failwith "OBJECTS NOT IMPLEMENTED"
  | Parsetree.Pexp_setinstvar (_, _) -> failwith "OBJECTS NOT IMPLEMENTED"
  | Parsetree.Pexp_override _ -> failwith "OBJECTS NOT IMPLEMENTED"
  | Parsetree.Pexp_letmodule (id, mod_expr, expr) ->
      fprintf ppf "let module %s = %a in %a" id
	pp_module_expr mod_expr
	pp_expression expr


and pp_value_description ppf valdesc =
  let iter_pp_string = Printbasic.iter_pp " "
                           (fun ppf s -> fprintf ppf "%s" s) in
  fprintf ppf " : %a %a" pp_core_type valdesc.Parsetree.pval_type
                         iter_pp_string valdesc.Parsetree.pval_prim



and pp_type_declaration ppf ty_decl =
  let iter_pp_string = Printbasic.iter_pp " "
                           (fun ppf s -> fprintf ppf "%s" s) in
  if ty_decl.Parsetree.ptype_params <> [] then
    fprintf ppf "(%a) " iter_pp_string ty_decl.Parsetree.ptype_params ;
  pp_type_kind ppf ty_decl.Parsetree.ptype_kind ;
  (match ty_decl.Parsetree.ptype_manifest with None -> ()
   | Some core_ty -> fprintf ppf " = %a" pp_core_type core_ty)



and pp_type_kind ppf = function
  | Parsetree.Ptype_abstract -> ()
  | Parsetree.Ptype_variant constructors ->
      (* Printer for 1 constructor definition *)
      let pp_constructor ppf =
         fun (cstr_name, args_core_tys) ->
	   fprintf ppf "%s" cstr_name ;
	   (* Print "of (...)" only is there some arguments *)
	   if args_core_tys <> []
	    then fprintf ppf " of (%a)"
                           iter_pp_core_type_star args_core_tys in
      (* Iterate on constructors definitions list *)
      let iter_pp_constructor = Printbasic.iter_pp " | " pp_constructor in
      (* Let's print now... *)
      fprintf ppf " = %a" iter_pp_constructor constructors
  | Parsetree.Ptype_record fields_types ->
      let pp_field = fun ppf (f_name, mut_flag, core_ty) ->
	                fprintf ppf "%a%s : %a"
                                          Printbasic.pp_mutable_flag mut_flag
                                          f_name pp_core_type core_ty in
      let iter_pp_field = Printbasic.iter_pp ";" pp_field in
      fprintf ppf "{%a}" iter_pp_field fields_types



and pp_exception_declaration ppf core_tys =
 if core_tys <> [] then fprintf ppf " of %a"
                                    iter_pp_core_type_star core_tys


and pp_module_type ppf mod_ty =
 pp_module_type_desc ppf mod_ty.Parsetree.pmty_desc



and pp_module_type_desc ppf = function
  | Parsetree.Pmty_ident mod_longident ->
      fprintf ppf "%a" Printbasic.pp_longident mod_longident
  | Parsetree.Pmty_signature signature ->
      fprintf ppf "(sig %a end)" pp_signature signature
  | Parsetree.Pmty_functor (arg_name, mod_ty0, mod_ty1) ->
      fprintf ppf "(functor (%s : %a) -> %a)" arg_name
                                            pp_module_type mod_ty0
                                            pp_module_type mod_ty1
  | Parsetree.Pmty_with (_, _) -> ()



and pp_signature ppf signature =
 let iter_pp_signature_item = Printbasic.iter_pp "\n" pp_signature_item in
 iter_pp_signature_item ppf signature



and pp_signature_item ppf sig_item =
 pp_signature_item_desc ppf sig_item.Parsetree.psig_desc



and pp_signature_item_desc ppf = function
  | Parsetree.Psig_value (val_name, val_descr) ->
      fprintf ppf "value %s : %a" val_name pp_value_description val_descr
  | Parsetree.Psig_type ty_names_ty_decls ->
      let iter_pp_ty_names_ty_decls = Printbasic.iter_pp " and "
        (fun ppf (ty_name, ty_decl) -> fprintf ppf "%s%a" ty_name
	                                        pp_type_declaration ty_decl) in
      fprintf ppf "type %a" iter_pp_ty_names_ty_decls ty_names_ty_decls
  | Parsetree.Psig_exception (exc_name, exc_decl) ->
      fprintf ppf "exception %s%a" exc_name pp_exception_declaration exc_decl
  | Parsetree.Psig_module (mod_name, mod_ty) ->
      fprintf ppf "module %s : %a" mod_name pp_module_type mod_ty
  | Parsetree.Psig_modtype (mod_tyname, mod_ty_decl) ->
      fprintf ppf "module type %s : %a" mod_tyname
                                        pp_modtype_declaration mod_ty_decl
  | Parsetree.Psig_open longident ->
      fprintf ppf "open %a" Printbasic.pp_longident longident
  | Parsetree.Psig_include _ -> ()
  | Parsetree.Psig_class _ -> failwith "OBJECTS NOT IMPLEMENTED"
  | Parsetree.Psig_class_type _ -> failwith "OBJECTS NOT IMPLEMENTED"


and pp_modtype_declaration ppf = function
  | Parsetree.Pmodtype_abstract -> ()
  | Parsetree.Pmodtype_manifest mod_ty ->
      pp_module_type ppf mod_ty


and pp_module_expr ppf mod_expr =
  pp_module_expr_desc ppf mod_expr.Parsetree.pmod_desc



and pp_module_expr_desc ppf = function
  | Parsetree.Pmod_ident longident ->
      fprintf ppf "%a" Printbasic.pp_longident longident
  | Parsetree.Pmod_structure structure ->
      fprintf ppf "(struct %a end)" pp_structure structure
  | Parsetree.Pmod_functor (arg_name, mod_ty, mod_expr_body) ->
      fprintf ppf "(functor (%s : %a) -> %a)" arg_name
                                        pp_module_type mod_ty
                                        pp_module_expr mod_expr_body
  | Parsetree.Pmod_apply (mod_expr0, mod_expr1) ->
      fprintf ppf "(%a %a)" pp_module_expr mod_expr0 pp_module_expr mod_expr1
  | Parsetree.Pmod_constraint (mod_expr, mod_ty) ->
      fprintf ppf "(%a : sig %a end)"
                     pp_module_expr mod_expr pp_module_type mod_ty



and pp_structure ppf structure =
 let iter_pp_structure_item = Printbasic.iter_pp "\n" pp_structure_item in
 iter_pp_structure_item ppf structure



and pp_structure_item ppf struct_item =
  pp_structure_item_desc ppf struct_item.Parsetree.pstr_desc



and pp_structure_item_desc ppf = function
  | Parsetree.Pstr_eval expr ->
      pp_expression ppf expr
  | Parsetree.Pstr_value (rec_flag, bindings) ->
      fprintf ppf "let %a%a" Printbasic.pp_rec_flag rec_flag
                             pp_let_bindings bindings
  | Parsetree.Pstr_primitive (prim_name, _) ->
      fprintf ppf "external %s : of no interest" prim_name
  | Parsetree.Pstr_type ty_names_ty_decls ->
      let iter_pp_ty_names_ty_decls = Printbasic.iter_pp " and "
        (fun ppf (ty_name, ty_decl) -> fprintf ppf "%s%a" ty_name
	                                       pp_type_declaration ty_decl) in
      fprintf ppf "type %a" iter_pp_ty_names_ty_decls ty_names_ty_decls
  | Parsetree.Pstr_exception (exc_name, exc_decl) ->
      fprintf ppf "exception %s%a" exc_name pp_exception_declaration exc_decl
  | Parsetree.Pstr_module (mod_name, mod_expr) ->
      fprintf ppf "module %s = %a" mod_name pp_module_expr mod_expr
  | Parsetree.Pstr_modtype (mod_tyname, mod_ty_decl) ->
      fprintf ppf "module type %s : %a" mod_tyname
                                        pp_module_type mod_ty_decl
  | Parsetree.Pstr_open longident ->
      fprintf ppf "open %a" Printbasic.pp_longident longident
  | Parsetree.Pstr_class _ -> failwith "OBJECTS NOT IMPLEMENTED"
  | Parsetree.Pstr_class_type _ -> failwith "OBJECTS NOT IMPLEMENTED"
;;



let pp_directive_argument ppf = function
  | Parsetree.Pdir_none -> ()
  | Parsetree.Pdir_string str -> fprintf ppf "%s" str
  | Parsetree.Pdir_int i -> fprintf ppf "%d" i
  | Parsetree.Pdir_ident longident ->
      fprintf ppf "%a" Printbasic.pp_longident longident
;;



let pp_toplevel_phrase ppf = function
  | Parsetree.Ptop_def structure ->
      pp_structure ppf structure
  | Parsetree.Ptop_dir (dir_name, dir_arg) ->
      fprintf ppf "#%s %a" dir_name pp_directive_argument dir_arg
;;
