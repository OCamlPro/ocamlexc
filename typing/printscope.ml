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



(* pp_core_type : Format.formatter -> Scopedtree.core_type -> unit *)
let rec pp_core_type prio ppf core_ty =
  pp_core_type_desc prio ppf core_ty.Scopedtree.ptyp_desc



(* Used everywhere, let compute it once for all... *)

(* iter_pp_core_type_comma: Format.formatter ->                    *)
(*                               Scopedtree.core_type list -> unit *)
and iter_pp_core_type_comma core_tys = Printbasic.iter_pp ","
                                           (pp_core_type 0) core_tys

(* iter_pp_core_type_star: Format.formatter ->                     *)
(*                               Scopedtree.core_type list -> unit *)
and iter_pp_core_type_star core_tys = Printbasic.iter_pp " *"
                                           (pp_core_type 0) core_tys

(* iter_pp_string: Format.formatter -> string list -> unit *)
and iter_pp_string strlst = Printbasic.iter_pp ""
                                    (fun ppf s -> fprintf ppf "%s" s) strlst



(* pp_core_type_desc: Format.formatter -> Scopedtree.core_type_desc -> unit *)
and pp_core_type_desc prio ppf = function
  | Scopedtree.Ptyp_any -> (* I don't known what's this type *) assert false
  | Scopedtree.Ptyp_var var_name -> fprintf ppf "'%s" var_name
  | Scopedtree.Ptyp_arrow (core_ty1, core_ty2) ->
      if prio >= 2 then fprintf ppf "@[<1>(" else fprintf ppf "@[" ;
      fprintf ppf "%a@ ->@ %a"
              (pp_core_type 2) core_ty1
              (pp_core_type 1) core_ty2 ;
	if prio >= 2 then fprintf ppf ")@]" else fprintf ppf "@]"
  | Scopedtree.Ptyp_tuple core_tys ->
      let iter_pp_core_type_star =
	Printbasic.iter_pp " *" (pp_core_type 3) in
      if prio >= 3 then fprintf ppf "@[<1>(%a)@]"
	                        iter_pp_core_type_star core_tys
      else fprintf ppf "@[%a@]" iter_pp_core_type_star core_tys
  | Scopedtree.Ptyp_constr (path, core_tys) ->
      let iter_pp_core_type_star core_tys =
	Printbasic.iter_pp "," (pp_core_type 0) core_tys in
      (match core_tys with
        | [] -> ()
        | [_] -> fprintf ppf "%a@ " iter_pp_core_type_star core_tys
        | _ -> fprintf ppf "(%a)@ " iter_pp_core_type_star core_tys) ;
      Path.pp_path ppf !path
  | Scopedtree.Ptyp_alias (core_ty, alias_name) ->
      fprintf ppf "%a as '%s" (pp_core_type prio) core_ty alias_name



(* pp_core_field_type: Format.formatter -> Scopedtree.core_field_type -> unit*)
and pp_core_field_type ppf core_f_ty =
  pp_core_field_desc ppf core_f_ty.Scopedtree.pfield_desc



(* pp_core_field_desc: Format.formatter -> Scopedtree.core_field_desc -> unit*)
and pp_core_field_desc ppf = function
  | Scopedtree.Pfield (field_name, core_ty) ->
      fprintf ppf "%s :@ %a" field_name (pp_core_type 0) core_ty
  | Scopedtree.Pfield_var ->
      fprintf ppf "Pfield_var"
;;



(* pp_pattern: Format.formatter -> Scopedtree.pattern -> unit *)
let rec pp_pattern ppf pat =
  pp_pattern_desc ppf pat.Scopedtree.ppat_desc



(* Used everywhere, let compute it once for all... *)
(* iter_pp_pattern: Format.formatter -> Scopedtree.pattern list -> unit *)
and iter_pp_pattern pats = Printbasic.iter_pp "," pp_pattern pats



(* pp_pattern_desc: Format.formatter -> Scopedtree.pattern_desc -> unit *)
and pp_pattern_desc ppf = function
  | Scopedtree.Ppat_any -> fprintf ppf "_"
  | Scopedtree.Ppat_var var_ident -> fprintf ppf "%s" (Ident.name var_ident)
  | Scopedtree.Ppat_alias (pat, alias_name) ->
      fprintf ppf "%a as %s" pp_pattern pat (Ident.name alias_name)
  | Scopedtree.Ppat_constant constant ->
      Printbasic.pp_constant ppf constant
  | Scopedtree.Ppat_tuple pats ->
      let iter_pp_pattern = Printbasic.iter_pp "," pp_pattern in
      fprintf ppf "(%a)" iter_pp_pattern pats
  | Scopedtree.Ppat_construct (path, pat_opt) ->
      Path.pp_path ppf path ;
      (match pat_opt with None -> ()
       | Some p -> fprintf ppf " (%a)" pp_pattern p)
  | Scopedtree.Ppat_record rec_fields ->
      let iter_pp_rec_fields = Printbasic.iter_pp ";"
	                         (fun ppf (path, pat) ->
				      fprintf ppf "%a : %a"
				                Path.pp_path path
				                pp_pattern pat) in
      fprintf ppf "{%a}" iter_pp_rec_fields rec_fields
  | Scopedtree.Ppat_array pats ->
      let iter_pp_pattern = Printbasic.iter_pp ";" pp_pattern in
      fprintf ppf "[|%a|]" iter_pp_pattern pats
  | Scopedtree.Ppat_or (pat0, pat1) ->
      fprintf ppf "%a | %a" pp_pattern pat0 pp_pattern pat1
  | Scopedtree.Ppat_constraint (pat, core_ty) ->
      fprintf ppf "%a : %a" pp_pattern pat (pp_core_type 0) core_ty
;;



(* pp_expression: Format.formatter -> Scopedtree.expression -> unit *)
let rec pp_expression ppf expr =
  pp_expression_desc ppf expr.Scopedtree.pexp_desc



(* iter_pp_expression: Format.formatter -> Scopedtree.expression list -> unit*)
and iter_pp_expression exprs = Printbasic.iter_pp "" pp_expression exprs



(* pp_let_bindings: Format.formatter ->                                     *)
(*                (Scopedtree.pattern * Scopedtree.expression) list -> unit *)
(* Print 'let-bindings' *)
and pp_let_bindings bindings =
  Printbasic.iter_pp "and"
      (fun ppf (pat, expr) ->
	  fprintf ppf "%a = %a" pp_pattern pat pp_expression expr)
      bindings



(* pp_fun_bindings: Format.formatter ->                                  *)
(*             (Scopedtree.pattern * Scopedtree.expression) list -> unit *)
(* Print 'fun-bindings' *)
and pp_fun_bindings bindings =
  Printbasic.iter_pp "|"
      (fun ppf (pat, expr) ->
	  fprintf ppf "%a -> %a" pp_pattern pat pp_expression expr)
      bindings



(* pp_expression_desc: Format.formatter -> Scopedtree.expression_desc -> unit*)
and pp_expression_desc ppf = function
  | Scopedtree.Pexp_ident path ->
      Path.pp_path ppf path
  | Scopedtree.Pexp_constant constant ->
      Printbasic.pp_constant ppf constant
  | Scopedtree.Pexp_let (rec_flag, bindings, expr) ->
      fprintf ppf "let %a%a in %a" Printbasic.pp_rec_flag rec_flag
                                    pp_let_bindings bindings
                                    pp_expression expr
  | Scopedtree.Pexp_function bindings ->
      fprintf ppf "function %a" pp_fun_bindings bindings
  | Scopedtree.Pexp_apply (expr0, exprs) ->
      let iter_pp_expression = Printbasic.iter_pp "" pp_expression in
      fprintf ppf "%a %a" pp_expression expr0 iter_pp_expression exprs
  | Scopedtree.Pexp_match (expr, bindings) ->
      fprintf ppf "match %a with %a" pp_expression expr
                                     pp_fun_bindings bindings
  | Scopedtree.Pexp_try (expr, bindings) ->
      fprintf ppf "try %a with %a" pp_expression expr
                                     pp_fun_bindings bindings
  | Scopedtree.Pexp_tuple exprs ->
      let iter_pp_expression = Printbasic.iter_pp "," pp_expression in
      fprintf ppf "(%a)" iter_pp_expression exprs
  | Scopedtree.Pexp_construct (path, expr_opt) ->
      Path.pp_path ppf path ;
      (match expr_opt with None -> ()
       | Some e -> fprintf ppf " (%a)" pp_expression e)
  | Scopedtree.Pexp_record (rec_fields, expr_opt) ->
      (match expr_opt with None -> ()
       | Some e -> fprintf ppf "(%a) with " pp_expression e) ;
      let iter_pp_rec_fields = Printbasic.iter_pp ";"
	                         (fun ppf (path, expr) ->
				      fprintf ppf "%a : %a"
				                Path.pp_path path
				                pp_expression expr) in
      fprintf ppf "{%a}" iter_pp_rec_fields rec_fields
  | Scopedtree.Pexp_field (expr, path) ->
      fprintf ppf "(%a).%a" pp_expression expr Path.pp_path path
  | Scopedtree.Pexp_setfield (expr0, path, expr1) ->
      fprintf ppf "(%a).%a <- %a"
               pp_expression expr0 Path.pp_path path
               pp_expression expr1
  | Scopedtree.Pexp_array exprs ->
      let iter_pp_expression = Printbasic.iter_pp ";" pp_expression in
      fprintf ppf "[|%a|]" iter_pp_expression exprs
  | Scopedtree.Pexp_ifthenelse (expr0, expr1, expr2_opt) ->
      fprintf ppf "if %a then %a" pp_expression expr0 pp_expression expr1 ;
      (match expr2_opt with None -> ()
       | Some e -> fprintf ppf " else %a" pp_expression e)
  | Scopedtree.Pexp_sequence (expr0, expr1) ->
      fprintf ppf "%a ; %a" pp_expression expr0 pp_expression expr1
  | Scopedtree.Pexp_while  (expr0, expr1) ->
      fprintf ppf "while %a do %a done"
                  pp_expression expr0 pp_expression expr1
  | Scopedtree.Pexp_for (index_name, expr0, expr1, dir_flag, expr2) ->
      fprintf ppf "for %s = %a %a %a do %a done"
                              (Ident.name index_name)
                              pp_expression expr0
                              Printbasic.pp_direction_flag dir_flag
                              pp_expression expr1
                              pp_expression expr2
  | Scopedtree.Pexp_constraint (expr, core_ty_opt1, core_ty_opt2) ->
      fprintf ppf "%a" pp_expression expr ;
      (match core_ty_opt1 with None -> ()
       | Some ct -> fprintf ppf " : %a" (pp_core_type 0) ct) ;
      (match core_ty_opt2 with None -> ()
       | Some ct -> fprintf ppf " : %a" (pp_core_type 0) ct)
  | Scopedtree.Pexp_when (expr0, expr1) ->
      fprintf ppf "%a when %a" pp_expression expr0 pp_expression expr1
  | Scopedtree.Pexp_letmodule (id, mod_expr, expr) ->
      fprintf ppf "let module %s = %a in %a" (Ident.name id)
	pp_module_expr mod_expr
	pp_expression expr



(* pp_value_description: Format.formatter -> Scopedtree.value_description -> *)
(*                                           unit                            *)
and pp_value_description ppf valdesc =
  let iter_pp_string = Printbasic.iter_pp ""
                           (fun ppf s -> fprintf ppf "%s" s) in
  fprintf ppf " : %a %a" (pp_core_type 0) valdesc.Scopedtree.pval_type
                         iter_pp_string valdesc.Scopedtree.pval_prim



(* pp_type_declaration: Format.formatter ->                                *)
(*                           Ident.t * Scopedtree.type_declaration -> unit *)
and pp_type_declaration ppf (ty_ident, ty_decl) =
  let iter_pp_string = Printbasic.iter_pp ","
                           (fun ppf s -> fprintf ppf "'%s" s) in
  (* If any params, print them *)
  if ty_decl.Scopedtree.ptype_params <> [] then
    fprintf ppf "(@[%a)@]@ " iter_pp_string ty_decl.Scopedtree.ptype_params ;
  if ty_decl.Scopedtree.ptype_cstrs <> [] then failwith "todo136" ;
  (* Print type name *)
  fprintf ppf "%s" (Ident.name ty_ident) ;
  (* Print the kind *)
  pp_type_kind ppf ty_decl.Scopedtree.ptype_kind ;
  (* check if it's an abbrev *)
  (match ty_decl.Scopedtree.ptype_manifest with None -> ()
   | Some core_ty -> fprintf ppf " =@ %a" (pp_core_type 0) core_ty)



(* pp_type_kind : Format.formatter -> Scopedtree.type_kind -> unit *)
and pp_type_kind ppf = function
  | Scopedtree.Ptype_abstract -> ()
  | Scopedtree.Ptype_variant constructors ->
      (* Printer for 1 constructor definition *)
      let pp_constructor ppf =
         fun (cstr_ident, args_core_tys) ->
	   fprintf ppf "%s" (Ident.name cstr_ident) ;
	   (* Print "of (...)" only is there some arguments *)
	   if args_core_tys <> []
	    then fprintf ppf " of@[ %a@]"
                           iter_pp_core_type_star args_core_tys in
      (* Iterate on constructors definitions list *)
      let iter_pp_constructor = Printbasic.iter_pp " |" pp_constructor in
      (* Let's print now... *)
      fprintf ppf " @[=@ %a@]" iter_pp_constructor constructors
  | Scopedtree.Ptype_record fields_types ->
      let pp_field = fun ppf (f_name, mut_flag, core_ty) ->
	                fprintf ppf "%a%s :@ %a"
                                          Printbasic.pp_mutable_flag mut_flag
                                          (Ident.name f_name)
	                                  (pp_core_type 0) core_ty in
      let iter_pp_field = Printbasic.iter_pp ";" pp_field in
      fprintf ppf " = {@ %a@ }" iter_pp_field fields_types



(* pp_exception_declaration: Format.formatter ->                  *)
(*                       Scopedtree.exception_declaration -> unit *)
and pp_exception_declaration ppf = function
  | None -> ()
  | Some core_ty -> fprintf ppf " of@[ %a@]" (pp_core_type 0) core_ty


(* pp_module_type: Format.formatter -> Scopedtree.module_type -> unit *)
and pp_module_type ppf mod_ty =
 pp_module_type_desc ppf mod_ty.Scopedtree.pmty_desc



(* pp_module_type_desc: Format.formatter -> Scopedtree.module_type_desc -> *)
(*                                          unit                           *)
and pp_module_type_desc ppf = function
  | Scopedtree.Pmty_ident mod_path ->
      fprintf ppf "%a" Path.pp_path mod_path
  | Scopedtree.Pmty_signature signature ->
      fprintf ppf "sig %a end" pp_signature signature
  | Scopedtree.Pmty_functor (arg_name, mod_ty0, mod_ty1) ->
      fprintf ppf "functor (%s : %a) -> %a" arg_name
                                            pp_module_type mod_ty0
                                            pp_module_type mod_ty1



(* pp_signature: Format.formatter -> Scopedtree.signature -> unit *)
and pp_signature ppf signature =
 let iter_pp_signature_item = Printbasic.iter_pp "\n" pp_signature_item in
 iter_pp_signature_item ppf signature



(* pp_signature_item: Format.formatter -> Scopedtree.signature_item -> unit *)
and pp_signature_item ppf sig_item =
 pp_signature_item_desc ppf sig_item.Scopedtree.psig_desc



(* pp_signature_item_desc: Format.formatter ->                           *)
(*                                Scopedtree.signature_item_desc -> unit *)
and pp_signature_item_desc ppf = function
  | Scopedtree.Psig_value (val_name, val_descr) ->
      fprintf ppf "val %s : %a" val_name pp_value_description val_descr
  | Scopedtree.Psig_type ty_names_ty_decls ->
      let iter_pp_ty_names_ty_decls = Printbasic.iter_pp " and"
        (fun ppf (ty_name, ty_decl) -> fprintf ppf "type %a"
                                     pp_type_declaration (ty_name, ty_decl)) in
      fprintf ppf "type %a" iter_pp_ty_names_ty_decls ty_names_ty_decls
  | Scopedtree.Psig_exception (_, exc_decl) ->
      fprintf ppf "exception %a" pp_exception_declaration exc_decl
  | Scopedtree.Psig_module (mod_name, mod_ty) ->
      fprintf ppf "module %s : %a" mod_name pp_module_type mod_ty
  | Scopedtree.Psig_modtype (mod_tyname, mod_ty_decl) ->
      fprintf ppf "module type %s : %a" mod_tyname
                                        pp_modtype_declaration mod_ty_decl
  | Scopedtree.Psig_open path ->
      fprintf ppf "open %a" Path.pp_path path
  | Scopedtree.Psig_include _ -> ()



(* pp_modtype_declaration: Format.formatter ->                         *)
(*                             Scopedtree.modtype_declaration -> unit  *)
and pp_modtype_declaration ppf = function
  | Scopedtree.Pmodtype_abstract -> ()
  | Scopedtree.Pmodtype_manifest mod_ty ->
      pp_module_type ppf mod_ty


(* pp_module_expr: Format.formatter -> Scopedtree.module_expr -> unit *)
and pp_module_expr ppf mod_expr =
  pp_module_expr_desc ppf mod_expr.Scopedtree.pmod_desc



(* pp_module_expr_desc: Format.formatter -> Scopedtree.module_expr_desc -> *)
(*                                          unit                           *)
and pp_module_expr_desc ppf = function
  | Scopedtree.Pmod_ident path ->
      fprintf ppf "%a" Path.pp_path path
  | Scopedtree.Pmod_structure structure ->
      fprintf ppf "struct %a end" pp_structure structure
  | Scopedtree.Pmod_functor (arg_name, mod_expr_body) ->
      fprintf ppf "functor %s -> %a" (Ident.name arg_name)
                                     pp_module_expr mod_expr_body
  | Scopedtree.Pmod_apply (mod_expr0, mod_expr1) ->
      fprintf ppf "(%a %a)" pp_module_expr mod_expr0 pp_module_expr mod_expr1



(* pp_structure: Format.formatter -> Scopedtree.structure -> unit *)
and pp_structure ppf structure =
 let iter_pp_structure_item = Printbasic.iter_pp "\n" pp_structure_item in
 iter_pp_structure_item ppf structure



(* pp_structure_item: Format.formatter -> Scopedtree.structure_item -> unit *)
and pp_structure_item ppf struct_item =
  pp_structure_item_desc ppf struct_item.Scopedtree.pstr_desc



(* pp_structure_item_desc: Format.formatter ->                             *)
(*                                  Scopedtree.structure_item_desc -> unit *)
and pp_structure_item_desc ppf = function
  | Scopedtree.Pstr_eval expr ->
      pp_expression ppf expr
  | Scopedtree.Pstr_value (rec_flag, bindings) ->
      fprintf ppf "let %a%a" Printbasic.pp_rec_flag rec_flag
                             pp_let_bindings bindings
  | Scopedtree.Pstr_primitive (prim_name, _) ->
      fprintf ppf "external %s : of no interest" (Ident.name prim_name)
  | Scopedtree.Pstr_type ty_names_ty_decls ->
      let iter_pp_ty_names_ty_decls = Printbasic.iter_pp " and"
        (fun ppf (ty_name, ty_decl) -> fprintf ppf "%a"
                              pp_type_declaration (ty_name, ty_decl)) in
      fprintf ppf "type %a" iter_pp_ty_names_ty_decls ty_names_ty_decls
  | Scopedtree.Pstr_exception (_, exc_decl) ->
      fprintf ppf "exception %a" pp_exception_declaration exc_decl
  | Scopedtree.Pstr_module (mod_name, mod_expr) ->
      fprintf ppf "module %s = %a" (Ident.name mod_name)
                                   pp_module_expr mod_expr
  | Scopedtree.Pstr_modtype ->
      fprintf ppf "module type ..."
  | Scopedtree.Pstr_open path ->
      fprintf ppf "open %a" Path.pp_path path
;;



(* pp_directive_argument: Format.formatter ->                            *)
(*                                 Scopedtree.directive_argument -> unit *)
let pp_directive_argument ppf = function
  | Scopedtree.Pdir_none -> ()
  | Scopedtree.Pdir_string str -> fprintf ppf "%s" str
  | Scopedtree.Pdir_int i -> fprintf ppf "%d" i
  | Scopedtree.Pdir_ident path -> fprintf ppf "%a" Path.pp_path path
;;


(* pp_toplevel_phrase: Format.formatter -> Scopedtree.toplevel_phrase -> unit*)
let pp_toplevel_phrase ppf = function
  | Scopedtree.Ptop_def structure ->
      pp_structure ppf structure
  | Scopedtree.Ptop_dir (dir_name, dir_arg) ->
      fprintf ppf "#%s %a" dir_name pp_directive_argument dir_arg
;;
