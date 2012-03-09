(***********************************************************************)
(*                                                                     *)
(*                           Ocamlexc                                  *)
(*                                                                     *)
(*        Francois Pessaux, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(* Pretty print of module types.                                       *)
(***********************************************************************)




open Format ;;



let rec pp_pattern ppf pat = pp_pattern_desc ppf pat.Typedtree.pat_desc

and pp_pattern_desc ppf = function
  | Typedtree.Tpat_any -> fprintf ppf "_"
  | Typedtree.Tpat_var id -> fprintf ppf "%s" (Ident.name id)
  | Typedtree.Tpat_alias (pat, id) -> fprintf ppf "%s as %a" (Ident.name id)
                                              pp_pattern pat
  | Typedtree.Tpat_constant c -> fprintf ppf "%a" Printbasic.pp_constant c
  | Typedtree.Tpat_tuple pats ->
      let iter_pp_pattern = Printbasic.iter_pp "," pp_pattern in
      fprintf ppf "(%a)" iter_pp_pattern pats
  | Typedtree.Tpat_construct (path, pat_opt) ->
      fprintf ppf "%a" Path.pp_path path ;
      (match pat_opt with None -> ()
       | Some p -> fprintf ppf " (%a)" pp_pattern p)
  | Typedtree.Tpat_record rec_fields ->
      let iter_pp_rec_fields = Printbasic.iter_pp ";"
	                         (fun ppf (path, pat) -> fprintf ppf "%a : %a"
				                            Path.pp_path path
				                            pp_pattern pat) in
      fprintf ppf "{%a}" iter_pp_rec_fields rec_fields
  | Typedtree.Tpat_array pats ->
      let iter_pp_pattern = Printbasic.iter_pp ";" pp_pattern in
      fprintf ppf "[|%a|]" iter_pp_pattern pats
  | Typedtree.Tpat_or (pat0, pat1) ->
      fprintf ppf "%a | %a" pp_pattern pat0 pp_pattern pat1
  | Typedtree.Tpat_constraint (pat, _) -> fprintf ppf "%a" pp_pattern pat
;;



(* pp_label_description: Format.formatter -> Typedtree.label_description -> *)
(*                       unit                                               *)
let pp_label_description ppf lbl_desc =
  fprintf ppf "%a:@ %a" Printbasic.pp_mutable_flag lbl_desc.Typedtree.fld_mut
                    Printtypes.pp_ml_type_scheme lbl_desc.Typedtree.fld_scheme
;;



(* pp_signature: Format.formatter -> Typedtree.signature_item list -> unit *)
(* Pretty print signatures *)
let rec pp_signature ppf signature = 
  let iter_pp_signature_item = Printbasic.iter_pp "" pp_signature_item in
  fprintf ppf "%a" iter_pp_signature_item signature



(* pp_signature_item: Format.formatter -> Typedtree.signature_item -> unit *)
(* Pretty print signature items *)
and pp_signature_item ppf = function
  | Typedtree.Tsig_value (ident, scheme) ->
      fprintf ppf "val@[ %s :@ %a@]" (Ident.name ident)
                        Printtypes.pp_ml_type_scheme scheme ;
  | Typedtree.Tsig_type (ident, ty_declaration) ->
      fprintf ppf "typ@[e %a@]"
                  Printtypes.pp_type_declaration (ident, ty_declaration)
  | Typedtree.Tsig_module (ident, mod_ty) ->
      fprintf ppf "mod@[ule %s :@ %a@]"
              (Ident.name ident) pp_module_type mod_ty
  | Typedtree.Tsig_constructor (id, cd) ->
      (* As specified in typedtree.mli, this the constructor comes *)
      (* from a type declaration, this kind of component is hidden *)
      (* to the user. It only serves to keep trace of constructors *)
      (* induced by type definitions in a module. So don't print ! *)
      (* Else if it comes from an exception, we must display it !  *)
      begin
      match cd.Typedtree.cstr_kind with
       | Typedtree.Exn file ->
	   fprintf ppf "exc@[eption %s__%s :@ %a@]" file (Ident.name id)
                                        Printtypes.pp_ml_type_scheme
                                        cd.Typedtree.cstr_scheme
       | Typedtree.Sum ->
	   fprintf ppf "con@[structor %s :@ %a@]" (Ident.name id)
                                        Printtypes.pp_ml_type_scheme
                                        cd.Typedtree.cstr_scheme
      end
  | Typedtree.Tsig_label (id, lbl_descr) ->
      fprintf ppf "rec@[ord label %s %a@]" (Ident.name id)
                                       pp_label_description lbl_descr
     


and pp_module_type ppf = function
  | Typedtree.Tmty_signature signature ->
      fprintf ppf "@[<hv>sig@;<1 1>@[<hv>%a@]@ end@]" pp_signature signature
  | Typedtree.Tmty_functor (_, _) ->
      fprintf ppf "functor (body skiped : only applications are displayed)"
;;



let rec pp_structure_excs ppf structure =
  let iter_pp_structure_excs = Printbasic.iter_pp "" pp_struct_item_excs in
  fprintf ppf "%a" iter_pp_structure_excs structure



and pp_struct_item_excs ppf = function
  | Typedtree.Tstr_eval e ->
      fprintf ppf "- :@[@ %a@]" Printtypes.pp_phi_type e.Typedtree.exp_exn
  | Typedtree.Tstr_value (_, bindings) ->
      List.iter (fun (p, e) ->
                     fprintf ppf "%a :@[@ %a@]" pp_pattern p
	                     Printtypes.pp_phi_type e.Typedtree.exp_exn)
                bindings
  | Typedtree.Tstr_primitive (_, _) -> ()
  | Typedtree.Tstr_type _ -> ()
  | Typedtree.Tstr_exception _ -> ()
  | Typedtree.Tstr_module (_, mod_expr) -> pp_module_expr_excs ppf mod_expr


and pp_module_expr_excs ppf mod_expr =
  pp_module_expr_desc_excs ppf mod_expr.Typedtree.mod_desc



and pp_module_expr_desc_excs ppf = function
  | Typedtree.Tmod_ident _ -> ()
  | Typedtree.Tmod_structure structure -> pp_structure_excs ppf structure
  | Typedtree.Tmod_functor (_, _) -> ()
  | Typedtree.Tmod_apply (_, _) ->
      (* Because applications are always between module idents *)
      (* there cannot be any escaping exceptions from here.    *)
      ()
;;



(* pp_toplevel_toplevel: Format.formatter ->                                *)
(*        Typedtree.toplevel_phrase * Typedtree.signature_item list -> unit *)
(* Print types for a toplevel phrase. If the structure is a Tstr_eval *)
(* then, this corresponds to a direct evaluation, so we don't look at *)
(* the signature. Else, we direclty print the signature. This is      *)
(* needed because direct order are forgotten in signatures, so their  *)
(* type can't be printed.                                             *)
let pp_toplevel_toplevel ppf (typed_phrase, phrase_type) =
 match typed_phrase with
  | Typedtree.Ptop_def [Typedtree.Tstr_eval e] ->
      fprintf ppf "- :@[ %a@.@]" Printtypes.pp_ml_type e.Typedtree.exp_type ;
      fprintf ppf "  :@[\ %a@]" Printtypes.pp_phi_type e.Typedtree.exp_exn
  | Typedtree.Ptop_def whatever ->
      (* Print type elements *)
      fprintf ppf "%a@." pp_signature phrase_type ;
      (* Print uncaught exceptions elements *)
      fprintf ppf "%a" pp_structure_excs whatever
  | _ -> assert false
;;
