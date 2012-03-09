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


(* Used when one tries to apply a module expression which is not a functor *)
exception Module_is_not_a_functor ;;
exception Unknown_directive of string ;;



(*
(* Normalize (NOT recursively) a signature by making *)
(* each component unique.                            *)
let normalize_signature signature =

 let rec is_in sig_item = function
   | [] -> false
   | Typedtree.Tsig_value (ident, _) :: rem ->
       (match sig_item with
        | Typedtree.Tsig_value (ident', _)
	            when Ident.name ident = Ident.name ident' -> true
        | _ -> is_in sig_item rem)
   | Typedtree.Tsig_type (ident, _) :: rem -> 
       (match sig_item with
        | Typedtree.Tsig_type (ident', _)
	            when Ident.name ident = Ident.name ident' -> true
        | _ -> is_in sig_item rem)
   | Typedtree.Tsig_module (ident, _) :: rem ->
       (match sig_item with
        | Typedtree.Tsig_module (ident', _)
                    when Ident.name ident = Ident.name ident' -> true
        | _ -> is_in sig_item rem)
   | Typedtree.Tsig_constructor (ident, _) :: rem ->
       (match sig_item with
        | Typedtree.Tsig_constructor (ident', _)
                     when Ident.name ident = Ident.name ident' -> true
	| _ -> is_in sig_item rem)
   | Typedtree.Tsig_label (ident, _) :: rem ->
       (match sig_item with
        | Typedtree.Tsig_label (ident', _)
                    when Ident.name ident = Ident.name ident' -> true
        | _ -> is_in sig_item rem) in

 let rec rec_normalize seen = function
   | [] -> []
   | sig_item :: rem ->
       if is_in sig_item seen then rec_normalize seen rem
       else sig_item :: rec_normalize (sig_item :: seen) rem

 in List.rev (rec_normalize [] (List.rev signature))
;;
*)



(* Set of functions that prefixes all types expressions using types  *)
(* constructors defined in this module, by the name of this module.  *)
(* That's a stronger version of the substitution we used before      *)
(* while accessing environment. In fact, this is a combinaision of   *)
(* this substitution and of strengthening. Because we shared the     *)
(* name (the path in fact) of all uses of a type name, when we       *)
(* replace it somewhere, it replaces it everywhere. The only thing   *)
(* it to know where to find where to do the modification. Because    *)
(* we kept a handle on the name of each type in its definition, we   *)
(* can make this global modification by just changing the handle in  *)
(* its definition. So, to change all occurences of a type name, we   *)
(* just need to change the name in its definition.                   *)
(* Why do we need this ? For 2 things. First when defining a type t  *)
(* in a module M, out of this module it must be seen as M.t          *)
(* Second, if other modules know this type under the name M.t, this  *)
(* becomes incompatible with the name is has in the module M (i.e t) *)
(* so if we try to unify a value of type t coming from M with a      *)
(* value of type t from another module (so with the name M.t), this  *)
(* fails. So the idea is to use M.t everywhere !                     *)
(* To do this, each time we have typed a module component, we change *)
(* all occurences (in other type definitions, as well as in          *)
(* constructors or field definitions) of all types names defined in  *)
(* this module by "this module"."this type name".                    *)
(* And for imbricated modules, we do the same thign recursively. So  *)
(* module A = module B = type t ... end end will lead to "t" changed *)
(* into A.B.t                                                        *)
(* So the first point we saw was previously handled by applying a    *)
(* substitution while acessing the environment. And the second one   *)
(* was handled by the strengthening. But with only these 2 stuffs,   *)
(* we failed on the second point when we had non generalized         *)
(* variables escaping from a module. For instance, the following     *)
(* case failed:                                                      *)
(*   module Perv =                                                   *)
(*     let daube = ref Nof_found                                     *)
(*     end ;;                                                        *)
(*   open Perv ;;                                                    *)
(*   module A =                                                      *)
(*     type error = Foo | Bar                                        *)
(*     exception Type_error of error                                 *)
(*     let f x = raise (if true then !daube else Type_error Foo)     *)
(*     end ;;                                                        *)
(*   module B =                                                      *)
(*     let g x = raise (if true then !daube else A.Type_error A.Bar) *)
(*     end ;;                                                        *)
(*                                                                   *)
(* This was because in A, daube was unified with                     *)
(*              exn[Type_error (error[Foo; ...]); ...]               *)
(* and in B, it was unified with                                     *)
(*              exn[Type_error (A.error[Bar; ...]); ...]             *)
(* and then error and A.error were not unifiable.                    *)
(* This is due to the fact that we had a *non generalized variable*  *)
(* escaping from a module and re-used in other modules.              *)
(* Indeed after havind "added" Foo to the reference, strengthening   *)
(* and substitution never change the "error" now present in this     *)
(* reference to "A.error", so "error" stayed as "error" for ever.    *)


(* ************************
val prefix_module_expr: Path.t -> Scopedtree.module_expr -> unit
*)
let rec prefix_module_expr prefix_path mod_expr =
 prefix_module_expr_desc prefix_path mod_expr.Typedtree.mod_desc


(* ************************
val prefix_module_expr_desc: Path.t -> Typedtree.module_expr_desc -> unit
*)
and prefix_module_expr_desc prefix_path = function
  | Typedtree.Tmod_ident _ -> ()
  | Typedtree.Tmod_structure str -> prefix_structure prefix_path str
  | Typedtree.Tmod_functor (_, _) -> ()
  | Typedtree.Tmod_apply (_, _) -> ()


(* ************************
val prefix_structure: Path.t -> Typedtree.structure -> unit
*)
and prefix_structure prefix_path str = 
 List.iter (prefix_structure_item prefix_path) str


(* ************************
val prefix_structure_item: Path.t -> Typedtree.structure_item_desc -> unit
*)
and prefix_structure_item prefix_path = function
 | Typedtree.Tstr_type list_of_ty_decl ->
     (* We change the path of each type declared in this module *)
     List.iter (fun (_, type_declaration) ->
                  let suffix =
		    Path.end_name !(type_declaration.Typedtree.type_path) in
                  let new_path = Path.Pdot (prefix_path, suffix) in
		  type_declaration.Typedtree.type_path := new_path)
	       list_of_ty_decl
 | Typedtree.Tstr_module (mod_ident, module_expr) ->
     (* We must inspect recursively imbricated module to replace their *)
     (* type declaration names by the current path + the imbricated    *)
     (* module name + the type path.                                   *)
     let new_prefix_path = Path.Pdot (prefix_path, Ident.name mod_ident) in
     prefix_module_expr new_prefix_path module_expr
 | _ -> ()
;;



(* ************************
val circularize_constructors:
  Typedtree.type_declaration list ->
  ('a * Typecore.ml_type_expr * 'b) list ->
  ('a * Typecore.ml_type_expr * 'b) list
*)
let circularize_constructors type_decls cstr_infos =
 Typecore.begin_definition () ;
 (* Cree la liste d'accus *)
 let accu_list =
   List.map (fun td -> (td.Typedtree.type_path, Typecore.type_variable ()))
            type_decls in
 Typecore.end_definition () ;
 (* Clean all types  *)
 List.iter (fun (_, cstr_ty, _) ->
              Typecore.circularize accu_list cstr_ty ;
              Typecore.clean_ml_type cstr_ty)
           cstr_infos ;
 cstr_infos
;;



(* ************************
val circularize_record_labels:
  Typedtree.type_declaration list ->
  ('a * 'b * Typecore.ml_type_expr * 'c) list ->
  ('a * 'b * Typecore.ml_type_expr * 'c) list
*)
let rec circularize_record_labels type_decls label_infos =
 Typecore.begin_definition () ;
 (* Cree la liste d'accus *)
 let accu_list =
   List.map (fun td -> (td.Typedtree.type_path, Typecore.type_variable ()))
            type_decls in
 Typecore.end_definition () ;
 (* Clean all types  *)
 List.iter (fun (_, _, lbl_ty, _) ->
              Typecore.circularize accu_list lbl_ty ;
              Typecore.clean_ml_type lbl_ty)
           label_infos ;
 label_infos
;;



(* ************************
val infer_structure:  Typedtree.envt ->
  Scopedtree.structure -> Typedtree.structure * Typedtree.signature
*)
(* ************************
Infers a signature for a structure. Return a types Typedtree.structure
(i.e Typedtree.structure_item list) and a Typedtree.signature (i.e
Typedtree.signature_item list)
*)
let rec infer_structure env = function
  | [] -> ([], [])
  | str_item :: rem ->
      (* While infering 1 Scopedtree.structure_item, we can *)
      (* get several Typedtree.signature_item and as many   *)
      (* Typedtree.structure, because functor applications  *)
      (* expansion can generate intermediate components.    *)
      let (typed_struct_items, sig_items) =
                   infer_structure_item env str_item.Scopedtree.pstr_desc in
      (* Because each next component of the structure must be typed  *)
      (* in the environment extended by previous components, ew must *)
      (* build a new environment before going on with typing next    *)
      (* components.                                                 *)
      let env' =
	List.fold_left (fun env_accu si -> Envtype.add_spec si env_accu)
                       env sig_items in
      let (rem_struct_items, rem_sig_items) = infer_structure env' rem in
      (typed_struct_items @ rem_struct_items, sig_items @ rem_sig_items)



(* ************************
val infer_structure_item :  Typedtree.envt ->
  Scopedtree.structure_item_desc ->
  Typedtree.structure_item list * Typedtree.signature_item list
*)
(* ************************
Infers a signature item for a structure item. Returns a <list> of
items because explicit naming can generate intermediate items.
*)
and infer_structure_item env = function
  | Scopedtree.Pstr_eval expr ->
      Infercore.new_mapping Infercore.Extensible [] ;
      (* Vider le mu-probleme *)
      Infercore.mu_problem := [] ;
      Infercore.mu_flag := Infercore.Mu_off ;
      (* Direct expressions don't appear in module types *)
      (* Typer l'expression : le mu-problème se remplit par effet de bord  *)
      (* les idents les plus extérieurs se retrouvant en te^te de la liste *)
      let typed_expr = Infercore.infer_expression env expr in
      Infercore.release_vars_mapping () ;
      (* On inverse le mu-problème pour simuler la structure d'envt, cad *)
      (* pour traiter les définitions les plus imbriquées en premier.   *)
      Infercore.mu_problem := List.rev (!Infercore.mu_problem) ;
      let _ = Infercore.solve_mu_problem () in
      (* But we keep the syntactic component *)
      ([Typedtree.Tstr_eval typed_expr], [(* No signature item *)])
  | Scopedtree.Pstr_value (rec_flag, bindings) ->
      Infercore.new_mapping Infercore.Extensible [] ;
      (* Vider le mu-probleme *)
      Infercore.mu_problem := [] ;
      Infercore.mu_flag := Infercore.Mu_off ;
      (* links : (Ident.t * Typecore.value_binding) list *)
      let (links, typed_bindings) =
	Infercore.infer_let_definition env rec_flag bindings in
      Infercore.release_vars_mapping () ;
      (* On inverse le mu-problème pour simuler la structure d'envt, cad *)
      (* pour traiter les définitions les plus imbriquées en premier.   *)
      Infercore.mu_problem := List.rev (!Infercore.mu_problem) ;
      let solved_mu_problem = Infercore.solve_mu_problem () in
      (* Juste pour avoir les defs toplevel en premier *)
      let solved_mu_problem_reversed = List.rev solved_mu_problem in
      (* Corrige les links en remplaçant les Mu par des Regular    *)
      (* a partir du mu_probleme résolu. ATTENTION: Comme certains *)
      (* trucs on pu etre retirés du mu pb ou carrement non mis    *)
      (* dedans, il se peut qu'on ne trouve pas tous les schémas   *)
      (* dans le solved_mu_problem_reversed. Dans ce cas, il faut  *)
      (* prendre ceux correspondant dans les links.                *)
      let newlinks =
	Infercore.make_final_links solved_mu_problem_reversed links in
      (* Maintenant, on cree les Sig_item à partir des schémas *)
      (* qu'on a récupéré. On rajoute chaque définition comme  *)
      (* une Tsig_value separée.                               *)
      let sig_items =
	  List.map (fun (id, sc) -> Typedtree.Tsig_value (id, sc))
                   newlinks in
      (* But we have only 1 structure item component *)
      let typed_struct_item =
	  [ Typedtree.Tstr_value (rec_flag, typed_bindings)] in
      (typed_struct_item, sig_items)
  | Scopedtree.Pstr_primitive (id, annoted_value_decl) ->
      let ty = Infercore.infer_primitive env annoted_value_decl in
      let scheme = Typecore.generalize_ml_type ty in
      let sig_items = [ Typedtree.Tsig_value (id, scheme) ] in
      let typed_struct_item =
	   [ Typedtree.Tstr_primitive (id, annoted_value_decl) ] in
      (typed_struct_item, sig_items)
  | Scopedtree.Pstr_type names_ty_declarations ->
	(* Let's build the fake environment where each type identifier is *)
        (* bound to handle recursive type definitions.                    *)
        let fake_decls = List.map
               (fun (id, decl) ->
		 Typecore.begin_definition () ;
		 (* Paramètres temporaires du type *)
		 let tmpplist = List.map (fun _ -> Typecore.type_variable ())
                                         decl.Scopedtree.ptype_params in
		 (* Ce qui va devenir le schéma temporaire du type *)
		 let tmpvar = Typecore.type_variable () in
		 Typecore.end_definition () ;
		 match decl.Scopedtree.ptype_kind with
		  | Scopedtree.Ptype_abstract ->
		      let d =
		      { Typedtree.type_params = tmpplist ;
			Typedtree.type_arity =
                              List.length decl.Scopedtree.ptype_params ;
			Typedtree.type_kind = Typedtree.Type_abstract ;
			Typedtree.type_manifest =
			   (* Si le type st totalement abstrait, c'est pas *)
                           (* utile de mettre un schéma dans la declaration *)
                           (* typée car il ne sera jamais utilisé. *)
			   if decl.Scopedtree.ptype_manifest = None then None
			   else Some (Typecore.trivial_ml_type_scheme tmpvar) ;
			Typedtree.type_path = decl.Scopedtree.ptype_path } in
		      (id, d)
		  | Scopedtree.Ptype_record _ ->
		      let d =
		      { Typedtree.type_params = tmpplist ;
			Typedtree.type_arity =
                               List.length decl.Scopedtree.ptype_params ;
			Typedtree.type_kind = Typedtree.Type_record [] ;
			Typedtree.type_manifest = None ;
			Typedtree.type_path = decl.Scopedtree.ptype_path } in
		      (id, d)
		  | Scopedtree.Ptype_variant _ ->
		      let d =
		      { Typedtree.type_params = tmpplist ;
			Typedtree.type_arity =
                               List.length decl.Scopedtree.ptype_params ;
			Typedtree.type_kind = Typedtree.Type_variant [] ;
			Typedtree.type_manifest = None ;
			Typedtree.type_path = decl.Scopedtree.ptype_path } in
		      (id, d))
	  names_ty_declarations in

	let fake_env =
	  List.fold_left
	    (fun env_accu (id, decl) -> Envtype.add_type id decl env_accu)
	    env fake_decls in

	(* On infère les déclarations de types *)
	Typecore.begin_definition () ;
        let tmp = List.map
          (fun (_, ty_decl) ->
		  Infercore.infer_type_declaration fake_env ty_decl)
	  names_ty_declarations in
	Typecore.end_definition () ;

	let (constr_infos, label_infos, type_infos) = Listextra.split3 tmp in

	(* On fabrique les declarations de type finales *)
	let names_ty_declarations =
	  List.map2 (fun (name, decl) (alias, params) ->
	    match alias with
	     | None ->
		 (* Provient forcément d'un type totalement abstrait *)
		 (* d'un type somme.                                 *)
		 let _ = List.map Typecore.generalize_ml_type params in
		 let decl' = {
		   Typedtree.type_params = params ;
		   Typedtree.type_arity = decl.Typedtree.type_arity ;
		   Typedtree.type_kind = Typedtree.Type_abstract ;
                   Typedtree.type_manifest = None ;
		   Typedtree.type_path = decl.Typedtree.type_path } in
		 (name, decl')
	     | Some ty ->
		 (* Pour le moment, provient d'un type abbrév *)
		 let (sch, params') = Typecore.generalize_ml_type2
                                           ty decl.Typedtree.type_params in
		 let decl' = {
		   Typedtree.type_params = params' ;
		   Typedtree.type_arity = decl.Typedtree.type_arity ;
		   Typedtree.type_kind = Typedtree.Type_abstract ;
                   Typedtree.type_manifest = Some sch ;
		   Typedtree.type_path = decl.Typedtree.type_path } in
		 (name, decl'))
	  fake_decls type_infos in
	let only_ty_declarations = List.map snd names_ty_declarations in

	(* QUESTION: How to add constructors in the envt without adding them *)
        (* to the visible signature ? Solution : create a Tig_constructor    *)
        (* which won't be printed, but used during typing.                   *)

        (* Flatten all constructors of all type declarations *)
        let constr_infos = List.flatten constr_infos in
        (* Circularize constructors of recursive types *)
        let constr_infos = circularize_constructors
	                            only_ty_declarations
                                    constr_infos in
        (* Il faut maintenant généraliser les schémas des constructeurs *)
        let constr_descrs =
	  List.map (fun (name, ty, arity) ->
	              let cstr_sch = Typecore.generalize_ml_type ty in
	              let cstr_decl = {
			Typedtree.cstr_kind = Typedtree.Sum ;
			Typedtree.cstr_arity = arity ;
			Typedtree.cstr_scheme = cstr_sch } in
		      (name, cstr_decl))
                   constr_infos in

        (* Flatten all labels of all type declarations *)
        let label_infos = List.flatten label_infos in
        (* Circularize record labels of recursive types *)
        let label_infos = circularize_record_labels only_ty_declarations
	                                     label_infos in
        (* Il faut maintenant généraliser les schémas des labels *)
	let label_descrs =
	  List.map (fun (name, mut_flag, ty, lbl_count) ->
	             let lbl_sch = Typecore.generalize_ml_type ty in
		     let lbl_decl = {
		       Typedtree.fld_mut = mut_flag ;
		       Typedtree.fld_scheme = lbl_sch ;
		       Typedtree.fld_countall = lbl_count } in
		     (name, lbl_decl))
	           label_infos in
        (* First build the Tsig_type conponents *)
        let tsig_types = List.map (fun (n, d) -> Typedtree.Tsig_type (n, d))
                                  names_ty_declarations in
        (* Then build the Tsig_constructor components *)
        let tsig_constructors =
	  List.map (fun (n, d) -> Typedtree.Tsig_constructor (n, d))
	           constr_descrs in
        (* Next, build the Tsig_label components *)
        let tsig_labels =
	  List.map (fun (n, d) -> Typedtree.Tsig_label (n, d))
	           label_descrs in
      (* The signature items are the union of these 2 components lists *)
      let sig_items = tsig_types @ tsig_constructors @ tsig_labels in
      (* The structure items only contain the type declarations *)
      let typed_struct_item =
	[ Typedtree.Tstr_type names_ty_declarations] in
      (* Ouf !.. *)
      (typed_struct_item, sig_items)
  | Scopedtree.Pstr_exception (id, exc_decl) ->
      (* C'est 'infer_exception' qui se charge des begin/end_definitions *)
      let cstr_decl = Infercore.infer_exception env id exc_decl in
      (* We simply consider exceptions as constructors for typing *)
      let typed_struct_item = [ Typedtree.Tstr_exception id ] in
      let sig_items = [ Typedtree.Tsig_constructor (id, cstr_decl) ] in
      (typed_struct_item, sig_items)
  | Scopedtree.Pstr_module (ident, mod_expr) ->
      let (typed_mod_expr, shadows) = infer_module_expr env mod_expr in
      (* If the module expression was an application, then we       *)
      (* have implicitely constructed a shadowed module expression  *)
      (* and then THIS module expression must be prefixed, and not  *)
      (* mod_expr (which is THE application expression). So, in     *)
      (* the case where we have infered an Apply expression, we are *)
      (* given the real expression that has been typed (see the     *)
      (* function 'infer_module_expr').                             *)
      let shadowed_mod_expr =
	List.map (fun (name, expr) ->
	             Typedtree.Tstr_module (name, expr)) shadows in
      let shadowed_mod_tys =
	List.map (fun (name, expr) ->
	             Typedtree.Tsig_module (name, expr.Typedtree.mod_type))
	         shadows in
      prefix_module_expr (Path.Pident ident) typed_mod_expr ;
      (shadowed_mod_expr @ [Typedtree.Tstr_module (ident, typed_mod_expr)],
       shadowed_mod_tys @
	 [Typedtree.Tsig_module (ident, typed_mod_expr.Typedtree.mod_type)])
  | Scopedtree.Pstr_modtype ->
      (* Module types are forgotten because we don't     *)
      (* have signature matching, so we don't need them. *)
      ([], [])
  | Scopedtree.Pstr_open _ ->
      (* This was handled during scoping, so forget this component *)
      ([], [])


(* ************************
val infer_module_expr:
  Typedtree.envt ->
  Scopedtree.module_expr ->
  Typedtree.module_expr * (Ident.t * Typedtree.module_expr) list
*)
(* ************************
Infers a module type for a module expression. Returns a typed module
expression.
*)
and infer_module_expr env mod_expr =
  let ((typed_mod_expr_desc, module_ty), shadows) =
        infer_module_expr_desc env mod_expr.Scopedtree.pmod_desc in
  { Typedtree.mod_desc = typed_mod_expr_desc ;
    Typedtree.mod_type = module_ty ;
    Typedtree.mod_loc= mod_expr.Scopedtree.pmod_loc }, shadows



(* ************************
val infer_module_expr_desc:
  Typedtree.envt ->
  Scopedtree.module_expr_desc ->
  (Typedtree.module_expr_desc * Typedtree.module_type) *
  (Ident.t * Typedtree.module_expr) list
*)
(* ************************
Infers a module type for a module expression description. Returns a typed
Typedtree.module_expr_desc and a Typedtree.module_type, so that it's possible
to re-construct a typed module expression.
*)
and infer_module_expr_desc env = function
  | Scopedtree.Pmod_ident path ->
      let mod_ty = Envtype.find_module path env in
      (Typedtree.Tmod_ident path, mod_ty), []
  | Scopedtree.Pmod_structure structure ->
      let (typed_struct, signature) = infer_structure env structure in
      (* let signature = normalize_signature signature in *)
      (* Return the syntactic structure typed, and its signature *)
      (Typedtree.Tmod_structure typed_struct,
       Typedtree.Tmty_signature signature), []
  | Scopedtree.Pmod_functor (arg_name, body) ->
      (* The type of a functor is it text *)
      (* We do not need any more to enclose the environment because  *)
      (* the functor's body has been scoped, so there is no problem  *)
      (* of capture. And then, enclosign the environment caused a    *)
      (* problem: module F (X) = struct type t = X.t end             *)
      (*          type foo = ...                                     *)
      (*          module A = struct type t = foo end                 *)
      (*          module R = F (A)                                   *)
      (* When expanding the body of the functor, we should have used *)
      (* the environment it had enclosed to ensure non-capture. But  *)
      (* in this environment, type foo was not yet known, so we got  *)
      (* unable to type the expanded body of the functor !!!         *)
      (Typedtree.Tmod_functor (arg_name, body),
       Typedtree.Tmty_functor (arg_name, body)), []
  | Scopedtree.Pmod_apply (funct, arg) ->
      let (typed_funct, fun_shadow) = infer_module_expr env funct in
      (* Il faut rajouter ds l'environnement les modules caches provenant *)
      (* de la partie fonctionnelle de l'application.                     *)
      let env =
        List.fold_left
	        (fun accu (name, tme) ->
		        Envtype.add_module name tme.Typedtree.mod_type accu)
	        env fun_shadow in
      match typed_funct.Typedtree.mod_type with
       | Typedtree.Tmty_functor (param_ident, funct_body) ->
           (* Typer l'arg *)
	   let (typed_arg, arg_shadow) = infer_module_expr env arg in
	   (* Il faut s'assurer que l'arg a ete nomme. Si c'est pas le *)
	   (* cas, lier son type a un nom local qui ne doit pas *)
           (* ressortir *)
	   let (env', arg_path, new_shadow) =
	     (match arg.Scopedtree.pmod_desc with
	       | Scopedtree.Pmod_ident arg_path -> (env, arg_path, [])
	       | _ ->
		   (* Creer un path pp, rajouter (pp, ty de arg) dans env. *)
		   let new_ident = Ident.create "^M" in
		   let new_path = Path.Pident new_ident in
		   (* Don't forget to prefix the newly create module type *)
		   prefix_module_expr new_path typed_arg ;
		   let new_env = Envtype.add_module new_ident
		                   typed_arg.Typedtree.mod_type env in
		   let shad = (new_ident, {
			   Typedtree.mod_desc = typed_arg.Typedtree.mod_desc ;
			   Typedtree.mod_type = typed_arg.Typedtree.mod_type ; 
			  Typedtree.mod_loc = Location.none }) in
		   (new_env, new_path, [shad])) in
           (* Recuperer le nom du module argument et le substituer *)
           (* a 'param_ident' dans 'funct_body'                    *)
	   let subst = Subst.add param_ident arg_path Subst.identity in
	   let substituted_body =
	     Substmod.subst_module_expr subst funct_body in
           (* Typer le nouveau body dans l'env ou on a rajoute l'eventuel *)
           (* nommage explicite *)
	   let (typed_body, shadow') =
	     infer_module_expr env' substituted_body in
	   (typed_body.Typedtree.mod_desc, typed_body.Typedtree.mod_type),
	   (fun_shadow @ arg_shadow @ new_shadow @ shadow')
       | _ -> raise Module_is_not_a_functor
;;



(* Used to tell the main that we must swith to an included file *)
exception Push_file of string ;;



(* ************************
val infer_toplevel_phrase:
  Typedtree.envt ->
  Scopedtree.toplevel_phrase ->
  Typedtree.toplevel_phrase * Typedtree.signature
*)
(* ************************
Return the typed toplevel_phrase and it type (== Typedtree.signature)
*)
let infer_toplevel_phrase env = function
  | Scopedtree.Ptop_def structure ->
      let (typed_structure, signature) = infer_structure env structure in
      (* let signature = normalize_signature signature in *)
      (Typedtree.Ptop_def (typed_structure), signature)
  | Scopedtree.Ptop_dir (dir_name, dir_arg) ->
      match (dir_name, dir_arg) with
       | ("use", Scopedtree.Pdir_string filename) -> raise (Push_file filename)
       | _ -> raise (Unknown_directive dir_name)
;;



(* ************************
val find_module_expr_exception :
  Typecore.phi_expr -> Typedtree.module_expr -> unit
*)
(* ************************
Recherche et unifie dans 'exn_accu' toutes les exceptions pouvant sortir d'une
expression de module. Sert à récupérer les exceptions pouvant sortir d'un
letmodule. En effet, il faut prendre en compte celles survenant dans la
définition du module. Or afin d'éviter des unifications parasites, dans le
cas du typage général d'un module, on n'unifie pas les effets de chaque
composante.
*)
let rec find_module_expr_exception exn_accu mod_expr =
  find_module_expr_desc_exception exn_accu mod_expr.Typedtree.mod_desc


and find_module_expr_desc_exception exn_accu = function
  | Typedtree.Tmod_ident _ -> ()
  | Typedtree.Tmod_structure structure ->
      List.iter (find_structure_item_exception exn_accu) structure
  | Typedtree.Tmod_functor (_, _) -> ()
  | Typedtree.Tmod_apply (_, _) ->
      (* Because applications are always between module idents *)
      (* there cannot be any escaping exceptions from here.    *)
      ()


and find_structure_item_exception exn_accu = function
  | Typedtree.Tstr_eval e ->
      Typecore.unify_phi_type exn_accu e.Typedtree.exp_exn
  | Typedtree.Tstr_value (_, bindings) ->
      List.iter
	(fun (_, e) -> Typecore.unify_phi_type exn_accu e.Typedtree.exp_exn)
        bindings
  | Typedtree.Tstr_primitive (_, _) -> ()
  | Typedtree.Tstr_type _ -> ()
  | Typedtree.Tstr_exception _ -> ()
  | Typedtree.Tstr_module (_, mod_expr) ->
       find_module_expr_exception exn_accu mod_expr
;;


Infercore.infer_module_expr_forward := infer_module_expr ;;
Infercore.find_module_expr_exception_forward := find_module_expr_exception ;;
