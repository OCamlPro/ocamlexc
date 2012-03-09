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



let infer_module_expr_forward = ref (fun _ _ -> assert false) ;;
let find_module_expr_exception_forward = ref (fun _ _ -> assert false) ;;


type policy = Fixed | Extensible ;;

type variable_mapping = {
  vm_policy : policy ;
  mutable vm_ml : (string * Typecore.ml_type_expr) list ;
  mutable vm_rho : (string * Typecore.row_variable) list ;
  mutable vm_pre : (string * Typecore.simple_presence) list
} ;;


let global_vars_mappings = ref ([] : variable_mapping list) ;;

let new_mapping policy mlvar_names =
  let m = {
    vm_policy = policy ;
    vm_ml = List.map (fun n -> n, Typecore.type_variable ()) mlvar_names ;
    vm_rho = [] ;
    vm_pre = [] } in
  global_vars_mappings := m :: !global_vars_mappings
;;

let release_vars_mapping () =
  match !global_vars_mappings with
   [] -> assert false | _ :: q -> global_vars_mappings := q
;;

let current_vars_mapping () =
  match !global_vars_mappings with [] -> assert false | m :: _ -> m
;;




(* Raised when a constructor is used with a bad arity *)
exception Constructor_arity_error of Path.t ;;
exception Unbound_type_variable of string ;;
exception Type_arity_error of Path.t ;;
exception Bad_labels_number ;;
exception Field_not_mutable of Path.t ;;
exception Invalid_equiv_in_variant_type ;;


type mu_indicator = Mu_off | Mu_on ;;
let mu_flag = ref Mu_off ;;

type mu_definition = {
    (* Compteur d'expansions *)
    mu_saturation : int ;
    (* Sche'ma d'accumulation *)
    mu_scheme : Typecore.ml_types_scheme ;
    (* Nom de l'identificateur lie' par let ou let rec *)
    mu_name : Ident.t ;
    (* Occurrences de cet identificateur dans l'expr definissante seult *)
    mu_body_occs : Typecore.occurrence list ;
    (* Occurrences de cet identificateur ds la partie 'in' *)
    mu_in_occs : Typecore.occurrence list ref ;
    (* Type trouve' pour le corps de l'expression de'finissante *)
    mu_type : Typecore.ml_type_expr
  } ;;

type mu_sub_problem = {
    (* Niveau auquel se trouve le "let" qui donne lieu a` ce sous-proble`me *)
    mu_sub_level : int ;
    (* Ensemble des sous-proble`mes, du let (rec) and *)
    mu_sub_defs : mu_definition list
  } ;;

type mu_problem = mu_sub_problem list ;;

let mu_problem = ref ([] : mu_problem) ;;





(* is_nonexpansive: Scopedtree.expression -> bool *)
(* If true, then we can generalize *)
let rec is_nonexpansive env express =
  match express.Scopedtree.pexp_desc with
   | Scopedtree.Pexp_ident _ -> true
   | Scopedtree.Pexp_constant _ -> true
   | Scopedtree.Pexp_let (_, pat_exp_list, body) ->
       List.for_all (fun (_, e) -> is_nonexpansive env e) pat_exp_list &&
       is_nonexpansive env body
   | Scopedtree.Pexp_function _ -> true
   | Scopedtree.Pexp_apply (_, _) -> false
   | Scopedtree.Pexp_match (e0, pat_exp_list) ->
       List.for_all (fun (_, e) -> is_nonexpansive env e) pat_exp_list &&
       is_nonexpansive env e0
   | Scopedtree.Pexp_try (e0, pat_exp_list) ->
       List.for_all (fun (_, e) -> is_nonexpansive env e) pat_exp_list &&
       is_nonexpansive env e0
   | Scopedtree.Pexp_tuple el ->
       List.for_all (is_nonexpansive env) el
   | Scopedtree.Pexp_construct (_, e_opt) ->
       (match e_opt with None -> true | Some e -> is_nonexpansive env e)
   | Scopedtree.Pexp_record (lbl_exp_list, opt_init_exp) ->
       List.for_all
          (fun (lbl, exp) ->
	    let lbl_desc = Envtype.find_label lbl env in
	    lbl_desc.Typedtree.fld_mut = Asttypes.Immutable &&
            is_nonexpansive env exp)
          lbl_exp_list &&
      (match opt_init_exp with None -> true | Some e -> is_nonexpansive env e)
   | Scopedtree.Pexp_field (e, _) -> is_nonexpansive env e
   | Scopedtree.Pexp_setfield (e0, _, e1) ->
       is_nonexpansive env e0 && is_nonexpansive env e1
   | Scopedtree.Pexp_array _ -> false
   | Scopedtree.Pexp_ifthenelse (_, e1, e2_opt) ->
       (* is_nonexpansive env e0 && *)
       is_nonexpansive env e1 &&
       (match e2_opt with None -> true | Some e2 -> is_nonexpansive env e2)
   | Scopedtree.Pexp_sequence (e0, e1) ->
       is_nonexpansive env e0 && is_nonexpansive env e1
   | Scopedtree.Pexp_while (e0, e1) ->
       is_nonexpansive env e0 && is_nonexpansive env e1
   | Scopedtree.Pexp_for (_, e0, e1, _, e2) ->
       is_nonexpansive env e0 && is_nonexpansive env e1 &&
       is_nonexpansive env e2
   | Scopedtree.Pexp_constraint (e0, _, _) -> is_nonexpansive env e0
   | Scopedtree.Pexp_when (e0, e1) ->
       is_nonexpansive env e0 && is_nonexpansive env e1
   | Scopedtree.Pexp_letmodule _ -> false
;;



(* infer_constant: Asttypes.constant -> Typecore.ml_type_expr *)
let infer_constant = function
  | Asttypes.Const_int i -> Typecore.type_int i
  | Asttypes.Const_char chr -> Typecore.type_char chr
  | Asttypes.Const_string str -> Typecore.type_string str
  | Asttypes.Const_float float_string -> Typecore.type_float float_string
;;



let rec core_type_to_ml_type_expr env core_ty =
 let vars_mapping = current_vars_mapping () in
 match core_ty.Scopedtree.ptyp_desc with
  | Scopedtree.Ptyp_any -> assert false   (* Never used but by camlp4 *)
  | Scopedtree.Ptyp_var var_name ->
      begin
      try List.assoc var_name vars_mapping.vm_ml with
       Not_found ->
	 match vars_mapping.vm_policy with
 	  | Fixed -> raise (Unbound_type_variable var_name)
	  | Extensible ->
	      let new_var = Typecore.type_variable () in
	      vars_mapping.vm_ml <- (var_name, new_var) :: vars_mapping.vm_ml ;
	      new_var
      end
  | Scopedtree.Ptyp_arrow (neg, pos) ->
      let neg' = core_type_to_ml_type_expr env neg in
      let pos' = core_type_to_ml_type_expr env pos in
      Typecore.type_arrow neg' pos' (Typecore.empty_phi ())
  | Scopedtree.Ptyp_tuple args ->
      let args' = List.map (core_type_to_ml_type_expr env) args in
      Typecore.type_tuple args'
  | Scopedtree.Ptyp_constr (path, args) ->
      let args' = List.map (core_type_to_ml_type_expr env) args in
      let decl = Envtype.find_type !path env in
      if List.length args <> decl.Typedtree.type_arity then
	raise (Type_arity_error !path) ;
      begin
      match decl.Typedtree.type_manifest with
       | None ->
	   (* Le path est celui d'un type abstrait, donc on génère *)
           (* simplement un type avec comme nom ce path. *)
	   Typecore.type_constr decl.Typedtree.type_path args'
                                (Typecore.empty_phi ())
       | Some sch ->
	   (* Le path est celui d'un type abbrév, i.e manifest égal à  *)
           (* qq chose, donc on va récupérer une instance de ce à quoi *)
           (* se résume le type ayant ce path et on va l'unifier ses   *)
           (* paramètres avec nos arguments.                           *)
           let (ty, tys) =
	     Typecore.specialize_ml_type2 sch decl.Typedtree.type_params in
	   List.iter2 Typecore.unify_ml_type tys args' ;
	   ty
      end
  | Scopedtree.Ptyp_alias (core_ty, alias_name) ->
      (* Dans le cas d'un alias, on étend toujours le mapping *)
      let ty = core_type_to_ml_type_expr env core_ty in
      vars_mapping.vm_ml <- (alias_name, ty) :: vars_mapping.vm_ml ;
      ty
;;



(* ************************
val infer_pattern :
  Typecore.ml_type_expr ->
  Typedtree.envt ->
  Scopedtree.pattern ->
  (Ident.t * Typecore.ml_type_expr) list * Typedtree.pattern
*)
(* ************************
Performs unification from one pattern to the next by side effect.
Returns bindings induced by the pattern and the typed pattern.
*)
let rec infer_pattern accu_ty env pat =
 let (bindings, pat') =
        infer_pattern_desc accu_ty env pat.Scopedtree.ppat_desc in
 (bindings, { Typedtree.pat_desc = pat' ;
	      Typedtree.pat_loc = pat.Scopedtree.ppat_loc })



(* ************************
val infer_pattern_desc :
  Typecore.ml_type_expr ->
  Typedtree.envt ->
  Scopedtree.pattern_desc ->
  (Ident.t * Typecore.ml_type_expr) list * Typedtree.pattern_desc
*)
(* ************************
Performs unification from one pattern to the next by side effect.
Returns bindings induced by the pattern and the typed pattern descr
*)
and infer_pattern_desc accu_ty env = function
  | Scopedtree.Ppat_any ->
      (* Treated like variable pattern, but we don't bind *)
      let ty = Typecore.type_variable () in
      (* Let's perform unification with previous patterns as a side effect *)
      Typecore.unify_ml_type accu_ty ty ;
      ([], Typedtree.Tpat_any)
  | Scopedtree.Ppat_var ident ->
      let ty = Typecore.type_variable () in
      (* Let's unify by side effect *)
      Typecore.unify_ml_type accu_ty ty ;
      let binding = [ident, ty] in
      (binding, Typedtree.Tpat_var ident)
  | Scopedtree.Ppat_alias (pat, alias_ident) ->
      let ty = Typecore.type_variable () in
      let (bnds, typed_pat) = infer_pattern ty env pat in
      Typecore.unify_ml_type accu_ty ty ;
      let bindings = (alias_ident, ty) :: bnds in
      (bindings, Typedtree.Tpat_alias (typed_pat, alias_ident))
  | Scopedtree.Ppat_constant constant ->
      let ty = infer_constant constant in
      (* Let's unify by side effect *)
      Typecore.unify_ml_type accu_ty ty ;
      ([], Typedtree.Tpat_constant constant)
  | Scopedtree.Ppat_tuple args ->
      let accu_args = List.map (fun _ -> Typecore.type_variable ()) args in
      let (bindings, args_pats) = List.split (
         List.map2 (fun accu pat -> infer_pattern accu env pat)
	           accu_args args) in
      (* Now unify by side effect *)
      Typecore.unify_ml_type accu_ty (Typecore.type_tuple accu_args) ;
      (List.flatten bindings, Typedtree.Tpat_tuple args_pats)
  | Scopedtree.Ppat_construct (path, pat_opt) ->
      let cstr_decl = Envtype.find_constructor path env in
      begin
      match (pat_opt, cstr_decl.Typedtree.cstr_arity) with
       | (None, Typedtree.Zeroary) ->
	   let ty =
	     Typecore.specialize_ml_type cstr_decl.Typedtree.cstr_scheme in
	   (* Let's unify by side effect *)
	   Typecore.unify_ml_type accu_ty ty ;
	   (* No bindings... *)
	   ([], Typedtree.Tpat_construct (path, None))
       | (Some rec_pat, Typedtree.Unary) ->
	   let cstr_ty =
	     Typecore.specialize_ml_type cstr_decl.Typedtree.cstr_scheme in
	   let arg_ty = Typecore.type_variable () in
	   let result_ty = Typecore.type_variable () in
	   (* Let's infer recursive pattern's type *)
	   let (rec_bnds, rec_pat') = infer_pattern arg_ty env rec_pat in
	   (* Simulation of an application *)
	   Typecore.unify_ml_type
	                  cstr_ty (Typecore.type_arrow arg_ty result_ty
                                                    (Typecore.empty_phi ())) ;
	   (* Let's unify by side effect *)
	   Typecore.unify_ml_type accu_ty result_ty ;
	   (* Bindings are those coming from the recursive pattern binding *)
	   (rec_bnds, Typedtree.Tpat_construct (path, Some rec_pat'))
       | (_, _) -> raise (Constructor_arity_error path)
      end
  | Scopedtree.Ppat_record labels ->
      (* Type for this pattern *)
      let ty = Typecore.type_variable () in
      (* Retrieve bindings coming from patterns embedded in *)
      (* the record, and the typed version of this pattern. *)
      let (bnds, typed_labels) = List.split (
        List.map (fun (lbl_path, pat) ->
		let lbl_desc = Envtype.find_label lbl_path env in
		(* Type (functionnal) of the label *)
		let lbl_ty = Typecore.specialize_ml_type
		                       lbl_desc.Typedtree.fld_scheme in
		(* We must infer the pattern for this field and verify it is *)
		(* compatible with the type of value for this pattern.       *)
		let lbl_arg_ty = Typecore.type_variable () in
		let (bnd, typed_pat) = infer_pattern lbl_arg_ty env pat in
		Typecore.unify_ml_type
		           (Typecore.type_arrow lbl_arg_ty ty
                                        (Typecore.empty_phi ()))
                           lbl_ty ;
		(* Returns bindings and the typed list of labels *)
		(bnd, (lbl_path, typed_pat)))
	      labels) in
      (* Flatten all bindings *)
      let bnds = List.flatten bnds in
      (* Perform unification as a side effect *)
      Typecore.unify_ml_type accu_ty ty ;
      (bnds, Typedtree.Tpat_record typed_labels)
  | Scopedtree.Ppat_array pat_lst ->
    (* Type of elelements in the array *)
    let ty_elem = Typecore.type_variable () in
    (* Type for this global pattern *)
    let ty = Typecore.type_array ty_elem in
    (* Now we need to type each subpattern *)
    let (bnds, typed_pats) =
      List.split (List.map (infer_pattern ty_elem env) pat_lst) in
    (* Perform unification as a side effect *)
    Typecore.unify_ml_type accu_ty ty ;
    (* Give the result *)
    (List.flatten bnds, Typedtree.Tpat_array typed_pats)
  | Scopedtree.Ppat_or (pat1, pat2) ->
      (* Just do as it was 2 separate patterns *)
      let (bnds1, typed_pat1) = infer_pattern accu_ty env pat1 in
      let (bnds2, typed_pat2) = infer_pattern accu_ty env pat2 in
      (bnds1 @ bnds2), Typedtree.Tpat_or (typed_pat1, typed_pat2)
  | Scopedtree.Ppat_constraint (pat, core_ty) ->
      let (bnds, typed_pat) = infer_pattern accu_ty env pat in
      let constraint_ty = core_type_to_ml_type_expr env core_ty in
      (* Force unification *)
      Typecore.unify_ml_type accu_ty constraint_ty ;
      (bnds, Typedtree.Tpat_constraint (typed_pat, core_ty))
;;




(* ************************
val build_defs_and_links :
  (Ident.t * Typecore.ml_type_expr * 'a) list ->
  Typedtree.envt ->
  mu_definition list * (Ident.t * Typecore.value_binding) list
*)
(* ************************
Note: links = (Tdent.t, Typecore.ml_type_expr) list
Cette fonction a pour but de cre'er les sous-mu-proble`mes induits par un
let (rec) and, ainsi que la simili-extension d'environnement qui sera rendue
'par infer_let_definition' (simili car c,a n'est qu'une structure de liste
et non re'ellement une structure d'environnement ; de toutes fac,ons cette
liste va e^tre transforme'e en environnement re'el apre`s). Dans cette
extension, les de'finitions ne ne'cessitant pas de re'solution mu sont
me'morise'es directement en tant que Regular.
En re'sume', les defs sont les e'le'ments devant passer en proble`me mu.
L'extension d'environnement contient les e'le'ments ne'cessitant la re`gle mu
inchange's et les e'le'ments ne la ne'ce'ssitant en fin de compte pas,
transforme' de Mu en Regular. L'environnement 'env' passe' en argument est
celui dans lequel on a type' des expressions de'finissantes de lets. Il va
donc nous servir pour rechercher a` quel Mu chaque identificateur des 'links' a
e'te' binde'. En effet, dans 'links' se trouvent tous les identificateurs qui
ont e'te' de'finis dans la de'finition let courante, et ce sont ces
identificateurs qui sont succeptibles de faire parti du mu proble`me.
Notons que quand on recherche un de ces identificateurs dans l'environnement,
il ne peut e^tre binde' qu`a` un lien de type Mu puisque l'on est en mode mu
(sinon on ne nous aurait pas appele') et que dans ce mode, tous les
identificateurs sont inse're' sous l'e'tiquette Mu.
*)
let rec build_defs_and_links original_links env =
  match original_links with
   | [] -> (* Plus d'identificateurs de'finis dans ce let *) ([], [])
   | (id, ety, _) :: rem ->
       (* On recherche a quoi a e'te' lie' cet identificateur dans     *)
       (* l'environnement utilise' pour typer le corps de l'expression *)
       (* de'finissante du let courant.                                *)
       match Envtype.get_raw_value_binding id env with
        | Typecore.Mu_ident (_, occs) as evtelem ->
	    (* On garde la main sur les occurences de corps (re'cursives) *)
	    let boccs = !occs in
	    (* On vide la liste des occurences, comme c,a maintenant *)
            (* elle ne va plus me'moriser que les occurences de 'in' *)
	    occs := [] ;
            (* Le sche'ma de type inse're' au de'but dans le mu-proble`me *)
            (* est une ge'ne'ralisation du type trouve' par Milner pour   *)
            (* l'expression de'finissante (celui-la` me^me qui se trouve  *)
            (* me'morise' dans les 'original_links'.                      *)
	    let mu_scheme = Typecore.copying_generalization ety in
            (* S'il n'y a pas de variables ge'ne'ralisables dans ce sche'ma *)
            (* de de'part, alors on est su^r que le type trouve' n'est pas  *)
            (* trop ge'ne'ral et donc pas besoin d'ite'rer dessus.          *)
	    if mu_scheme.Typecore.mts_count = 0 then
	      begin
	      (* Il faut alors ve'rifier que toutes les occurrences du corps *)
              (* sont bien des instances du sche'ma ainsi trouve'.           *)
	      List.iter (fun { Typecore.occ_level = lev ;
			       Typecore.occ_type = nty } ->
                           Typecore.unify_ml_type nty
                                        (Typecore.specialize_ml_type_with_level
					                lev mu_scheme))
		        boccs ;
	      (* Cet identificateur n'a plus aucune raison de faire parti *)
              (* du mu proble`me. On cre'e donc directement sont binding  *)
              (* d'environnement en le faisant Regular (il vient donc en  *)
              (* remplacement de celui, Mu, qu'il y avait avant).         *)
	      let evtelem' = Typecore.Regular_ident mu_scheme in
	      let (defs, links') = build_defs_and_links rem env in
	      (* Pas de nouvelle def, mais environnement passe' en Regular *)
	      (defs, (id, evtelem') :: links')
	      end
	    else
	      begin
	      (* Il y a des variables ge'ne'ralisables dans le sche'ma, *)
              (* il peut encore se spe'cialiser au cours des prochaines *)
              (* ite'rations. Il intervient donc dans le mu proble`me.  *)
              (* Il faut donc cre'er un mu-sous-proble`me pour cet      *)
              (* identificateur.                                        *)
	      let def =
		{ mu_saturation = 0 ;       (* A l'origine, aucun gonflage *)
		  mu_scheme = mu_scheme ;
		  mu_name = id ;
		  mu_body_occs = boccs ;  (* Occurrences du corps seulement *)
		  mu_in_occs = occs ;     (* Occurrences du 'in' a` venir *)
		  mu_type = ety (* Type pour le corps de l'expr definissante *)
	       } in
	      let (defs, links') = build_defs_and_links rem env in
	      (* Nouvelle def, mais environnement inchange' *)
	      ((def :: defs), ((id, evtelem) :: links'))
	      end
        | Typecore.Regular_ident _ -> assert false
;;



(* ************************
val debug_mu_sub : string -> mu_sub_problem -> unit
*)
(* ************************
Affiche le contenu et l'e'tat d'un sous-proble`me mu
*)
let debug_mu_sub title { mu_sub_level = level ; mu_sub_defs = defs } =
  let print_occs =
               (fun pp l ->
		 List.iter
		 (fun { Typecore.occ_level = lev ;
			Typecore.occ_type = v } ->
		      Printtypes.pp_ml_type pp v ;
                      Format.printf "@ level=%d@ " lev)
                 l) in
  Format.printf "%s : LEVEL=%d@.%a" title level
     (fun _pp l ->
       List.iter (fun { mu_scheme = sc ; mu_name = n ;
			mu_body_occs = boccs ; mu_type = ety ;
			mu_saturation = saturation ; mu_in_occs = ioccs } ->
                     Format.printf "  NAME: %s@.  SCHEME: %a@.  TY: %a@.  SATUR: %d@.  BOCCS: %a@.  IOCCS: %a@."
                      (Ident.name n) (Printtypes.pp_ml_type_scheme) sc
                      (Printtypes.pp_ml_type) ety
		      saturation
		      print_occs boccs print_occs !ioccs)
  l) defs
;;



(* ************************
val debug_mu_problem : string -> mu_problem -> unit
*)
(* ************************
Affiche le contenu et l'e'tat d'un proble`me mu
*)
let debug_mu_problem title (mu_problem : mu_problem) =
 List.iter (debug_mu_sub title) mu_problem
;;



(* ************************
val infer_expression :
  Typedtree.envt -> Scopedtree.expression -> Typedtree.expression
*)
(* ************************
Returns a typed expression.
*)
let rec infer_expression env expr =
 let (expr_desc_ty, expr_desc_exn, expr') =
   infer_expression_desc env expr.Scopedtree.pexp_desc in
 { Typedtree.exp_desc = expr' ;
   Typedtree.exp_loc = expr.Scopedtree.pexp_loc ;
   Typedtree.exp_type = expr_desc_ty ;
   Typedtree.exp_exn = expr_desc_exn }



(* ************************
val infer_let_definition :
  Typedtree.envt ->
  Asttypes.rec_flag ->
  (Scopedtree.pattern * Scopedtree.expression) list ->
  (Ident.t * Typecore.value_binding) list *
  (Typedtree.pattern * Typedtree.expression) list
*)
(* ************************
Cette fonction est charge'e d'infe'erer le type d'identificateurs lie's par
let et let rec. Elle fonctionne avec la re`gle mu. Elle renvoie une EXTENSION
a` rajouter a` l'environnement initial et non un nouvel environnement
global, ainsi que lq version type'e des 'bindings' (couples pattern, expr)
passe's en argument. ATTENTION, une fois cette fonction passe'e sur le
let (rec), les types infe're's ne sont pas encore corrects. Il faudra appeler
'solve_mu_problem' pour achever la recherche des sche'mas de types corrects.
On conside`re qu'on est en mode mu si on trouve un let rec quelque part dans
la phrase.
*)
and infer_let_definition env rec_flag bindings =
  (* Si la de'finition est re'cursive, alors il faut *)
  (* passer en mode mu pour tout le proble`me.       *)
  if rec_flag = Asttypes.Recursive then mu_flag := Mu_on ;
  (* On type les patterns, re'cupere les liens qu'ils induisent, leur   *)
  (* version type'e et le sche'ma de type donne' a` chacun d'entre eux. *)
  let (links, typed_patterns, pat_types) = Listextra.split3 (
           List.map (fun (pat, expr) ->
	         if is_nonexpansive env expr then
		   begin
                   Typecore.begin_definition () ;
	           let accu_ty = Typecore.type_variable () in
	           let (links, typed_pat) = infer_pattern accu_ty env pat in
                   Typecore.end_definition () ;
		   let links' =
		     List.map (fun (id, ty) ->
		                 (id, ty, true (* Ge'ne'ralisable *)))
			      links in
		   (links', typed_pat, accu_ty)
		   end
		 else
		   begin
	           let accu_ty = Typecore.type_variable () in
	           let (links, typed_pat) = infer_pattern accu_ty env pat in
		   let links' =
		     List.map (fun (id, ty) ->
		                 (id, ty, false (* Pas ge'ne'ralisable *)))
		              links in
		   (links', typed_pat, accu_ty)
		   end)
              bindings) in
  (* On construit un "sous-environnement" contenant ces liens sous forme *)
  (* de liens "Mu". Note: 'links' contient le type de chaque pattern,    *)
  (* associe' a` son nom.                                                *)
  let links = List.flatten links in
  let rec_env = List.fold_left
                   (fun evt (id, ty, gen) ->
		     let scheme =
                        (* USE copying generalization ! *)
		        if gen then Typecore.copying_generalization ty
		                  else Typecore.trivial_ml_type_scheme ty in
		     Envtype.add_value_rec id (scheme, ref []) evt)
                   (Envtype.empty_env ()) links in
  (* Environnement dans lequel on type les de'finitions. Si la de'finition *)
  (* est re'cursive (let rec) alors cet environnement contient un binding  *)
  (* pour les identificateurs lie's par ce let rec. Sinon, il ne les       *)
  (* contient pas. Ca e'vite les masquage dans le second cas et permet de  *)
  (* connaitre les identificateurs re'cursifs dans le premier cas.         *)
  let def_env = if rec_flag = Asttypes.Recursive
                 then Envtype.append rec_env env
                 else env in
  (* Maintenant, on type le corps des expressions de'finissantes    *)
  (* normalement et on re'cupe`re la liste des expressions type'es. *)
  let typed_exprs =
	 Listextra.map3 (fun _ (_, expr) pat_type ->
		    let can_gen = is_nonexpansive env expr in
                    (* Increase level if generalization is allowed *)
                    if can_gen then Typecore.begin_definition () ;
                    let typed_expr = infer_expression def_env expr in
		    (* Don't forget to take in account constraints provided *)
                    (* by the pattern *)
		    Typecore.unify_ml_type pat_type
		                          typed_expr.Typedtree.exp_type ;
                    (* Don't forget to reduce level if previously increased *)
                    if can_gen then Typecore.end_definition () ;
                    (* Return typed expression *)
		    typed_expr)
                  typed_patterns bindings pat_types in
   (* On switch maintenant selon le flag indiquant si on est en mode mu *)
   if !mu_flag = Mu_off then
     begin
     (* Aucune definition re'cursive au dessus de nous. Il faut simplement *)
     (* cre'er l'extension d'environnement finale. En effet, en absence de *)
     (* de'finitions re'cursives dans toute la phrase, il n'est pas besoin *)
     (* de la re`gle mu. L'extention finale d'environnement doit e^tre     *)
     (* faite sous forme de links (liste (ident, schema))                  *)

     (* Note: comme le type des patterns a e'te' unifie' avec le type de *)
     (* l'expression de'finissante, c,a nous permet de retrouver le type *)
     (* de l'expression de'finissante pour en faire un schema a` partir  *)
     (* du type du pattern. (Rappelons-nous que 'links'  contient le     *)
     (* type de chaque pattern,  associe' a` son nom.                    *)
     let final_links =
       List.map (fun (id, ty, gen) ->
 	           let scheme = if gen then Typecore.generalize_ml_type ty
	                        else Typecore.trivial_ml_type_scheme ty in
		   (id, Typecore.Regular_ident scheme))
		links in
     let bindings' = List.combine typed_patterns typed_exprs in
     (final_links, bindings')
     end
   else
     begin
     (* Il existe des de'finitions re'cursives au dessus de nous. On cre'e *)
     (* les sous-proble`mes induits par le let (rec) and. Le sche'ma de    *)
     (* de'part est celui infe're' apre`s Milner simple. 'mu_type' est le  *)
     (* type de l'expression de'finissante trouve' par Milner  simple (a`  *)
     (* ne pas confondre avec celui des occurrences).                      *)
     (* On re'cupe`re aussi l'extension d'environnement qui va bien, cad   *)
     (* dans laquelle les de'finitions ne ne'cessitant pas de re'solution  *)
     (* mu sont devenues des Regular.                                      *)
     let (defs, final_links) = build_defs_and_links links rec_env in
     (* On rajoute le sous-proble`me au proble`me mu global *)
     mu_problem := { mu_sub_level = Typecore.get_current_binding_level () ;
                     mu_sub_defs = defs } :: !mu_problem ;
     (* Et on rend la de'finition type'e. A ce niveau, le type est encore *)
     (* incomplet (trop ge'ne'ral). Il faudra penser a` re'soudre le      *)
     (* mu-proble`me ainsi emmagasine' une fois de retour au toplevel.    *)
     let bindings' = List.combine typed_patterns typed_exprs in
     (final_links, bindings')
     end




(* ************************
val apply_substraction :
  (Ident.t * Typecore.ml_type_expr) list list ->
  Typedtree.envt ->
  Typecore.ml_type_expr ->
  Typecore.ml_type_expr ->
  (Scopedtree.pattern * Scopedtree.expression) list ->
  Typedtree.expression list
*)
(* ************************
Take a type and a pattern. This type represent how many information succeeded
in arriving to the pattern. So we return bindings between this pattern and
this type. Next, we compute what information are absorbed by this pattern.
This generate a type, representing how many informations can fuse to the next
pattern we will see. As a side effect, the type of the body expressions are
unified into the 'accu_result_ty'. So that applying substraction also compute
the result-part of the function-bindings it is applied to.
*)
and apply_substraction early_bindings env accu_result_ty remaining_ty =
 function
 | [] -> []
 | (pat, body) :: rem ->
     (* Build environment with raw basic bindings for this pattern *)
     let env' = List.fold_left
         (fun evt (id, ty) -> Envtype.add_value id
                                     (Typecore.trivial_ml_type_scheme ty) evt)
        env (List.hd early_bindings) in
     (* Try to find finer bindings with substraction for this pattern *)
     let (links, new_remaining_ty) =
              Substract.substract_ml_type env' remaining_ty pat in
     (* Build the environment with finer bindings. Better found bindings *)
     (* will overlap old and less precise ones.                          *)
     let env'' = List.fold_left
         (fun evt (id, ty) -> Envtype.add_value id
                                     (Typecore.trivial_ml_type_scheme ty) evt)
        env' links in
     (* Now type body in <extended environment> *)
     let body' = infer_expression env'' body in
     (* Unify body type with result type so that all *)
     (* bodies will have the same type               *)
     Typecore.unify_ml_type accu_result_ty body'.Typedtree.exp_type ;
     (* If pattern is guarded, we <MUST NOT> use the result of substraction *)
     (* So in this case, the remaining type is the original type we got     *)
     (* when we enter here...                                               *)
     let new_remaining_ty = (match body.Scopedtree.pexp_desc with
                             | Scopedtree.Pexp_when (_, _) -> remaining_ty
			     | _ -> new_remaining_ty) in
     (* Return the typed body *)
     body' :: (apply_substraction (List.tl early_bindings) env accu_result_ty
                                  new_remaining_ty rem)



(* ************************
val infer_expression_desc :
  Typedtree.envt ->
  Scopedtree.expression_desc ->
  Typecore.ml_type_expr * Typecore.phi_expr * Typedtree.expression_desc
*)
(* ************************
Return a type and an a typed expression desc
*)
and infer_expression_desc env = function
  | Scopedtree.Pexp_ident path ->
      let ty = Envtype.find_value path env (* Specialization is done *) in
      let expr_desc' = Typedtree.Texp_ident path in
      (ty, Typecore.empty_phi (), expr_desc')
  | Scopedtree.Pexp_constant constant ->
      let ty = infer_constant constant in
      (ty, Typecore.empty_phi (), Typedtree.Texp_constant constant)
  | Scopedtree.Pexp_let (rec_flag, bindings, expr) ->
      (* Don't increase level, this will be done in definition inference *)
      let (links, bindings') = infer_let_definition env rec_flag bindings in
      (* Let's build the environment for typing the ending expression *)
      let env' = List.fold_left (fun evt (id, vbind) ->
                                    Envtype.add_value_binding id vbind evt)
                                env links in
      let expr' = infer_expression env' expr in
      (* Uncaught exception collecting *)
      let excs =  expr'.Typedtree.exp_exn in
      List.iter (fun (_, e) ->
                      Typecore.unify_phi_type excs e.Typedtree.exp_exn)
                bindings' ;
      (* Now reconstruct an expression description for ending-in expression *)
      let expr_desc' = Typedtree.Texp_let (rec_flag, bindings', expr') in
      (expr'.Typedtree.exp_type, excs, expr_desc')
  | Scopedtree.Pexp_function bindings ->
      let argument_ty = Typecore.type_variable () in
      (* First, type patterns. *)
      let (early_bindings, typed_pats) = List.split (List.map
	(fun (pat, _) -> infer_pattern argument_ty env pat) bindings) in
      (* Let's create the type where we'll accumulate            *)
      (* constraints for the result par of the functionnal type. *)
      let result_ty = Typecore.type_variable () in
      (* Now we will type bodies in an extended env containing *)
      (* bindings once substraction happened.                  *)
      let typed_bodies =
	apply_substraction early_bindings env result_ty argument_ty bindings in
      (* Let's collect uncaught exceptions *)
      let excs = Typecore.empty_phi () in
      List.iter (fun e -> Typecore.unify_phi_type excs e.Typedtree.exp_exn)
                typed_bodies ;
      (* Re-construct function-bindings once typed *)
      let bindings' = List.combine typed_pats typed_bodies in
      (* And synthetize final type. Exceptions raised in the *)
      (* body are put as effect in the functional type !     *)
      let fun_type = Typecore.type_arrow argument_ty result_ty excs in
      (* A function definition never raise exceptions *)
      (fun_type, Typecore.empty_phi (), Typedtree.Texp_function bindings')
  | Scopedtree.Pexp_apply (expr, exprs) ->
      let typed_expr = infer_expression env expr in
      let typed_exprs = List.map (infer_expression env) exprs in
      let excs = Typecore.empty_phi () in
      let final_ty =
	 List.fold_left
           (fun functionnal_ty arg_expr ->
	      (* Type of the current argument in the expressions list *)
              let arg_ty = arg_expr.Typedtree.exp_type in
	      (* Type returned after this application step *)
	      let result_ty = Typecore.type_variable () in
	      (* Temporary functionnal type to unify with the type *)
	      (* of the current applicator. By the way, accumulate *)
              (* uncaught exceptions comming from the effect of    *)
              (* the functions.                                    *)
	      let fun_ty = Typecore.type_arrow arg_ty result_ty excs in
	      Typecore.unify_ml_type fun_ty functionnal_ty ;
	      (* Result is the positive part of the arrow *)
	      result_ty)
           typed_expr.Typedtree.exp_type typed_exprs in
      (* Let's collect uncaught exceptions from applicator expression   *)
      (* and arguments expressions for this application. Note that fun  *)
      (* effect was taken in account by side effect just above.         *)
      (* So we only accumulate constraints with those found previously. *)
      Typecore.unify_phi_type excs typed_expr.Typedtree.exp_exn ;
      List.iter (fun e -> Typecore.unify_phi_type excs e.Typedtree.exp_exn)
		 typed_exprs ;
      (* And now the final result... *)
      (final_ty, excs, Typedtree.Texp_apply (typed_expr, typed_exprs))
  | Scopedtree.Pexp_match (expr, bindings) ->
      let typed_expr = infer_expression env expr in
      let matched_ty = typed_expr.Typedtree.exp_type in
      (* First, type patterns and basic bindings for the patterns. *)
      let (early_bindings, typed_pats) = List.split (List.map
	  (fun (pat, _) -> infer_pattern matched_ty env pat) bindings) in
      (* Let's create type where we'll accumulate constraints for the result *)
      let result_ty = Typecore.type_variable () in
      (* Now we will type bodies in an extended env containing *)
      (* bindings once substraction happened.                  *)
      let typed_bodies =
	 apply_substraction early_bindings env result_ty matched_ty bindings in
      (* Re-construct match-bindings once typed *)
      let bindings' = List.combine typed_pats typed_bodies in
      (* Let'collect exceptions *)
      let excs = typed_expr.Typedtree.exp_exn in
      List.iter (fun e -> Typecore.unify_phi_type excs e.Typedtree.exp_exn)
                typed_bodies ;
      (result_ty, excs, Typedtree.Texp_match (typed_expr, bindings'))
  | Scopedtree.Pexp_try (expr, bindings) ->
      let typed_expr = infer_expression env expr in
      (* Type exn containing exceptions raised by tried expression *)
      let exn_ty = Typecore.caught_exception (typed_expr.Typedtree.exp_exn) in
      (* First, type patterns. *)
      let (early_bindings, typed_pats) = List.split (List.map
	(fun (pat, _) -> infer_pattern exn_ty env pat) bindings) in
      (* Let's create type where we'll accumulate constraints for the result *)
      let result_ty = typed_expr.Typedtree.exp_type in
      (* Now we will type bodies in an extended env containing *)
      (* bindings once substraction happened.                  *)
      let typed_bodies =
	     apply_substraction early_bindings env result_ty exn_ty bindings in
      (* Re-construct try-bindings once typed *)
      let bindings' = List.combine typed_pats typed_bodies in
      (* Exception values accumulator. Used to accumulate uncaught *)
      (* exceptions from the handlers.                             *)
      let excs = Typecore.empty_phi () in
      (* Let's collect re-raised exceptions in the right part of handlers *)
      List.iter (fun e -> Typecore.unify_phi_type excs e.Typedtree.exp_exn)
                typed_bodies ;
      (result_ty, excs, Typedtree.Texp_try (typed_expr, bindings'))
  | Scopedtree.Pexp_tuple exprs ->
      let typed_exprs = List.map (infer_expression env) exprs in
      let tys = List.map (fun e -> e.Typedtree.exp_type) typed_exprs in
      let excs = Typecore.empty_phi () in
      List.iter (fun e -> Typecore.unify_phi_type excs e.Typedtree.exp_exn)
                typed_exprs ;
      (Typecore.type_tuple tys, excs, Typedtree.Texp_tuple typed_exprs)
  | Scopedtree.Pexp_construct (path, expr_opt) ->
      let cstr_decl = Envtype.find_constructor path env in
      begin
      match (expr_opt, cstr_decl.Typedtree.cstr_arity) with
       | (None, Typedtree.Zeroary) ->
	   let result_ty =
	      Typecore.specialize_ml_type cstr_decl.Typedtree.cstr_scheme in
	   (* If no args to constructor, then no exceptions raised ! *)
	   (result_ty, Typecore.empty_phi (),
	    Typedtree.Texp_construct (path, None))
       | (Some expr, Typedtree.Unary) ->
	   (* Constructor must be view as a function *)
	   let typed_expr = infer_expression env expr in
	   let cstr_ty =
	     Typecore.specialize_ml_type cstr_decl.Typedtree.cstr_scheme in
	   let result_ty = Typecore.type_variable () in
	   let arg_ty = typed_expr.Typedtree.exp_type in
	   (* Simulation of an application *)
	   Typecore.unify_ml_type cstr_ty
                            (Typecore.type_arrow arg_ty result_ty
                                                 (Typecore.empty_phi ())) ;
	   (* Let's collect uncaught exceptions *)
	   let excs = typed_expr.Typedtree.exp_exn in
	   (result_ty, excs,
	    Typedtree.Texp_construct (path, Some typed_expr))
       | (_, _) -> raise (Constructor_arity_error path)
      end
  | Scopedtree.Pexp_record (fields, expr_opt) ->
      (* At then end, must be the type of the host of all these labels *)
      let result_ty = Typecore.type_variable () in
      let excs = Typecore.empty_phi () in
      (* Typecheck the "with" construct *)
      let typed_expr_opt =
	(match expr_opt with None -> None
	 | Some e ->
	     let e' = infer_expression env e in
	   (* Don't forget exceptions *)
	     Typecore.unify_phi_type excs e'.Typedtree.exp_exn ;
	     Some e') in
      let fld_len = List.length fields in
      (* Parse each field and accumulate exception by side effect *)
      let fields' = List.map
	  (fun (label_path, expr) ->
	         let typed_expr = infer_expression env expr in
		 let lbl_descr = Envtype.find_label label_path env in
		 (* Functionnal type for this field *)
		 let field_ty = Typecore.specialize_ml_type
		                       lbl_descr.Typedtree.fld_scheme in

		 (* Let's unify in result type by side effect *)
		 Typecore.unify_ml_type (Typecore.type_arrow
                                           typed_expr.Typedtree.exp_type
                                           result_ty
					   (Typecore.empty_phi ()))
					 field_ty ;
		 (* Test for wrong # of fields *)
		 if lbl_descr.Typedtree.fld_countall <> fld_len then
		   raise Bad_labels_number ;
                 (* Accumulate exceptions *)
		 Typecore.unify_phi_type excs typed_expr.Typedtree.exp_exn ;
		 (label_path, typed_expr))
	       fields in
      (result_ty, excs, Typedtree.Texp_record (fields', typed_expr_opt))
  | Scopedtree.Pexp_field (expr, label_path) ->
      let typed_expr = infer_expression env expr in
      let label_desc = Envtype.find_label label_path env in
      let label_ty = Typecore.specialize_ml_type
                                  label_desc.Typedtree.fld_scheme in
      let result_ty = Typecore.type_variable () in
      Typecore.unify_ml_type
             (Typecore.type_arrow result_ty
                                  typed_expr.Typedtree.exp_type
                                  (Typecore.empty_phi ()))
             label_ty ;
      let excs = typed_expr.Typedtree.exp_exn in
      (result_ty, excs, Typedtree.Texp_field (typed_expr, label_path))
  | Scopedtree.Pexp_setfield (expr0, label_path, expr1) ->
      let typed_expr0 = infer_expression env expr0 in
      let typed_expr1 = infer_expression env expr1 in
      let lbl_desc = Envtype.find_label label_path env in
      (* Check for mutability to enable modification *)
      if lbl_desc.Typedtree.fld_mut <> Asttypes.Mutable
	then raise (Field_not_mutable label_path) ;
      let lbl_ty = Typecore.specialize_ml_type
                                  lbl_desc.Typedtree.fld_scheme in
      Typecore.unify_ml_type lbl_ty
		    (Typecore.type_arrow typed_expr1.Typedtree.exp_type
                                         typed_expr0.Typedtree.exp_type
                                         (Typecore.empty_phi ())) ;
      let result_ty = Typecore.type_unit () in
      let excs = typed_expr0.Typedtree.exp_exn in
      Typecore.unify_phi_type excs typed_expr1.Typedtree.exp_exn ;
      (result_ty, excs,
       Typedtree.Texp_setfield (typed_expr0, label_path, typed_expr1))
  | Scopedtree.Pexp_array exprs ->
      let typed_exprs = List.map (infer_expression env) exprs in
      let elem_ty = Typecore.type_variable () in
      (* Unify all elements type *)
      List.iter (fun e -> Typecore.unify_ml_type elem_ty e.Typedtree.exp_type)
                 typed_exprs ;
      let result_ty = Typecore.type_array elem_ty in
      let excs = Typecore.empty_phi () in
      List.iter (fun e -> Typecore.unify_phi_type excs e.Typedtree.exp_exn)
                typed_exprs ;
      (result_ty, excs, Typedtree.Texp_array typed_exprs)
  | Scopedtree.Pexp_ifthenelse (expr0, expr1, expr2_opt) ->
      let typed_expr0 = infer_expression env expr0 in
      (* Unify condition expression type with bool *)
      let type_cond = Typecore.type_bool_unknown () in
      Typecore.unify_ml_type type_cond typed_expr0.Typedtree.exp_type ;
      (* Type then-part *)
      let typed_expr1 = infer_expression env expr1 in
      (* Let's begin to collect exception *)
      let excs = typed_expr0.Typedtree.exp_exn in
      Typecore.unify_phi_type excs typed_expr1.Typedtree.exp_exn ;
      begin
      match expr2_opt with
        | None ->
	    (* Unit if *)
	    let result_ty = Typecore.type_unit () in
	    Typecore.unify_ml_type result_ty typed_expr1.Typedtree.exp_type ;
	    (result_ty, excs,
	     Typedtree.Texp_ifthenelse (typed_expr0, typed_expr1, None))
        | Some expr2 ->
	    (* Two sided if *)
	    let typed_expr2 = infer_expression env expr2 in
            let result_ty = Typecore.type_variable () in
	    Typecore.unify_ml_type result_ty typed_expr1.Typedtree.exp_type ;
	    Typecore.unify_ml_type result_ty typed_expr2.Typedtree.exp_type ;
	    (* Let's finish uncaught exceptions collecting *)
	    Typecore.unify_phi_type excs typed_expr2.Typedtree.exp_exn ;
	    (result_ty, excs,
	     Typedtree.Texp_ifthenelse (typed_expr0,
					typed_expr1, Some (typed_expr2)))
      end
  | Scopedtree.Pexp_sequence (expr0, expr1) ->
      let typed_expr0 = infer_expression env expr0 in
      let typed_expr1 = infer_expression env expr1 in
      (* Exception collecting... *)
      let excs = typed_expr0.Typedtree.exp_exn in
      Typecore.unify_phi_type excs typed_expr1.Typedtree.exp_exn ;
      let result_ty = typed_expr1.Typedtree.exp_type in
      (result_ty, excs, Typedtree.Texp_sequence (typed_expr0, typed_expr1))
  | Scopedtree.Pexp_while (expr0, expr1) ->
      let typed_expr0 = infer_expression env expr0 in
      let type_cond = Typecore.type_bool_unknown () in
      Typecore.unify_ml_type type_cond typed_expr0.Typedtree.exp_type ;
      let typed_expr1 = infer_expression env expr1 in
      (* Result type is always unit *)
      let result_ty = Typecore.type_unit () in
      (* Exception collecting... *)
      let excs = typed_expr0.Typedtree.exp_exn in
      Typecore.unify_phi_type excs typed_expr1.Typedtree.exp_exn ;
      (result_ty, excs,
       Typedtree.Texp_while (typed_expr0, typed_expr1))
  | Scopedtree.Pexp_for (index, expr0, expr1, dir_flag, expr2) ->
      let typed_expr0 = infer_expression env expr0 in
      let typed_expr1 = infer_expression env expr1 in
      (* Unify index bounds together and with int type. *)
      Typecore.unify_ml_type (Typecore.type_int_unknown ())
                             typed_expr0.Typedtree.exp_type ;
      Typecore.unify_ml_type typed_expr0.Typedtree.exp_type
                             typed_expr1.Typedtree.exp_type ;
      (* Type loop body in environment extended with index. *)
      let env' =
	Envtype.add_value index
              (Typecore.trivial_ml_type_scheme typed_expr0.Typedtree.exp_type)
	      env in
      let typed_expr2 = infer_expression env' expr2 in
      (* Uncaught exceptions collecting. *)
      let excs = typed_expr0.Typedtree.exp_exn in
      Typecore.unify_phi_type excs typed_expr1.Typedtree.exp_exn ;
      Typecore.unify_phi_type excs typed_expr2.Typedtree.exp_exn ;
      (* Result type is always unit. *)
      let result_ty = Typecore.type_unit () in
      (result_ty, excs,
       Typedtree.Texp_for (index, typed_expr0, typed_expr1,
			   dir_flag, typed_expr2))
  | Scopedtree.Pexp_constraint (expr, constraint1, constraint2) ->
      begin
      match (constraint1, constraint2) with
       | (Some ct1, None) ->
          let typed_expr = infer_expression env expr in
          let constraint_ty1 = core_type_to_ml_type_expr env ct1 in
	  Typecore.unify_ml_type constraint_ty1 typed_expr.Typedtree.exp_type ;
          let excs = typed_expr.Typedtree.exp_exn in
	  let result_ty = constraint_ty1 in
	  (result_ty, excs,
	   Typedtree.Texp_constraint (typed_expr, constraint1, constraint2))
       | (_, _) ->
           (* We don't yet handle substyping constraints. *)
	   failwith "SUBTYPING NOT IMPLEMENTED"
      end
  | Scopedtree.Pexp_when (expr0, expr1) ->
      let typed_expr0 = infer_expression env expr0 in
      let type_cond = Typecore.type_bool_unknown () in
      Typecore.unify_ml_type type_cond typed_expr0.Typedtree.exp_type ;
      let typed_expr1 = infer_expression env expr1 in
      let result_ty = typed_expr1.Typedtree.exp_type in
      (* Exception collecting... *)
      let excs = typed_expr0.Typedtree.exp_exn in
      Typecore.unify_phi_type excs typed_expr1.Typedtree.exp_exn ;
      (result_ty, excs, Typedtree.Texp_when (typed_expr0, typed_expr1))
  | Scopedtree.Pexp_letmodule (mod_ident, mod_expr, expr) ->
      let (typed_module_expr, shadow) = 
	!infer_module_expr_forward env mod_expr in
      (* On rajoute les modules cachés dus à l'expansion *)
      let env' = List.fold_left (fun envaccu (id, mexpr) ->
	                Envtype.add_module id mexpr.Typedtree.mod_type envaccu)
	                env shadow in
      (* On rajoute le module lui-même *)
      let env'' = Envtype.add_module mod_ident
	                 typed_module_expr.Typedtree.mod_type env' in
      let typed_expr = infer_expression env'' expr in
      let result_expr =
	Typedtree.Texp_letmodule (mod_ident, typed_module_expr, typed_expr) in
      (* Exceptions de l'expression 'in' *)
      let excs = typed_expr.Typedtree.exp_exn in
      (* Exceptions du module. On les accumule directement dans 'excs' *)
      !find_module_expr_exception_forward excs typed_module_expr ;
      (typed_expr.Typedtree.exp_type, excs, result_expr)
;;



(* ************************
val infer_type_declaration:
  Typedtree.envt ->
  Scopedtree.type_declaration ->
  (Ident.t * Typecore.ml_type_expr * Typedtree.constructor_arity) list *
  (Ident.t * Asttypes.mutable_flag * Typecore.ml_type_expr * int) list *
  (Typecore.ml_type_expr option * Typecore.ml_type_expr list)
*)
(* ************************
Retuns a list of (Ident.t * constructor_description), a list of
(Ident.t * Typedtree.label_description) and the (ident, declaration) because
the declaration can have been modified (expanded) somewhere. We assume that
the ident for this type is already in the environment, in a fake way of
course. This to enable recursive type definitions.
*)
(* Pas de begin/end definition ici. On est déjà au niveau +1 *)
let infer_type_declaration env ty_decl =
  match ty_decl.Scopedtree.ptype_kind with
   | Scopedtree.Ptype_abstract ->
      begin
      new_mapping Fixed ty_decl.Scopedtree.ptype_params ;
      (* Gestion des contraintes *)
      List.iter (fun (ct1, ct2, _) ->
	           let t1 = core_type_to_ml_type_expr env ct1 in
		   let t2 = core_type_to_ml_type_expr env ct2 in
		   Typecore.unify_ml_type t1 t2)
	ty_decl.Scopedtree.ptype_cstrs ;
      let ty_params = List.map snd (current_vars_mapping ()).vm_ml in
      match ty_decl.Scopedtree.ptype_manifest with
       | None ->
	   release_vars_mapping () ;
	   ([(*No constructors*)], [(*No labels*)], (None, ty_params))
       | Some core_ty ->
	   (* Le type est en fait une abbréviation. *)
           (* On récupère le type représentant l'abbréviation *)
	   let ty = core_type_to_ml_type_expr env core_ty in
	   let decl = Envtype.find_type !(ty_decl.Scopedtree.ptype_path) env in
           (* Maintenant, il faut penser à unifier ce à quoi est abbrégé *)
           (* le type et une instance du schéma temporaire qu'on lui a donné *)
	   let (ty_rec, tys_rec) = (match decl.Typedtree.type_manifest with
	       | None -> assert false
	       | Some s -> Typecore.specialize_ml_type2 s
	                                 decl.Typedtree.type_params) in
	   List.iter2 Typecore.unify_ml_type tys_rec ty_params ;
	   Typecore.unify_ml_type ty_rec ty ;
	   release_vars_mapping () ;
	   ([(*No constructors*)], [(*No labels*)], (Some ty, ty_params))
      end
   | Scopedtree.Ptype_variant constructors ->
       (* C'est une définition de type somme *)
       new_mapping Fixed ty_decl.Scopedtree.ptype_params ;
      (* Gestion des contraintes *)
       List.iter (fun (ct1, ct2, _) ->
	           let t1 = core_type_to_ml_type_expr env ct1 in
		   let t2 = core_type_to_ml_type_expr env ct2 in
		   Typecore.unify_ml_type t1 t2)
	ty_decl.Scopedtree.ptype_cstrs ;
       let ty_params = List.map snd (current_vars_mapping ()).vm_ml in
       (* ^^ Gestion sans vérification des type somme manifestes ^^ *)
       (* Récupère la declaration de type dans laquelle on a remplacé *)
       (* le path original par celui auquel le type somme est dit égal *)
       (* Récupère aussi l'expression de type auquel ce type somme est *)
       (* égal.                                                        *)
       let (ty_decl, equiv_ty) = 
	 begin
	 match ty_decl.Scopedtree.ptype_manifest with
	  | None -> (ty_decl, None)
	  | Some core_ty ->
	      let ty = Typecore.ml_type_repr
		            (core_type_to_ml_type_expr env core_ty) in
	      let equiv_path = (match ty.Typecore.mte_desc with
	        | Typecore.Tconstr (p, _, _) -> p
	        | _ -> raise Invalid_equiv_in_variant_type) in
	      ({ ty_decl with Scopedtree.ptype_path = equiv_path }, Some ty)
	 end in
       (* ^^^ *)
       let make_constructors (cstr_id, arg_core_tys) =
          (* Crée les types des arguments du constructeur *)
	  let arg_tys =
	       List.map (core_type_to_ml_type_expr env) arg_core_tys in
          (* Transforme la liste des arguments en tuple si nécessaire *)
          let cstr_opt_args = (match arg_tys with
                    | [] -> None
                    | [ only_one ] -> Some only_one
	            | several -> Some (Typecore.type_tuple several)) in
          (* Maintenant construit le type du constructeur. Ce type est *)
          (* fonctionnel si le constructeur a des arguments, sinon il  *)
          (* ne l'est pas.                                             *)
          let cstr_ty =
	       begin
	       match cstr_opt_args with
                | None ->
	           let appr = Typecore.constant_phi_pre (Ident.name cstr_id) in
	           Typecore.type_constr ty_decl.Scopedtree.ptype_path
                                        ty_params appr
                | Some args ->
		   let appr = Typecore.param_phi (Ident.name cstr_id) args in
	           Typecore.type_arrow args (Typecore.type_constr
                                                ty_decl.Scopedtree.ptype_path
                                                ty_params
					     appr)
                                       (Typecore.empty_phi ())
	       end in
          (* Trouve l'arité du constructeur *)
          let arity = (match arg_tys with [] -> Typedtree.Zeroary
                        | _ -> Typedtree.Unary) in
          (cstr_id, cstr_ty, arity) in
       (* Just do it *)
       let cstr_infos = List.map make_constructors constructors in
       release_vars_mapping () ;
       (cstr_infos, [(*No labels*)], (equiv_ty, ty_params))
  | Scopedtree.Ptype_record labels ->
      (* First, we sort the label list in order to get a canonical *)
      (* representation of a record.                               *)
      let labels = Sort.list (fun (n1, _, _) (n2, _, _) -> n1 <= n2) labels in
      new_mapping Fixed ty_decl.Scopedtree.ptype_params ;
      (* Gestion des contraintes *)
      List.iter (fun (ct1, ct2, _) ->
	           let t1 = core_type_to_ml_type_expr env ct1 in
		   let t2 = core_type_to_ml_type_expr env ct2 in
		   Typecore.unify_ml_type t1 t2)
	ty_decl.Scopedtree.ptype_cstrs ;
      let ty_params = List.map snd (current_vars_mapping ()).vm_ml in
      let nb_labels = List.length labels in

      let label_args_types = List.map
          (fun (_, _, cty) -> core_type_to_ml_type_expr env cty) labels in

      (* Let's build the type term host of this label. *)
      let ty_record =
         let fld_lst = List.map2 (fun (id, _, _) ty -> (Ident.name id, ty))
                                 labels label_args_types in
	 let rec_approx = Typecore.record_phi fld_lst in
	 Typecore.type_constr ty_decl.Scopedtree.ptype_path ty_params
		              rec_approx in
      (* Now we have the real type (with approx) of the record, we can *)
      (* build each label functionnal type.                            *)
      let make_labels (label_id, mut_flag, _) lbl_arg_ty =
	 let ty = Typecore.type_arrow lbl_arg_ty ty_record
	                              (Typecore.empty_phi ()) in
	 (label_id, mut_flag, ty, nb_labels) in
      (* Just do it *)
      let label_infos = List.map2 make_labels labels label_args_types in
      release_vars_mapping () ;
      ([(*No constructors*)], label_infos, (None, ty_params))
;;




(* ************************************************************************* *)
(* Primitive inference stuff.                                                *)



(* Exception utilisée lorsque l'utilisateur a défini une primitive avec  *)
(* une abbreviation. On préfère que les expansions de type soient faites *)
(* à la main par lui.                                                    *)
exception Expansion_required_in_primitive of Path.t ;;


let rec annoted_core_type_to_ml_type_expr ml_vars_mapping row_vars_mapping
                                          pre_vars_mapping env ann_core_ty =
 match ann_core_ty.Scopedtree.patyp_desc with
  | Scopedtree.Patyp_var varname ->
      (try List.assoc varname ml_vars_mapping with
       Not_found -> raise (Unbound_type_variable varname))
  | Scopedtree.Patyp_arrow (neg, effect, pos) ->
      let neg' = annoted_core_type_to_ml_type_expr ml_vars_mapping
                                row_vars_mapping pre_vars_mapping env neg in
      let pos' = annoted_core_type_to_ml_type_expr ml_vars_mapping
                                row_vars_mapping pre_vars_mapping env pos in
      let effect' = annotation_expr_to_phi ml_vars_mapping
                                row_vars_mapping pre_vars_mapping env effect in
      Typecore.type_arrow neg' pos' effect'
  | Scopedtree.Patyp_tuple args ->
      let args' =
	List.map (annoted_core_type_to_ml_type_expr ml_vars_mapping
                                    row_vars_mapping pre_vars_mapping env)
                 args in
      Typecore.type_tuple args'
  | Scopedtree.Patyp_constr (path, args, annot) ->
      let args' =
	List.map (annoted_core_type_to_ml_type_expr ml_vars_mapping
                                    row_vars_mapping pre_vars_mapping env)
                  args in
      let annot' = annotation_expr_to_phi ml_vars_mapping
                                row_vars_mapping pre_vars_mapping env annot in
      let decl = Envtype.find_type !path env in
      if List.length args <> decl.Typedtree.type_arity then
	raise (Type_arity_error !path) ;
      begin
      match decl.Typedtree.type_manifest with
       | None -> Typecore.type_constr decl.Typedtree.type_path args' annot'
       | Some _ -> raise (Expansion_required_in_primitive !path)
      end



and annotation_expr_to_phi ml_vars_mapping row_vars_mapping
                           pre_vars_mapping env = function
  | Scopedtree.Pann_bottom ->
      { Typecore.phi_value = Typecore.Pbottom ;
	Typecore.phi_print = false ; Typecore.phi_empty = true }
  | Scopedtree.Pann_explicit (annotation_elem_exprs, vname) ->
      let phis =
	List.map (annotation_elem_expr_to_phi_elem ml_vars_mapping
		                 row_vars_mapping pre_vars_mapping env) 
                 annotation_elem_exprs in
      let rowvar = (try List.assoc vname row_vars_mapping with
                    Not_found -> assert false) in
      { Typecore.phi_value = Typecore.Pexplicit (phis, rowvar) ;
	Typecore.phi_print = false ; Typecore.phi_empty = true }


and annotation_elem_expr_to_phi_elem ml_vars_mapping row_vars_mapping
                                   pre_vars_mapping env = function
  | Scopedtree.Pelem_constant (name, presence) ->
      let pr = presence_expr_to_simple_presence pre_vars_mapping
                 presence in
      Typecore.Constant (name, pr)
  | Scopedtree.Pelem_param (name, ann_type_expr) ->
      let arg = annoted_core_type_to_ml_type_expr
                                          ml_vars_mapping row_vars_mapping
                                          pre_vars_mapping env ann_type_expr in
      Typecore.Param (name, arg)
  | Scopedtree.Pelem_record fields ->
      let fields' =
	List.map
	   (fun (n, ann_ty) -> (n, annoted_core_type_to_ml_type_expr
			                  ml_vars_mapping row_vars_mapping
                                          pre_vars_mapping env ann_ty))
	   fields in
      Typecore.Record fields'



and presence_expr_to_simple_presence pre_vars_mapping = function
  | Scopedtree.Prexpr_pre -> Typecore.presence_present ()
  | Scopedtree.Prexpr_var varname ->
      (try List.assoc varname pre_vars_mapping with
       Not_found -> assert false)
;;

 

(* infer_primitive: Envtype.t -> Scopedtree.annoted_value_description ->  *)
(*                                          Typecore.ml_type_expr         *)
(* Infer a type for an external value declaration, and return it. *)
let infer_primitive env ann_val_desc =
  (* We must find all variable names used because primitives syntax *)
  (* does not provide them to us...                                 *)
  let ml_vars_mapping = ref [] in
  let row_vars_mapping = ref [] in
  let pre_vars_mapping = ref [] in

  let rec search ct =
    match ct.Scopedtree.patyp_desc with
     | Scopedtree.Patyp_var name ->
	  (try ignore (List.assoc name !ml_vars_mapping) with Not_found ->
             ml_vars_mapping := (name, Typecore.type_variable ())
			      :: !ml_vars_mapping)
     | Scopedtree.Patyp_arrow (ct0, ann, ct1) ->
	 search ct0 ; search_annotation ann ; search ct1
     | Scopedtree.Patyp_tuple args -> List.iter search args
     | Scopedtree.Patyp_constr (_, args, ann) ->
	 search_annotation ann ;
	 List.iter search args

  and search_annotation = function
    | Scopedtree.Pann_bottom -> ()
    | Scopedtree.Pann_explicit (ann_elem_exprs, name) ->
       List.iter (function Scopedtree.Pelem_param (_, ty) -> search ty
                    | Scopedtree.Pelem_constant (_, pr)  -> search_presence pr
		    | Scopedtree.Pelem_record fields ->
		         List.iter (fun (_, ty) -> search ty) fields)
                ann_elem_exprs ;
       (try ignore (List.assoc name !row_vars_mapping) with  Not_found ->
	  row_vars_mapping := (name, Typecore.row_variable ())
			      :: !row_vars_mapping)

  and search_presence = function
  | Scopedtree.Prexpr_pre -> ()
  | Scopedtree.Prexpr_var name ->
      (try ignore (List.assoc name !pre_vars_mapping) with  Not_found ->
	  pre_vars_mapping := (name, Typecore.presence_variable ())
			      :: !pre_vars_mapping) in

  (* On doit commencer par récupérer les noms variables qui servent *)
  (* en tant "qu'explanation" car celles-ci doivent être incluses   *)
  (* dans le mapping, MAIS NON generalisées ! Ensuite, quand on     *)
  (* créera le type, on unifiera ces variables avec ce à quoi elles *)
  (* sont égales dans "l'explanation".                              *)
  List.iter (fun (n, _) ->
               let v = Typecore.type_variable () in
               ml_vars_mapping := (n, v) :: !ml_vars_mapping)
            ann_val_desc.Scopedtree.paval_explaination ;
  (* We need to increase level in order to generalize type of the external *)
  Typecore.begin_definition () ;
  (* Build the variable name mapping by side effect *)
  search ann_val_desc.Scopedtree.paval_type ;
  List.iter (fun (_, act) -> search act)
            ann_val_desc.Scopedtree.paval_explaination ;
  (* Maintenant, on construit le type représenté par l'expression *)
  let ty = annoted_core_type_to_ml_type_expr
                                     !ml_vars_mapping
                                     !row_vars_mapping
                                     !pre_vars_mapping
	                             env
                                     ann_val_desc.Scopedtree.paval_type in
  (* Maintenant, on construit chacun des types de "l'explanation" *)
  let expl_tys = List.map
                   (fun (n, act) ->
		     let ty = annoted_core_type_to_ml_type_expr
                                     !ml_vars_mapping
                                     !row_vars_mapping
                                     !pre_vars_mapping
	                             env
			             act in
		     (n, ty))
                   ann_val_desc.Scopedtree.paval_explaination in
  (* Maintenant, on unifie les variables créées initialement avec ces types *)
  List.iter (fun (vname, ty) ->
               let v = List.assoc vname !ml_vars_mapping in
               Typecore.unify_ml_type v ty)
            expl_tys ;
  Typecore.end_definition () ;
  ty
;;




(* ************************************************************************* *)
(* Exception declaration inference stuff.                                    *)





(* infer_exception: Envtype.t -> Scopedtree.core_type option ->              *)
(*           Typedtree.constructor_description *)
(*
On se charge ici des begin/end_definitions
*)
let infer_exception env exn_name cty_opt =
 let approx_name = !Files.current_comp_unit ^ "__" ^ (Ident.name exn_name) ^
                   (string_of_int (Ident.stamp exn_name)) in
 match cty_opt with
  | None ->
      Typecore.begin_definition () ;
      let ty = Typecore.type_exception approx_name None in
      Typecore.end_definition () ;
      let sc = Typecore.generalize_ml_type ty in
      { Typedtree.cstr_kind = Typedtree.Exn !Files.current_comp_unit ;
	Typedtree.cstr_arity = Typedtree.Zeroary ;
        Typedtree.cstr_scheme = sc }
  | Some core_ty ->
      Typecore.begin_definition () ;
      new_mapping Fixed [] ;
      let ty_arg = core_type_to_ml_type_expr env core_ty in
      let ty_cstr = Typecore.type_arrow
                           ty_arg
                           (Typecore.type_exception approx_name (Some ty_arg))
                           (Typecore.empty_phi ()) in
      Typecore.end_definition () ;
      release_vars_mapping () ;
      let sc = Typecore.generalize_ml_type ty_cstr in
      { Typedtree.cstr_kind = Typedtree.Exn !Files.current_comp_unit ;
	 Typedtree.cstr_arity = Typedtree.Unary ;
	 Typedtree.cstr_scheme = sc }
;;



(* ************************************************************************* *)
(* For mu rule.                                                              *)




(* ************************
val find_rec_scheme_step : mu_sub_problem list -> mu_sub_problem list
*)
(* ************************
Cette fonction se charge de faire une e'tape d'ite'ration de recherche de
sche'mas corrects pour un mu proble`me donne' dans 'mu_problem'
Elle prend donc en entr'e un mu proble`me et rend en sortie un nouveau
mu proble`me. Elle ne retire aucun sous-proble`me du proble`me initial.
Autrement dit, elle ne se soucie pas de savoir si un sche'ma nouvellement
obtenu contient encore des variables ou non et donc ne peut ou peut e^tre
supprime' du mu proble`me.
*)
(*
LES SCHE'MAS MANIPULE'S VE'RIFIENT LES HYPOTHE`SES SUIVANTES:
-------------------------------------------------------------

* DEFINITION: "sche'ma de corps"
 'sch' est un sche'ma de corps si toutes les occurrences de corps sont des
 instances de 'sch'.

* DEFINITION: "sche'ma de in"
 'sch' est un sche'ma de in si toutes les occurrences de la partie "in" du let
 correspondant sont des instances de 'sch'.

* DEFINITION: "schema compatible"
 'sch' est un sche'ma compatible lorsqu'il est a` la fois sche'ma de corps et
 schema de 'in'

A L'ENTRE'E DE LA FONCTION:
---------------------------

Apres l'e'tape 1:
 Tous les sche'mas du mu proble`me d'entre'e sont de corps. (Trivial a` cause
 de 'check_body_inst')
 Si n >= 1 ils sont compatibles (car de corps et de "in" car ils e'taient
 sche'mas de sortie de l'e'tape n - 1)

A LA SORTIE DE LA FONCTION:
---------------------------

Tous les sche'mas sont sche'mas de "in".

PROPRIE'TE' (point-fixe):
-------------------------
Si l'on obtient les me^mes sche'mas en sortie qu'en entre'e:
              ALORS les schemas sont compatibles


Ce que fait l'algorithme en re'gime permanent: body - gen - in

Si le nombre de variables est nul: faut-il ou non ve'rifier que le sche'ma
obtenu est un sche'ma de corps (on sait qu'il est un sche'ma de "in")
*)
let find_rec_scheme_step mu_problem =
  (* 1 - Unification du type de chaque occurrence recursive (cad occurrences *)
  (* du *corps* de la de'finition) d'un identificateur avec une instance du  *)
  (* sche'ma obtenu jusqu'a` pre'sent pour cet identificateur. Ceci est fait *)
  (* en *paralle`le* sur tout le mu-proble`me. En effet, les de'finitions    *)
  (* re'cursives peuvent induire des contraintes arbitrairement profondes    *)
  (* dans la structure des environnements. Or quand on veut ge'ne'raliser un *)
  (* identificateur i1 se trouvant plus imbrique' dans la structure des      *)
  (* environnements et dont la de'finition fait intervenir un identificateur *)
  (* rec_i2 de'fini re'cursivement plus haut dans la structure d'environment,*)
  (* il faut que les contraintes portant sur l'identificateur re'cursif      *)
  (* rec_i2 soient prises en compte avant toute ge'ne'ralisation de i1.      *)
  (* Les instances du sche'ma obtenu jusqu'a` pre'sent pour cet ident sont   *)
  (* prises au niveau de l'occurrence (cad au niveau ou` on a cre'e' cette   *)
  (* occurrence). C'est comme dans le typage normal ou quand on a un sche'ma *)
  (* pour un identificateur lie' par let et qu'on en prend une instance,     *)
  (* cette instance est prise au niveau courant.                             *)
  let check_body_inst { mu_scheme = sc ; mu_body_occs = boccs } =
      List.iter (fun { Typecore.occ_level = lev ; Typecore.occ_type = nty } ->
                    Typecore.unify_ml_type nty
                           (Typecore.specialize_ml_type_with_level lev sc))
                boccs in
  (* Do it ! *)
  List.iter (fun { mu_sub_defs = defs } -> List.iter check_body_inst defs)
            mu_problem ;
  (* 2 - *En suivant la structure des environnements*, ge'ne'raliser    *)
  (* le type trouve' pour l'expression de'finissante. Ceci nous donne   *)
  (* un sche'ma.                                                        *)
  (* 3 - Ve'rifier que toutes les occurrences dans la partie "in" sont  *)
  (* des instances de ce sche'ma.                                       *)
  (* Le fait de suivre la structure des environnements nous garantit    *)
  (* que les contraintes portant sur les identificateurs les plus       *)
  (* imbrique's seront bien prises en compte au moment ou` on utilisera *)
  (* leur sche'ma dans les parties moins imbrique'es. Ceci e'vite que   *)
  (* le re'sultat du typage de'pende de l'ordre dans lequel les         *)
  (* de'finitions sont e'crites.                                        *)
  let make_new_mu_def level
                      ({ mu_scheme = sc ; mu_name = n;
			 mu_body_occs = boccs ; mu_saturation = sat ;
			 mu_in_occs = ioccs ; mu_type = ety }) =
    (* Maintenant qu'on a fait toutes les instanciations, il va falloir   *)
    (* cre'er les nouveaux sche'mas de type. Ge'ne'raliser en place les   *)
    (* types des expressions de'finissantes afin d'en faire des sche'mas  *)
    (* ne convient pas car alors les sche'mas partagent leurs variables   *)
    (* ge'neralise'es.                                                    *)
    (* D'une part d'une ite'ration sur l'autre les sche'mas resteraient   *)
    (* == et donc le test d'e'galite' serait toujours vrai.               *)
    (* D'autre part les sche'mas partageant des "morceaux" avec les types *)
    (* des occurrences, ge'ne'raliser en place invaliderait les niveaux   *)
    (* des variables dans ces "morceaux".                                 *)
    (* 'copying_generalization' produit un sche'ma de types par copie,    *)
    (* dont les variables ge'ne'ralise'es sont inde'pendantes de celles   *)
    (* des types intervenant dans le proble`me (occurrences et type des   *)
    (* expressions de'finissantes).                                       *)
    let new_scheme = Typecore.copying_generalization_with_level level ety in
    (* Le compteur de saturation est incremente' si le nouveau  *)
    (* sche'ma a plus de variables ge'ne'ralise'es que l'ancien *)
    let new_sat =
      sat + (if new_scheme.Typecore.mts_count > sc.Typecore.mts_count
             then 1 else 0) in
    let new_mu_def =
      { mu_saturation = new_sat ;
	mu_scheme = new_scheme ;
	mu_name = n ;
	mu_body_occs = boccs ;
	mu_in_occs = ioccs ;
	mu_type = ety } in
    (* Les occurrences considere'es sont seulement celles de "in" (point 3) *)
    let check_inst { mu_scheme = sc ; mu_in_occs = ioccs } =
	List.iter (fun { Typecore.occ_level = lev ;
			 Typecore.occ_type = nty } ->
                       Typecore.unify_ml_type nty
                               (Typecore.specialize_ml_type_with_level lev sc))
                  !ioccs in
    (* Do it ! *)
    check_inst new_mu_def ;
    new_mu_def in
  (* Rend les nouveaux sous-proble`mes (cad *un* nouveau mu-probleme) *)
  (* pour e'ventuellement re'curser.                                  *)
  let new_mu_problem =
    List.map (fun { mu_sub_level = level ; mu_sub_defs = defs } ->
               let new_defs = List.map (make_new_mu_def level) defs in
	       { mu_sub_level = level ; mu_sub_defs = new_defs })
              mu_problem in
  new_mu_problem
;;




(* ************************
val monomorphic_recursion_inference :
  mu_sub_problem list -> mu_sub_problem list
*)
(* Cas ou` le mu proble`me semble insoluble. On retourne alors dans *)
(* un mode correspondant au typage de ML conventionnel.             *)
let monomorphic_recursion_inference mu_problem =
  Printf.printf "Back to ML standard unification\n" ;
  (* 1 - Unifier toutes les occurrences *du corps* avec le type *)
  (* de l'expression de'finissante. REM: n'aura d'effet que sur *)
  (* les identificateurs de'finis re'cursivement (cad les       *)
  (* identificateurs de'finis par "let rec"). On fait c,a en    *)
  (* parale`lle sur tout le mu-proble`me.                       *)
  let unify_occurences_in_mu_sub { mu_sub_defs = defs } =
    List.iter
       (fun { mu_body_occs = body_occs ; mu_type = ety } ->
           List.iter (fun { Typecore.occ_type = nty } ->
                            Typecore.unify_ml_type nty ety)
		     body_occs)
       defs in
  (* Do it ! *)
  List.iter unify_occurences_in_mu_sub mu_problem ;
  (* En suivant la structure des environnements:                       *)
  (* 2 - Ge'ne'raliser le type obtenu pour le corps (-> schema SC)     *)
  (* 3 - Ve'rifier que toutes les occurrences (corps et ailleurs) sont *)
  (* des instances de SC.                                              *)
  (* "Suivre la structure des environnements" revient a` ite'rer sur   *)
  (* la liste des mu-sous-proble`mes car on l'a construite dans cette  *)
  (* optique.                                                          *)
  (* *POURQUOI* doit-on suivre la structure de environnement ? C'est   *)
  (* pour que quand on utilise un sche'ma qui provient d'un let (rec)  *)
  (* plus interne, les contraintes portant sur ce sche'ma soient bien  *)
  (* prises en compte au moment ou` l'on utilise ce sche'ma. C,a       *)
  (* revient a` simuler la me'canisme de ge'ne'ralisation *qui suit la *)
  (* structure* d'emboitement des lets utilise' par le typage a` la ML *)
  (* normal.                                                           *)
  (* On ne peut pas se contenter d'unifier *toutes* les occurrences de *)
  (* l'identificateur (body + in) entre elles avec le type de l'expr   *)
  (* de'finissante car dans ce cas, les occurrences se trouvant en     *)
  (* dehors de leur body de de'finition (cad se trouvant dans une      *)
  (* partie "in") seraient aussi unifie'es alors qu'avec un typage ML  *)
  (* normal, la ge'ne'ralisation aurait joue' et seulement des         *)
  (* instances de leur sche'ma auraient e'te' unifie'es. On se         *)
  (* retrouverait alors avec un type *moins* ge'ne'ral que celui       *)
  (* qu'aurait trouve' ML conventionnel.                               *)
  let generalize_and_check_instances_on_mu_problem
                             { mu_sub_level = level ; mu_sub_defs = defs } =
     (* On cree de nouvelles defs a` partir d'anciennes juste en       *)
     (* leur donnant un sche'ma provenant de la ge'ne'ralisation du    *)
     (* type trouve' jusqu'alors pour l'expression de'finissante.      *)
     (* La ge'ne'ralisation s'effectue au niveau ou le sous-proble`me  *)
     (* a e'te' inse're', ce qui correspond au niveau auquel on aurait *)
     (* type le corps du let correspondant dans un typage a` la ML     *)
     (* conventionnel.                                                 *)
     let new_defs =
      List.map (fun { mu_name = n ; mu_body_occs = boccs ;
		      mu_in_occs = ioccs ; mu_type = ety ;
		      mu_saturation = sat } ->
		  { mu_saturation = sat ;
		    (* Utiliser la ge'ne'ralisation copiante ! *)
		    mu_scheme =
                        Typecore.copying_generalization_with_level level ety ;
		    mu_name = n ;
		    mu_body_occs = boccs ;
		    mu_in_occs = ioccs ;
		    mu_type = ety })
               defs in
     (* Imple'mentation du point 3. Les occurrences sont *TOUTES* les *)
     (* occurrences. Peut-etre fait en me^me temps qu'au dessus ???   *)
     List.iter
        (fun { mu_scheme = sc ; mu_in_occs = ioccs ; mu_body_occs = boccs } ->
           List.iter (fun { Typecore.occ_level = lev ;
			    Typecore.occ_type = nty } ->
                        Typecore.unify_ml_type nty
                               (Typecore.specialize_ml_type_with_level lev sc))
                     (boccs @ !ioccs))
        new_defs ;
     (* Re-construit un mu-proble`me avec les nouvelles defs *)
     { mu_sub_level = level ; mu_sub_defs = new_defs } in
  (* Do it ! *)
  List.map generalize_and_check_instances_on_mu_problem mu_problem
;;



(* ************************
val nb_vars_of_mu_problem : mu_sub_problem list -> int
*)
(* ************************
Compte le nombre de variables ge'ne'ralise'es dans tous les sche'mas
d'un mu-proble`me. Ceci sert lors du test d'arre^t de re'cursion.
*)
let nb_vars_of_mu_problem mu_problem =
  List.fold_left
     (fun accu { mu_sub_defs = defs } ->
          List.fold_left
                 (fun accu { mu_scheme = sc } -> accu + sc.Typecore.mts_count)
                 accu defs)
       0 mu_problem
;;



(* ************************
val nb_saturation_of_mu_problem : mu_sub_problem list -> int
*)
(* ************************
Compte le nombre de sche'mas sature's dans tout un mu-proble`me. Un sche'ma
est dit sature' s'il a gonfle au moins 1 fois. Dans ce cas, on ne lui
autorise plus de nouvelle expansion.
*)
let nb_saturation_of_mu_problem mu_problem =
    List.fold_left
     (fun accu { mu_sub_defs = defs } ->
          List.fold_left
                 (fun accu { mu_saturation = expand } ->
                      accu + (if expand >= 1 then 1 else 0) )
                 accu defs)
       0 mu_problem
;;



(* ************************
val valid_saturations : mu_sub_problem list -> bool
*)
(* ************************
Ve'rifie si les flags de saturation d'un mu-proble`me de'notent un
mu-proble`me apte a` e^tre continue'. C'est le cas si tous ces flags sont
infe'rieurs strictement a` 2, autrement dit, si tous les sche'mas du
mu-proble`me n'ont pas depasse' leur quotas de gonflement.
*)
let valid_saturations mu_problem =
  List.for_all
      (fun { mu_sub_defs = defs } ->
             List.for_all 
                 (fun { mu_saturation = expand } -> expand <= 1)
                 defs)
      mu_problem
;;



(* ************************
val same_schemes : mu_sub_problem list -> mu_sub_problem list -> bool
*)
(* ************************
Ve'rifie que tous les sche'mas pre'sents dans new_mu_problem sont e'gaux
a` ceux pre'sents dans mu_problem.
*)
let same_schemes mu_problem new_mu_problem =
    List.for_all2
      (fun { mu_sub_defs = defs } { mu_sub_defs = new_defs } ->
           (List.for_all2 (fun { mu_scheme = sc } { mu_scheme = nsc } ->
                                Typecore.type_scheme_equal sc nsc)
		    defs new_defs))
   mu_problem new_mu_problem
;;


(* ************************
val find_rec_scheme_loop : int -> mu_sub_problem list -> mu_sub_problem list
*)
(* ************************
Fonction qui ite`re les e'tapes de recherche de sche'mas corrects. C'est elle
qui de'cide si il faut continuer les ite'rations, s'arre^ter avec succe`s ou
repasser en mode monomorphique. Grosso modo, on s'arre^te si on a atteint le
point fixe (e'galite' entre les sche'mas n et n-1 ; on s'arre^te avec
ve'rification s'il n'y a plus de variables ge'ne'ralisables dans les sche'mas ;
on passe en mode monomorphique si le nombre de variables du proble`me n'a pas
strictement diminue' et le nombre de saturations n'a pas augmente' (tout en
restant des saturations valides). Pour plus de de'tail, lire les commentaires
ci-dessous.
*)
let rec find_rec_scheme_loop nb_iterations mu_problem =
  let new_mu_problem = find_rec_scheme_step mu_problem in
  (* Maintenant, on ve'rifie si on boucle, si on a fini, si on continue *)
  (* On s'arre^te avec succe`s si :                                     *)
  (*  - il n'y a plus de variables ge'ne'ralise'es dans le proble`me    *)
  (*  - ou si les sche'mas s_n et s_n-1 sont e'gaux.                    *)
  (* Par contre si :                                                    *)
  (*  - le nombre de variables ge'ne'ralise'es du proble`me a           *)
  (*      strictementt de'cru                                           *)
  (*  - ou si le nombre de sche'mas sature's a augmente' strictement    *)
  (* alors on continue.                                                 *)
  (* REM: dans les cas ou` on s'autorise a` continuer, on sait qu'on ne *)
  (* vas pas boucler inde'finiment car il y toujours quelque chose qui  *)
  (* de'croit et qui ne pourra pas de'croitre inde'finiment. Dans le    *)
  (* 1er cas (# de vars ge'ne'ralise'es), comme ce # de variable est    *)
  (* fini, et que le nombre de variables que l'on pourra encore         *)
  (* ge'ne'raliser au prochain tour ne peut que diminuer strictement,   *)
  (* c,a s'arre^tera forcement un jour. Dans le second cas, (# de       *)
  (* sche'mas sature's) ce # de sche'mas est fini, et comme on demande  *)
  (* a` ce qu'il y en ait strictement plus de sature's a` chaque tour,  *)
  (* force'ment le nombre de sche'mas restants non sature's ne peut que *)
  (* diminuer jusqu'a` arriver a` 0.                                    *)
  (*                                                                    *)
  (* Dans le reste des cas, comme on risque de boucler, on se rabat sur *)
  (* l'unification traditionnelle de ML.                                *)
  let new_nb_vars = nb_vars_of_mu_problem new_mu_problem in
  if new_nb_vars = 0 then
    begin
    (* Comme on vient de finir une step, le dernier truc qu'on a fait c'est *)
    (* de ge'ne'raliser le type de l'expression de'finissante pour en faire *)
    (* un nouveau sche'ma (celui-la` me^me qui n'a plus de variables), on   *)
    (* est su^r qu'il ne pourra plus bouger, mais on n'est pas su^r qu'a`   *)
    (* nouveau toutes les occurrences de l'identificateur auquel il est     *)
    (* relie' en sont instances. Il faut donc le ve'rifier mais seulement   *)
    (* sur les occurrences de *corps* car on sait que le sche'ma est un     *)
    (* sche'ma de "in" (c.f commentaires dans ..step..)                     *)
    let check_inst_body { mu_scheme = sc ; mu_body_occs = boccs } =
      List.iter (fun { Typecore.occ_level = lev ; Typecore.occ_type = nty } ->
                    Typecore.unify_ml_type nty
                           (Typecore.specialize_ml_type_with_level lev sc))
                boccs     (* Occurrences de corps seulement *) in
    List.iter (fun { mu_sub_defs = defs } -> List.iter check_inst_body defs)
              new_mu_problem ;
    new_mu_problem
    end
  else
  if same_schemes mu_problem new_mu_problem then new_mu_problem
  else
    begin
    let nb_vars = nb_vars_of_mu_problem mu_problem in
    let new_nb_saturation = nb_saturation_of_mu_problem new_mu_problem in
    let nb_saturation = nb_saturation_of_mu_problem mu_problem in
    if new_nb_vars < nb_vars ||
       (new_nb_saturation > nb_saturation && valid_saturations new_mu_problem)
    then
      find_rec_scheme_loop (nb_iterations + 1) new_mu_problem
    else                     (* Unification ML traditionnelle *)
      (monomorphic_recursion_inference new_mu_problem)
    end
;;



(* ************************
val find_rec_scheme : mu_sub_problem list -> mu_sub_problem list
*)
(* ************************
Point d'entre'e dans la recherche de sche'mas corrects
*)
let find_rec_scheme mu_problem =
  (* Printf.printf "Tour : %d" 0 ; print_newline () ; *)
  (* debug_mu_problem "INITIAL" mu_problem ; *)
  (* On commence par fait une premie`re ite'ration sans test. A l'origine  *)
  (* ceci servait a` autoriser les sche'mas a` gonfler une fois. Si on ne  *)
  (* le faisait pas, sachant que l'on part de for_all 'a . 'a, le sche'ma  *)
  (* ne peut que gonfler et notre test d'arret se serait de'clenche' en    *)
  (* de'cidant de retourner en monomorphic (puisque ce test demandait que  *)
  (* le nombre de variables de'croisse strictement.                        *)
  (* Maintenant, que l'on autorise le gonflement se'pare (1 fois au max)   *)
  (* de chaque sche'ma se'parement et n'importe quand, ce premier coup est *)
  (* a` comprendre !                                                       *)
  let new_mu_problem = find_rec_scheme_step mu_problem in
  find_rec_scheme_loop 1 new_mu_problem
;;



(* ************************
val solve_mu_problem : unit -> mu_sub_problem list
*)
(* ************************
Lancement de la re'solution d'un mu proble`me accumule' lors du typage
d'une phrase. Retourne un mu proble`me re'solu. En particulier, dans ce
mu proble`me re'solu, on pourra trouver les sche'mas corrects a` lier a`
chaque identificateur lie' par le(s) let (rec).
*)
let solve_mu_problem () = find_rec_scheme !mu_problem ;;



(* ************************
val find_def_by_name : Ident.t -> mu_definition list -> mu_definition
*)
(* ************************
Recherche d'une def dans un mu-sous-proble`me, a` partir du nom de\
l'identificateur se rapportant a` cette def.
*)
let rec find_def_by_name idname = function
 | [] -> raise Not_found
 | ({ mu_name = mu_name } as d) :: _ when Ident.equal mu_name idname -> d
 | _ :: rem -> find_def_by_name idname rem
;;



(* ************************
val make_final_links :
  mu_sub_problem list ->
  (Ident.t * Typecore.value_binding) list ->
  (Ident.t * Typecore.ml_types_scheme) list
*)
(* ************************
Cette fonction est charge'e de reconstruire les links finaux devant e^tre
retourne's en fin de typage d'une phrase, apre`s re'solution du mu proble`me,
afin de ge'ne'rer les liens finaux et corrects dans l'environnement. Cette
fonction est donc utilise'e par Infermod, soit pour une phrase toplevel, soit
pour une composante de structure.

Le boulot est de corriger les 'links' obtenus apre`s typage du let (mais
avant re'solution du proble`me mu, ce qui fait que ces links ne sont pas
bons) en remplac,ant les Mu par des Regular a` partir du mu proble`me re'solu.
Ainsi, dans un environnement final obtenu apre`s typage *complet* d'une phrase,
tous les identificateurs seront bien lie's a` un sche'ma de type (correct)
comme on s'y attend en ML. Donc, a` la fin de cette manip, il ne doit plus y
avoir dans l'environnement que des Regular.

ATTENTION: Comme certains trucs on pu ne pas e^tre inse're's dans le mu
proble`me (c.f 'build_defs_and_links'), il se peut qu'on ne trouve pas tous
les sche'mas dans le 'solved_mu_problem_reversed'. Dans ce cas, il faut
prendre ceux correspondant dans les links qui sont alors bons, eux.
*)
let make_final_links solved_mu_problem_reversed links =
 match solved_mu_problem_reversed with
  | [] ->
     (* Le mu proble`me est inexistant, donc on va chercher dans les 'links' *)
     List.map (function (idname, bndval) ->
                match bndval with
                 | Typecore.Mu_ident (_, _) -> assert false
                 | Typecore.Regular_ident sc -> (idname, sc))
              links
  | mu_sub_problem :: _ ->
     (* Il existe un mu proble`me. On va donc commencer par regarder s'il *)
     (* ne contiendrait pas la solution a` notre identificateur.          *)
     (* NOTE: Comme on a inverse notre mu proble`me, et qu'ainsi toutes   *)
     (* les de'finitions toplevel sont en te^te, on ne va s'inte'resser   *)
     (* qu'a la te^te de la liste repre'sentant le mu proble`me re'solu   *)
     (* et inverse'.                                                      *)
     List.map
       (fun (idname, link_scheme) ->
          try
           (idname,
            (find_def_by_name idname mu_sub_problem.mu_sub_defs).mu_scheme)
          with Not_found ->
           begin
           (* On n'a rien trouve' dans le mu proble`me re'solu, c'est    *)
           (* donc que l'identificateur n'a pas eu besoin d'y e^tre mis  *)
           (* et donc qu'on lui avait trouve' un sche'ma correct de`s le *)
           (* de'but. Ce sche'ma a e'te' mis dans les 'links' originaux  *)
           (* et donc on va l'y chercher.                                *)
           match link_scheme with
            | Typecore.Mu_ident (_, _) -> assert false
            | Typecore.Regular_ident sc -> (idname, sc)
           end)
       links
;;
