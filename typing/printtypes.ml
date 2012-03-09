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

(* Bool telling if we must remove row variables only bound positively *)
let forget_pvars = ref false ;;


let abbr_counter = ref 0 ;;
let var_type_counter = ref 0 ;;
let var_row_counter = ref 0 ;;
let var_pre_counter = ref 0 ;;
let abbreviations = ref ([] : Typecore.ml_type_expr list) ;;


(* int_to_base_26 : int -> string *)
(* Transform an integer to a string compound of only a-z chars. Used to *)
(* write variables. In fact, that only a integer->base 26 printer.      *)
let rec int_to_base_26 i =
 if i >= 26 then
  let ch = (i mod 26) + (Char.code 'a') in
  (int_to_base_26 (i / 26)) ^ Char.escaped (Char.chr ch)
 else
  let ch = (i mod 26) + (Char.code 'a') in
  Char.escaped (Char.chr ch)
;;


let create_abbrev_name () =
  let name = "TY"^(string_of_int !abbr_counter) in
  incr abbr_counter ;
  name
;;


let create_type_variable_name generalized_var =
  let name = "'" ^ (int_to_base_26 !var_type_counter) in
  incr var_type_counter ;
  if generalized_var then name else "_" ^ name
;;


let create_row_variable_name generalized_var =
  let name = "`" ^ (int_to_base_26 !var_row_counter) in
  incr var_row_counter ;
  if generalized_var then name else "_" ^ name
;;


let create_presence_variable_name generalized_var =
  let name = "$" ^ (int_to_base_26 !var_pre_counter) in
  incr var_pre_counter ;
  if generalized_var then name else "_" ^ name
;;



let rec fake_pp_ml_type ml_ty =
 let ml_ty = Typecore.ml_type_repr ml_ty in
 match ml_ty.Typecore.mte_info with
  | Typecore.Abbreviated _ -> () (* C'est qu'on a voulu garder ce nom *)
                                 (* d'un coup précédent. C'est seulement *)
                                 (* utilisé quand on affiche les décl de *)
                                 (* type car on a besoin de garder le    *)
                                 (* partage de noms entre la liste préfixe *)
                                 (* des variables et leurs occurences dans *)
                                 (* le corps de la définition de type. Dans *)
                                 (* tous les autres cas, ceci ne devrait *)
                                 (* jamais se produire. *)
  | Typecore.Seen xtime -> incr xtime
  | Typecore.Aliased _ -> assert false
  | Typecore.Notseen | Typecore.Seennotrec ->
      begin
      ml_ty.Typecore.mte_info <- Typecore.Seen (ref 1) ;
      match ml_ty.Typecore.mte_desc with
       | Typecore.Tvar -> ()
       | Typecore.Tarrow (neg, pos, effect) ->
	   fake_pp_ml_type neg ;
	   fake_pp_ml_type pos ;
	   fake_pp_phi_type effect
       | Typecore.Tconstr (_, args, appr) ->
	   List.iter fake_pp_ml_type args ;
	   fake_pp_phi_type appr
       | Typecore.Ttuple (args) ->
	   List.iter fake_pp_ml_type args
      end ;
      begin
      (* Now we check if the current type is recursive, i.e if it *)
      (* appeared in the subtree below it. If not, then we don't  *)
      (* need to alias is with a "where" construction.            *)
      match ml_ty.Typecore.mte_info with
       | Typecore.Seen xtime' -> if !xtime' = 1 then
	   ml_ty.Typecore.mte_info <- Typecore.Seennotrec
       | _ -> assert false
      end


and fake_pp_phi_type phi =
  let phi = Typecore.phi_repr phi in
  match phi.Typecore.phi_value with
   | Typecore.Pbottom -> ()
   | Typecore.Pexplicit (pel, rv) ->
       List.iter fake_pp_phi_elem pel ;
       match rv.Typecore.row_info with
        | Typecore.Seen xtime -> incr xtime
	| Typecore.Aliased _ -> assert false
        | Typecore.Abbreviated _ ->
	    (* C'est qu'on a voulu garder ce nom *)
            (* d'un coup précédent. C'est seulement *)
            (* utilisé quand on affiche les décl de *)
            (* type car on a besoin de garder le    *)
            (* partage de noms entre la liste préfixe *)
            (* des variables et leurs occurences dans *)
            (* le corps de la définition de type. Dans *)
            (* tous les autres cas, ceci ne devrait *)
            (* jamais se produire. *) ()
	| Typecore.Notseen | Typecore.Seennotrec ->
	    rv.Typecore.row_info <- Typecore.Seen (ref 1)


and fake_pp_phi_elem = function
  | Typecore.Constant (_, presence) -> fake_pp_presence presence
  | Typecore.Param (_, ml_type_expr) -> fake_pp_ml_type ml_type_expr
  | Typecore.Record fields ->
      List.iter (fun (_, ty) -> fake_pp_ml_type ty) fields


and fake_pp_presence presence =
  let presence = Typecore.presence_repr presence in
  match presence.Typecore.pre_desc with
   | Typecore.Prpre -> ()
   | Typecore.Prvar ->
       match presence.Typecore.pre_info with
        | Typecore.Seen xtime -> incr xtime
        | Typecore.Aliased _ | Typecore.Abbreviated _ -> assert false
        | Typecore.Notseen | Typecore.Seennotrec ->
	    presence.Typecore.pre_info <- Typecore.Seen (ref 1)
;;



let rec ipp_ml_type prio ppf ml_ty =
 let ml_ty = Typecore.ml_type_repr ml_ty in
 match ml_ty.Typecore.mte_info with
  | Typecore.Aliased _ | Typecore.Notseen -> assert false
  | Typecore.Abbreviated abb_name -> fprintf ppf "%s" abb_name
  | Typecore.Seennotrec ->
      begin
      (* The type does not need to be aliased because it is not    *)
      (* recursive. So we just descent its structure for printing. *)
      match ml_ty.Typecore.mte_desc with
       | Typecore.Tvar ->
	   let abb_name = create_type_variable_name
	                 (ml_ty.Typecore.mte_level = Typecore.generic_level) in
	   (* We MUST alias the type because it is a variable. Hence *)
	   (* we preserve name sharing between same type variables.  *)
	   ml_ty.Typecore.mte_info <- Typecore.Abbreviated abb_name ;
	   fprintf ppf "%s" abb_name
       | Typecore.Tarrow (neg, pos, effect) ->
	   if prio >= 2 then fprintf ppf "@[<1>(" else fprintf ppf "@[" ;
	   fprintf ppf "%a@ @[-{@[%a@]}->@]@ %a"
	               (ipp_ml_type 2) neg
	               (ipp_phi_type "") effect
	               (ipp_ml_type 1) pos ;
	   if prio >= 2 then fprintf ppf ")@]" else fprintf ppf "@]"
       | Typecore.Tconstr (path, args, appr) ->
	   let iter_ipp_ml_type = Printbasic.iter_pp "," (ipp_ml_type 0) in
           fprintf ppf "@[" ;
	   begin
           match args with
	    | [] -> ()
	    | [ty1] -> fprintf ppf "%a@ " (ipp_ml_type 3) ty1
	    | tyl -> fprintf ppf "@[<1>(%a)@]@ " iter_ipp_ml_type tyl
	   end ;
	   let enclose = (
	     if Path.equal !path (Path.Pident Ident.string_ident) then "\""
	     else if Path.equal !path (Path.Pident Ident.char_ident) then "'"
	     else "") in
           fprintf ppf "%a@,@[<1>[%a]@]@]" Path.pp_path
	               !path (ipp_phi_type enclose) appr
       | Typecore.Ttuple (args) ->
	   let iter_ipp_ml_type = Printbasic.iter_pp " *" (ipp_ml_type 3) in
	   if prio >= 3 then fprintf ppf "@[<1>(%a)@]" iter_ipp_ml_type args
           else fprintf ppf "@[%a@]" iter_ipp_ml_type args      
      end
  | Typecore.Seen xtime ->
      (* Si xtime > 1 || ml_ty = Tvar alors print abbrev sinon print bestial *)
      if !xtime <= 0 then assert false ;
      if !xtime > 1 || ml_ty.Typecore.mte_desc = Typecore.Tvar then
	begin
	let abb_name =
	  if ml_ty.Typecore.mte_desc <> Typecore.Tvar then
	    create_abbrev_name ()
	  else
	    create_type_variable_name
	                 (ml_ty.Typecore.mte_level = Typecore.generic_level) in
	ml_ty.Typecore.mte_info <- Typecore.Abbreviated abb_name ;
	fprintf ppf "%s" abb_name ;
	(* Don't consider variables as abbreviations *)
	if ml_ty.Typecore.mte_desc <> Typecore.Tvar then
	  abbreviations := ml_ty :: !abbreviations
	end
      else
	match ml_ty.Typecore.mte_desc with
	 | Typecore.Tvar -> ()  (* Done above *)
	 | Typecore.Tarrow (neg, pos, effect) ->
	     if prio >= 2 then fprintf ppf "@[<1>(" else fprintf ppf "@[" ;
	     fprintf ppf "%a@ @[-{@[%a@]}->@]@ %a"
	                 (ipp_ml_type 2) neg
	                 (ipp_phi_type "") effect
	                 (ipp_ml_type 1) pos ;
	     if prio >= 2 then fprintf ppf ")@]" else fprintf ppf "@]"
	 | Typecore.Tconstr (path, args, appr) ->
	     let iter_ipp_ml_type = Printbasic.iter_pp "," (ipp_ml_type 0) in
             fprintf ppf "@[" ;
	     begin
               match args with
	        | [] -> ()
	        | [ty1] -> fprintf ppf "%a@ " (ipp_ml_type 3) ty1
	        | tyl -> fprintf ppf "@[<1>(%a)@]@ " iter_ipp_ml_type tyl
	     end ;
	     let enclose = (
	      if Path.equal !path (Path.Pident Ident.string_ident) then "\""
	      else if Path.equal !path (Path.Pident Ident.char_ident) then "'"
	      else "") in
             fprintf ppf "%a@,@[<1>[%a]@]@]" Path.pp_path
	                 !path (ipp_phi_type enclose) appr
	 | Typecore.Ttuple (args) ->
	     let iter_ipp_ml_type = Printbasic.iter_pp " *" (ipp_ml_type 3) in
	     if prio >= 3 then fprintf ppf "@[<1>(%a)@]" iter_ipp_ml_type args
             else fprintf ppf "@[%a@]" iter_ipp_ml_type args



(* Utilise pour initier l'impression des abbreviations. Autrement dit   *)
(* comme le type a imprimer est forcement Abbreviated, si on utilise    *)
(* l'impression standard, on sera bien avance car ca nous ecrira .. son *)
(* nom d'abbrev. Donc pour la premiere fois, on descend sur le type,    *)
(* ensuite on repart normalement avec le routine d'impression normale.  *)
(* au passage, on notera que le type par lequel on rentre ne peut pas   *)
(* etre Seen _ car forcement il aurait ete passe en Abbreviated par la  *)
(* routine standard d'impression qui est forcement appelee avant pour   *)
(* afficher le corps du type (n'oublions pas qu'ici on est en train     *)
(* d'afficher la signification des abbreviations !).                    *)
and iexplain_ml_type_abbrev prio ppf ml_ty =
 let ml_ty = Typecore.ml_type_repr ml_ty in
 match ml_ty.Typecore.mte_info with
  | Typecore.Aliased _ | Typecore.Notseen | Typecore.Seen _ -> assert false
  | _ ->
      match ml_ty.Typecore.mte_desc with
      | Typecore.Tvar -> ()  (* Done above *)
      | Typecore.Tarrow (neg, pos, effect) ->
	  if prio >= 2 then fprintf ppf "@[<1>(" else fprintf ppf "@[" ;
	  fprintf ppf "%a@ @[-{@[%a@]}->@]@ %a"
	              (ipp_ml_type 2) neg
	              (ipp_phi_type "") effect
	              (ipp_ml_type 1) pos ;
	  if prio >= 2 then fprintf ppf ")@]" else fprintf ppf "@]"
      | Typecore.Tconstr (path, args, appr) ->
	  let iter_ipp_ml_type = Printbasic.iter_pp "," (ipp_ml_type 0) in
          fprintf ppf "@[" ;
	  begin
          match args with
	   | [] -> ()
	   | [ty1] -> fprintf ppf "%a@ " (ipp_ml_type 3) ty1
	   | tyl -> fprintf ppf "@[<1>(%a)@]@ " iter_ipp_ml_type tyl
	  end ;
	  let enclose = (
	    if Path.equal !path (Path.Pident Ident.string_ident) then "\""
	    else if Path.equal !path (Path.Pident Ident.char_ident) then "'"
	    else "") in
          fprintf ppf "%a@,@[<1>[%a]@]@]" Path.pp_path !path
                  (ipp_phi_type enclose) appr
      | Typecore.Ttuple (args) ->
	  let iter_ipp_ml_type = Printbasic.iter_pp " *" (ipp_ml_type 3) in
	  if prio >= 3 then fprintf ppf "@[<1>(%a)@]" iter_ipp_ml_type args
          else fprintf ppf "@[%a@]" iter_ipp_ml_type args



(* We nee the path in order to know if we must enclose         *)
(* approxs by "" or '' if they are approxs of string or chars. *)
and ipp_phi_type enclose ppf phi =
 (* No repr, it has already be done by 'fake' *)
   match phi.Typecore.phi_value with
    | Typecore.Pbottom -> fprintf ppf "_"
    | Typecore.Pexplicit (pel, rv) ->
	List.iter (pp_phi_elem enclose ppf) pel ;
	match rv.Typecore.row_info with
	 | Typecore.Notseen | Typecore.Aliased _ -> assert false
	 | Typecore.Abbreviated abb_name -> fprintf ppf "%s" abb_name
	 | Typecore.Seennotrec ->
              let abb_name =
		create_row_variable_name
                          (rv.Typecore.row_level = Typecore.generic_level) in
	      fprintf ppf "%s" abb_name
	 | Typecore.Seen xtime ->
	     if !xtime <= 0 then assert false ;
	     let abb_name =
		create_row_variable_name
                          (rv.Typecore.row_level = Typecore.generic_level) in
	     rv.Typecore.row_info <- Typecore.Abbreviated abb_name ;
	     fprintf ppf "%s" abb_name


and pp_phi_elem enclose ppf = function
  | Typecore.Constant (name, presence) ->
      (* Enclose values by "" or '' if needed *)
      fprintf ppf "%s%s%s:%a;@ " enclose name enclose pp_presence presence
  | Typecore.Param (name, ml_type_expr) ->
      fprintf ppf "@[<1>%s@ @[<1>(%a)@]@];@ " name (ipp_ml_type 0) ml_type_expr
  | Typecore.Record fields ->
      let pp_field ppf (name, ty) =
	fprintf ppf "@[<1>%s :@ %a@]" name (ipp_ml_type 0) ty in
      let iter_pp_field = Printbasic.iter_pp "; " pp_field in
      fprintf ppf "@[<2>{ %a }@]" iter_pp_field fields



and pp_presence ppf pr =
 let pr = Typecore.presence_repr pr in
 match pr.Typecore.pre_desc with
  | Typecore.Prpre -> fprintf ppf "Pre"
  | Typecore.Prvar ->
      match pr.Typecore.pre_info with
       | Typecore.Notseen | Typecore.Aliased _ -> assert false
       | Typecore.Abbreviated abb_name -> fprintf ppf "%s" abb_name
       | Typecore.Seennotrec ->
	   let abb_name = create_presence_variable_name
	                   (pr.Typecore.pre_level = Typecore.generic_level) in
	   fprintf ppf "%s" abb_name
       | Typecore.Seen xtime ->
	   if !xtime <= 0 then assert false ;
	   let abb_name = create_presence_variable_name
	                   (pr.Typecore.pre_level = Typecore.generic_level) in
	   pr.Typecore.pre_info <- Typecore.Abbreviated abb_name ;
	   fprintf ppf "%s" abb_name
;;



let rec ipp_abbreviations ppf abbreviated_tys =
  (* Clear the found abbreviations list *)
  abbreviations := [] ;
  List.iter (fun abbreviated_ty ->
               (* repr has already be done *)
               match abbreviated_ty.Typecore.mte_info with
	        | Typecore.Abbreviated name ->
		    Format.fprintf ppf "@\n  @[where %s = %a@]" name
                                  (iexplain_ml_type_abbrev 0) abbreviated_ty
		| _ -> assert false)
  abbreviated_tys ;
  (* Check if during abbrv printing we have found hidden new abbrevs *)
  let abbreviated_tys2 = !abbreviations in
  (* If yes, go on printing. This must terminate because each time *)
  (* we find something new, we mark it as Abbreviated so when all  *)
  (* is marked Abbreviated, we do not have anything to do...       *)
  if abbreviated_tys2 <> [] then ipp_abbreviations ppf abbreviated_tys2
;;


(* pp_ml_type_scheme: Format.formatter -> Typecore.ml_types_scheme -> unit *)
(* Pretty print a ml types scheme *)
let pp_ml_type_scheme ppf scheme =
  abbr_counter := 0 ;
  var_type_counter := 0 ;
  var_row_counter := 0 ;
  var_pre_counter := 0 ;
  abbreviations := [] ;
  fake_pp_ml_type scheme.Typecore.mts_body ;
  ipp_ml_type 0 ppf scheme.Typecore.mts_body ;
  ipp_abbreviations ppf (List.rev !abbreviations) ;
  Typecore.clean_ml_type scheme.Typecore.mts_body
;;



(* pp_ml_type: Format.formatter -> Typecore.ml_type_expr -> unit  *)
(* Pretty print a type expresion *)
let pp_ml_type ppf ty =
  abbr_counter := 0 ;
  var_type_counter := 0 ;
  var_row_counter := 0 ;
  var_pre_counter := 0 ;
  abbreviations := [] ;
  fake_pp_ml_type ty ;
  ipp_ml_type 0 ppf ty ;
  ipp_abbreviations ppf (List.rev !abbreviations) ;
  Typecore.clean_ml_type ty
;;



(* pp_phi_type: pp_phi_type : Format.formatter -> Typecore.phi_expr -> unit *)
(* Pretty print an approx (phi_expr) expresion *)
let pp_phi_type ppf phi =
  abbr_counter := 0 ;
  var_type_counter := 0 ;
  var_row_counter := 0 ;
  var_pre_counter := 0 ;
  abbreviations := [] ;
  fake_pp_phi_type phi ;
  ipp_phi_type "" ppf phi ;
  ipp_abbreviations ppf (List.rev !abbreviations) ;
  Typecore.clean_phi_type phi
;;



let pp_type_declaration ppf (ty_name, ty_decl) =
  abbr_counter := 0 ;
  var_type_counter := 0 ;
  var_row_counter := 0 ;
  var_pre_counter := 0 ;
  abbreviations := [] ;
  if ty_decl.Typedtree.type_arity <> 0 then
    begin
    (* Ne pas effacer les compteurs entre chaque variable *)
    let iter_pp_ty =
      Printbasic.iter_pp ","
                         (fun ppf t -> fprintf ppf "%a" (ipp_ml_type 0) t) in
    List.iter fake_pp_ml_type ty_decl.Typedtree.type_params ;
    fprintf ppf "(@[%a)@]@ " iter_pp_ty ty_decl.Typedtree.type_params
    end ;
  fprintf ppf "%s" (Ident.name ty_name) ;
  (* Traiter le kind *)
  begin
  match ty_decl.Typedtree.type_kind with
   | Typedtree.Type_abstract -> ()
   | Typedtree.Type_variant cstrs ->
       fprintf ppf " =@ " ;
       List.iter (fun (_, sc) -> fake_pp_ml_type sc.Typecore.mts_body) cstrs ;
       List.iter (fun (name, sc) ->
                    fprintf ppf "%s of %a" (Ident.name name)
                            (ipp_ml_type 0) sc.Typecore.mts_body)
                 cstrs ;
       ipp_abbreviations ppf (List.rev !abbreviations) ;
       List.iter (fun (_, sc) -> Typecore.clean_ml_type sc.Typecore.mts_body)
                 cstrs
   | Typedtree.Type_record _ -> assert false
  end ;
  (* Traiter ce à quoi le type est égal en cas d'abbréviation *)
  begin
  match ty_decl.Typedtree.type_manifest with
   | None -> ()
   | Some scheme ->
       fprintf ppf " =@ " ;
       fake_pp_ml_type scheme.Typecore.mts_body ;
       ipp_ml_type 0 ppf scheme.Typecore.mts_body ;
       ipp_abbreviations ppf (List.rev !abbreviations) ;
       Typecore.clean_ml_type scheme.Typecore.mts_body
  end ;
  (* Maintenant, on peut nettoyer les paramètres puisque leur nom    *)
  (* a bien été entre la liste préfixe et le corps de la définition. *)
  List.iter Typecore.clean_ml_type ty_decl.Typedtree.type_params
;;

