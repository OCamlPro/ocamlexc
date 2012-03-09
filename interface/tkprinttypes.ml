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
open Tk ;;


(* Bool telling if we must force "empty" vrs to appear in types *)
let force_vars_print = ref false ;;


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



(* create_type_tag: unit -> string *)
(* Creates a new tag for enclosing type printing.  *)
let create_type_tag =
 let cpt = ref 0 in
 function () ->
  let name = "TYPE" ^ (string_of_int !cpt) in
  incr cpt ;
  name
;;



(* Update the flag indicating in which positions the type occurs    *)
(* Must be called FIRST. Just after, remember to clean the type up. *)
let rec update_position_ml_type position ml_ty =
 let ml_ty = Typecore.ml_type_repr ml_ty in
 match ml_ty.Typecore.mte_info with
 | Typecore.Aliased _ | Typecore.Abbreviated _ | Typecore.Seennotrec ->
     (* These markers should not be used during "position walking" *)
     assert false
  | Typecore.Seen posit when !posit = -1 || !posit = position -> ()
  | Typecore.Notseen | Typecore.Seen _ ->
      (* Either the type has never be seen. Either is has already be seen *)
      (* but not in position -1 (so in position 1), and not with the      *)
      (* current position (so current position is mandatorily -1).        *)
      ml_ty.Typecore.mte_info <- Typecore.Seen (ref position) ;
      ml_ty.Typecore.mte_position <- position ;
      match ml_ty.Typecore.mte_desc with
       | Typecore.Tvar -> ()
       | Typecore.Tarrow (neg, pos, effect) ->
	   update_position_ml_type (- position) neg ;
	   update_position_ml_type position pos ;
	   update_position_phi_type position effect
       | Typecore.Tconstr (_, args, appr) ->
	   List.iter (update_position_ml_type position) args ;
	   update_position_phi_type position appr
       | Typecore.Ttuple args ->
	   List.iter (update_position_ml_type position) args


and update_position_phi_type position phi =
  let phi = Typecore.phi_repr phi in
  match phi.Typecore.phi_value with
   | Typecore.Pbottom -> ()
   | Typecore.Pexplicit (pel, rv) ->
       List.iter (update_position_phi_elem position) pel ;
       match rv.Typecore.row_info with
	| Typecore.Aliased _ | Typecore.Abbreviated _ | Typecore.Seennotrec ->
	    (* These markers should not be used during "position walking" *)
	    assert false
        | Typecore.Seen posit when !posit = -1 || !posit = position -> ()
	| Typecore.Notseen | Typecore.Seen _ ->
	    rv.Typecore.row_info <- Typecore.Seen (ref position) ;
	    rv.Typecore.row_position <- position


and update_position_phi_elem position = function
  | Typecore.Constant (_, pres) -> update_position_presence position pres
  | Typecore.Param (_, ty) -> update_position_ml_type position ty
  | Typecore.Record fields ->
      List.iter (fun (_, ty) -> update_position_ml_type position ty) fields


and update_position_presence position pres =
  let pres = Typecore.presence_repr pres in
  match pres.Typecore.pre_info with
   | Typecore.Aliased _ | Typecore.Abbreviated _ | Typecore.Seennotrec ->
       (* These markers should not be used during "position walking" *)
       assert false
   | Typecore.Seen posit when !posit = -1 || !posit = position -> ()
   | Typecore.Notseen | Typecore.Seen _ ->
       (* We don't need to check the structure because no recursion *)
       pres.Typecore.pre_info <- Typecore.Seen (ref position) ;
       pres.Typecore.pre_position <- position
;;



let is_ml_type_empty ml_ty =
 let ml_ty = Typecore.ml_type_repr ml_ty in
 ml_ty.Typecore.mte_empty
;;


let is_phi_type_empty phi =
 let phi = Typecore.phi_repr phi in
 phi.Typecore.phi_empty
;;


let is_phi_type_visible phi =
 let phi = Typecore.phi_repr phi in
 phi.Typecore.phi_print
;;


let is_presence_empty pre =
 let pre = Typecore.presence_repr pre in
 match pre.Typecore.pre_desc with
  | Typecore.Prpre -> false
  | Typecore.Prvar -> pre.Typecore.pre_position = 1
;;



(* Update the flag indicating if the type is "empty". Must be called *)
(* in SECOND stage, i.e after the position flags are updated. Just   *)
(* after, remember to clean the type up.                             *)
let rec update_empty_flag_ml_type ml_ty =
 let ml_ty = Typecore.ml_type_repr ml_ty in
 match ml_ty.Typecore.mte_info with
 | Typecore.Aliased _ | Typecore.Abbreviated _ | Typecore.Seennotrec ->
     (* These markers should not be used during "empty walking" *)
     assert false
  | Typecore.Seen _ -> ()
  | Typecore.Notseen ->
      ml_ty.Typecore.mte_info <- Typecore.Seen (ref 1) ;
      match ml_ty.Typecore.mte_desc with
       | Typecore.Tvar ->
	   if ml_ty.Typecore.mte_position = -1 then
	     ml_ty.Typecore.mte_empty <- false
       | Typecore.Tarrow (neg, pos, eff) ->
	   update_empty_flag_ml_type neg ;
	   update_empty_flag_ml_type pos ;
	   update_empty_flag_phi_type eff ;
	   ml_ty.Typecore.mte_empty <-
	           (is_ml_type_empty neg) && (is_ml_type_empty pos) &&
	           (is_phi_type_empty eff)
       | Typecore.Ttuple args ->
	   List.iter update_empty_flag_ml_type args ;
	   ml_ty.Typecore.mte_empty <-
	        List.for_all (fun t -> is_ml_type_empty t) args
       | Typecore.Tconstr (_, args, appr) ->
	   List.iter update_empty_flag_ml_type args ;
	   update_empty_flag_phi_type appr ;
	   ml_ty.Typecore.mte_empty <-
	          List.for_all (fun t -> is_ml_type_empty t) args
		  && is_phi_type_empty appr ;


and update_empty_flag_phi_type phi =
 let phi = Typecore.phi_repr phi in
 match phi.Typecore.phi_value with
  | Typecore.Pbottom -> phi.Typecore.phi_empty <- false
  | Typecore.Pexplicit (pel, rv) ->
      let pel_emptiness = List.map update_empty_flag_phi_elem pel in
      match rv.Typecore.row_info with
      | Typecore.Aliased _ | Typecore.Abbreviated _ | Typecore.Seennotrec ->
	  (* These markers should not be used during "empty walking" *)
	  assert false
      | Typecore.Seen _ ->
	  (* We have seen the row_variable, but perhaps it is used  *)
	  (* in another row than the one where we saw it before. So *)
	  (* Update anyway the phi empty flag. THIS IS BECAUSE OUR  *)
	  (* EMPTINESS FLAG IS ASSOCIATED TO A ROW AND NOT ONLY TO  *)
	  (* A ROW VARIABLE.                                        *)
	  phi.Typecore.phi_empty <-
	              List.for_all (fun x -> x) pel_emptiness
	              && rv.Typecore.row_position = 1
      | Typecore.Notseen ->
	  rv.Typecore.row_info <- Typecore.Seen (ref 1) ;
	  phi.Typecore.phi_empty <-
	              List.for_all (fun x -> x) pel_emptiness
	              && rv.Typecore.row_position = 1


and update_empty_flag_phi_elem = function
  | Typecore.Constant (_, pres) -> is_presence_empty pres
  | Typecore.Param (_, ml_type_expr) ->
      update_empty_flag_ml_type ml_type_expr ;
      is_ml_type_empty ml_type_expr
  | Typecore.Record fields ->
      List.iter (fun (_, t) -> update_empty_flag_ml_type t) fields ;
      List.exists (fun (_, t) -> is_ml_type_empty t) fields
;;





let rec fake_pp_ml_type ml_ty =
 let ml_ty = Typecore.ml_type_repr ml_ty in
 match ml_ty.Typecore.mte_info with
  | Typecore.Seen xtime -> incr xtime
  | Typecore.Aliased _ | Typecore.Abbreviated _ -> assert false
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
  if phi.Typecore.phi_print then
  match phi.Typecore.phi_value with
   | Typecore.Pbottom -> ()
   | Typecore.Pexplicit (pel, rv) ->
       List.iter fake_pp_phi_elem pel ;
       match rv.Typecore.row_info with
        | Typecore.Seen xtime -> incr xtime
	| Typecore.Aliased _ | Typecore.Abbreviated _ -> assert false
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



(* When partially applied, this gives the callback to associate to a *)
(* mark in order to make the type expression rollable.               *)
let rec make_callback pcontext appr_to_expand ppf root_type _ =
  let widget = pcontext.Printcontext.widget in
  appr_to_expand.Typecore.phi_print <- not appr_to_expand.Typecore.phi_print ;
  Text.configure widget [State Normal] ;
  Text.delete widget (TextIndex
		      (Mark
		       ("START" ^ pcontext.Printcontext.mark_radical), []))
                     (TextIndex
		      (Mark
		       ("STOP" ^ pcontext.Printcontext.mark_radical), [])) ;
  Text.mark_gravity_set widget
                    ("STOP" ^ pcontext.Printcontext.mark_radical) Mark_Right ;
  (* Get the current indentation level *)
  let indentation =
    (match Text.index widget
		      (TextIndex
		       (Mark
			("STOP" ^ pcontext.Printcontext.mark_radical), []))
    with LineChar (_, c) -> c | _ -> assert false) in
  let pcontext' = { pcontext with
                    Printcontext.widget = widget ;
                    Printcontext.left_indent = indentation } in
  pp_set_margin ppf (78 - indentation) ;
  let (old_print, old_flush) = get_formatter_output_functions () in
  set_formatter_output_functions
            (fun s pos num ->
	      Tklowprint.scan_string_at pcontext'
			         (TextIndex
				  (Mark
				   ("STOP" ^
				    pcontext'.Printcontext.mark_radical), []))
			         s pos num)
            (fun () -> ()) ;
  (match root_type with
   | Printcontext.Nothing -> assert false
   | Printcontext.Ml ml_ty ->
       fprintf std_formatter "%a@?" (pp_ml_type pcontext') ml_ty
   | Printcontext.Phi phi_ty ->
       fprintf std_formatter "%a@?" (pp_phi_type pcontext') phi_ty) ;
  set_formatter_output_functions old_print old_flush ;
  Text.configure widget [State Disabled] ;
  Text.mark_gravity_set widget ("STOP" ^
				pcontext'.Printcontext.mark_radical) Mark_Left



and ipp_ml_type pcontext prio ppf ml_ty =
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
	   let abb_name =
	     create_type_variable_name
	                (ml_ty.Typecore.mte_level = Typecore.generic_level) in
	   (* We MUST alias the type because it is a variable. Hence *)
	   (* we preserve name sharing between same type variables.  *)
	   ml_ty.Typecore.mte_info <- Typecore.Abbreviated abb_name ;
	   fprintf ppf "%s" abb_name
       | Typecore.Tarrow (neg, pos, effect) ->
	   if prio >= 2 then fprintf ppf "@[<1>(" else fprintf ppf "@[" ;
	   let type_tag = create_type_tag () in
	   if is_phi_type_empty effect then
	     (* Because effect is empty, we don't put any anchor *)
	     fprintf ppf "%a@ ->@ %a" (ipp_ml_type pcontext 2) neg
                                      (ipp_ml_type pcontext 1) pos
	   else
	     fprintf ppf "%a@ @[@<0>%s-{@<0>%s@[%a@]@<0>%s}->@<0>%s@]@ %a"
                     (ipp_ml_type pcontext 2) neg
	             ("\006"^ type_tag ^"\007") "\006\007"
	             (ipp_phi_type "" pcontext) effect
	             ("\006"^ type_tag ^"\007") "\006\007"
                     (ipp_ml_type pcontext 1) pos ;
	   if prio >= 2 then fprintf ppf ")@]" else fprintf ppf "@]" ;
	   (* Tag stuff *)
	   begin
	   match pcontext.Printcontext.root_type with
	    | Printcontext.Nothing -> ()
	    | root_type ->
		(* Create the callback function *)
		let callback = make_callback pcontext effect ppf root_type in
		(* And now bind and configure the tag *)
		Text.tag_configure pcontext.Printcontext.widget type_tag
		                   [Foreground Red; Underline true] ;
		Text.tag_bind pcontext.Printcontext.widget type_tag
	                      [([], ButtonPressDetail 1)]
                             (BindSet ([], callback))
	   end
       | Typecore.Tconstr (path, args, appr) ->
	   let iter_ipp_ml_type =
	     Printbasic.iter_pp "," (ipp_ml_type pcontext 0) in
           fprintf ppf "@[" ;
	   begin
           match args with
	    | [] -> ()
	    | [ty1] -> fprintf ppf "%a@ " (ipp_ml_type pcontext 3) ty1
	    | tyl -> fprintf ppf "@[<1>(%a)@]@ " iter_ipp_ml_type tyl
	   end ;
	   (* Print the type name enclosed by the tag *)
	   let type_tag = create_type_tag () in
	   let enclose = (
	     if Path.equal !path (Path.Pident Ident.string_ident) then "\""
	     else if Path.equal !path (Path.Pident Ident.char_ident) then "'"
	     else "") in
	   if is_phi_type_empty appr then
	     fprintf ppf "%a@]" Path.pp_path !path
	   else
	     if is_phi_type_visible appr then
	       fprintf ppf "@<0>%s%a@<0>%s@,@[<1>[%a]@]@]"
	             ("\006"^ type_tag ^"\007") Path.pp_path !path "\006\007"
                     (ipp_phi_type enclose pcontext) appr
	     else
	       fprintf ppf "@<0>%s%a@<0>%s@,@]"
	             ("\006"^ type_tag ^"\007") Path.pp_path !path "\006\007" ;
	   (* Tag stuff *)
	   begin
	   match pcontext.Printcontext.root_type with
	    | Printcontext.Nothing -> ()
	    | root_type ->
		(* Create the callback function *)
		let callback = make_callback pcontext appr ppf root_type in
		(* And now bind and configure the tag *)
		Text.tag_configure pcontext.Printcontext.widget type_tag
		                   [Foreground (NamedColor "gray45");
				    Underline true] ;
		Text.tag_bind pcontext.Printcontext.widget type_tag
	                      [([], ButtonPressDetail 1)]
                             (BindSet ([], callback))
	   end
       | Typecore.Ttuple (args) ->
	   let iter_ipp_ml_type =
	     Printbasic.iter_pp " *" (ipp_ml_type pcontext 3) in
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
	     let type_tag = create_type_tag () in
	     if is_phi_type_empty effect then
	       (* Because effect is empty, we don't put any anchor *)
	       fprintf ppf "%a@ ->@ %a" (ipp_ml_type pcontext 2) neg
                                        (ipp_ml_type pcontext 1) pos
	       else
	       fprintf ppf "%a@ @[@<0>%s-{@<0>%s@[%a@]@<0>%s}->@<0>%s@]@ %a"
                       (ipp_ml_type pcontext 2) neg
	               ("\006"^ type_tag ^"\007") "\006\007"
	               (ipp_phi_type "" pcontext) effect
	               ("\006"^ type_tag ^"\007") "\006\007"
                       (ipp_ml_type pcontext 1) pos ;
	     if prio >= 2 then fprintf ppf ")@]" else fprintf ppf "@]"
	 | Typecore.Tconstr (path, args, appr) ->
	     let iter_ipp_ml_type =
	       Printbasic.iter_pp "," (ipp_ml_type pcontext 0) in
             fprintf ppf "@[" ;
	     begin
               match args with
	        | [] -> ()
	        | [ty1] -> fprintf ppf "%a@ " (ipp_ml_type pcontext 3) ty1
	        | tyl -> fprintf ppf "@[<1>(%a)@]@ " iter_ipp_ml_type tyl
	     end ;
	     (* Print the type name enclosed by the tag *)
	     let type_tag = create_type_tag () in
	     let enclose = (
	       if Path.equal !path (Path.Pident Ident.string_ident) then "\""
	       else if Path.equal !path (Path.Pident Ident.char_ident) then "'"
	       else "") in
	     if is_phi_type_empty appr then
	       fprintf ppf "%a@]" Path.pp_path !path
	     else
	       if is_phi_type_visible appr then
		 fprintf ppf "@<0>%s%a@<0>%s@,@[<1>[%a]@]@]"
	               ("\006"^ type_tag ^"\007") Path.pp_path !path "\006\007"
                       (ipp_phi_type enclose pcontext) appr
	       else
		 fprintf ppf "@<0>%s%a@<0>%s@,@]"
	             ("\006"^ type_tag ^"\007") Path.pp_path !path "\006\007" ;
	     (* Tag stuff *)
	     begin
	     match pcontext.Printcontext.root_type with
	      | Printcontext.Nothing -> ()
	      | root_type ->
		  (* Create the callback function *)
		  let callback = make_callback pcontext appr ppf root_type in
		  (* And now bind and configure the tag *)
		  Text.tag_configure pcontext.Printcontext.widget type_tag
		                     [Foreground (NamedColor "gray45");
			  	      Underline true] ;
		  Text.tag_bind pcontext.Printcontext.widget type_tag
	                        [([], ButtonPressDetail 1)]
				(BindSet ([], callback))
	     end
	 | Typecore.Ttuple (args) ->
	     let iter_ipp_ml_type =
	       Printbasic.iter_pp " *" (ipp_ml_type pcontext 3) in
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
and iexplain_ml_type_abbrev pcontext prio ppf ml_ty =
 let ml_ty = Typecore.ml_type_repr ml_ty in
 match ml_ty.Typecore.mte_info with
  | Typecore.Aliased _ | Typecore.Notseen | Typecore.Seen _ -> assert false
  | _ ->
      match ml_ty.Typecore.mte_desc with
      | Typecore.Tvar -> ()  (* Done above *)
      | Typecore.Tarrow (neg, pos, effect) ->
	  if prio >= 2 then fprintf ppf "@[<1>(" else fprintf ppf "@[" ;
	  let type_tag = create_type_tag () in
	  if is_phi_type_empty effect then
	    (* Because effect is empty, we don't put any anchor *)
	    fprintf ppf "%a@ ->@ %a" (ipp_ml_type pcontext 2) neg
                                     (ipp_ml_type pcontext 1) pos
	  else
	    fprintf ppf "%a@ @[@<0>%s-{@<0>%s@[%a@]@<0>%s}->@<0>%s@]@ %a"
                    (ipp_ml_type pcontext 2) neg
	            ("\006"^ type_tag ^"\007") "\006\007"
	            (ipp_phi_type "" pcontext) effect
	            ("\006"^ type_tag ^"\007") "\006\007"
                    (ipp_ml_type pcontext 1) pos ;
	  if prio >= 2 then fprintf ppf ")@]" else fprintf ppf "@]"
      | Typecore.Tconstr (path, args, appr) ->
	  let iter_ipp_ml_type =
	    Printbasic.iter_pp "," (ipp_ml_type pcontext 0) in
          fprintf ppf "@[" ;
	  begin
          match args with
	   | [] -> ()
	   | [ty1] -> fprintf ppf "%a@ " (ipp_ml_type pcontext 3) ty1
	   | tyl -> fprintf ppf "@[<1>(%a)@]@ " iter_ipp_ml_type tyl
	  end ;
	  (* Print the type name enclosed by the tag *)
	  let type_tag = create_type_tag () in
	  let enclose = (
	    if Path.equal !path (Path.Pident Ident.string_ident) then "\""
	    else if Path.equal !path (Path.Pident Ident.char_ident) then "'"
	    else "") in
	  if is_phi_type_empty appr then
	    fprintf ppf "%a@]" Path.pp_path !path
	  else
	    if is_phi_type_visible appr then
	      fprintf ppf "@<0>%s%a@<0>%s@,@[<1>[%a]@]@]"
	            ("\006"^ type_tag ^"\007") Path.pp_path !path "\006\007"
                    (ipp_phi_type enclose pcontext) appr
	    else
	      fprintf ppf "@<0>%s%a@<0>%s@,@]"
	             ("\006"^ type_tag ^"\007") Path.pp_path !path "\006\007" ;
	  (* Tag stuff *)
	  begin
	  match pcontext.Printcontext.root_type with
	   | Printcontext.Nothing -> ()
	   | root_type ->
	       (* Create the callback function *)
	       let callback = make_callback pcontext appr ppf root_type in
	       (* And now bind and configure the tag *)
	       Text.tag_configure pcontext.Printcontext.widget type_tag
		                  [Foreground (NamedColor "gray45");
		 	           Underline true] ;
	       Text.tag_bind pcontext.Printcontext.widget type_tag
	                     [([], ButtonPressDetail 1)]
			      (BindSet ([], callback))
	  end
      | Typecore.Ttuple (args) ->
	  let iter_ipp_ml_type =
	    Printbasic.iter_pp " *" (ipp_ml_type pcontext 3) in
	  if prio >= 3 then fprintf ppf "@[<1>(%a)@]" iter_ipp_ml_type args
          else fprintf ppf "@[%a@]" iter_ipp_ml_type args



(* was_smthg_before : bool disant si on a imprime qq chose le coup d'avant *)
and iter_ipp_phi_elem enclose pcontext was_smthg_before ppf = function
 | [] -> was_smthg_before
 | h :: rem ->
     let something_printed = 
       begin
       match h with
        | Typecore.Constant (name, presence) ->
	    if not (is_presence_empty presence) then
	      begin
	      if was_smthg_before then fprintf ppf ";@ " ;
	      fprintf ppf "%s%s%s%a" enclose name enclose pp_presence presence;
	      true
	      end
	    else was_smthg_before
	| Typecore.Param (name, ml_type_expr) ->
	    if not (is_ml_type_empty ml_type_expr) then
	      begin
	      if was_smthg_before then fprintf ppf ";@ " ;
	      fprintf ppf "@[<1>%s@ @[<1>(%a)@]@]" name
                       (ipp_ml_type pcontext 0) ml_type_expr ;
	      true
	      end
	    else was_smthg_before
	| Typecore.Record fields ->
	    let pp_field ppf (name, ty) =
	      fprintf ppf "@[<1>%s :@ %a@]" name (ipp_ml_type pcontext 0) ty in
	    let iter_pp_field = Printbasic.iter_pp "; " pp_field in
	    fprintf ppf "@[<2>{ %a }@]" iter_pp_field fields ;
	    true
       end in
     iter_ipp_phi_elem enclose pcontext something_printed ppf rem



and ipp_phi_type enclose pcontext ppf phi =
 let phi = Typecore.phi_repr phi in
 if phi.Typecore.phi_print then
   match phi.Typecore.phi_value with
    | Typecore.Pbottom -> fprintf ppf "_"
    | Typecore.Pexplicit (pel, rv) ->
	let was_smthg_before =
	  iter_ipp_phi_elem enclose pcontext false ppf pel in
	match rv.Typecore.row_info with
	 | Typecore.Notseen | Typecore.Aliased _ -> assert false
	 | Typecore.Abbreviated abb_name ->
	     (* Add a ";" only if needed (something before and after) *)
	     if abb_name <> "" && was_smthg_before then fprintf ppf ";@ " ;
	     fprintf ppf "%s" abb_name
	 | Typecore.Seennotrec ->
	     let abb_name =
	       if rv.Typecore.row_position = 1 && (not !force_vars_print)
	       then ""
	       else create_row_variable_name
                          (rv.Typecore.row_level = Typecore.generic_level) in
	     (* Add a ";" only if needed (something before and after) *)
	     if abb_name <> "" && was_smthg_before then fprintf ppf ";@ " ;
	     fprintf ppf "%s" abb_name
	 | Typecore.Seen xtime ->
	     if !xtime <= 0 then assert false ;
	     let abb_name =
	      if rv.Typecore.row_position = 1 && (not !force_vars_print)
	      then ""
	      else create_row_variable_name
                          (rv.Typecore.row_level = Typecore.generic_level) in
	     rv.Typecore.row_info <- Typecore.Abbreviated abb_name ;
	     (* Add a ";" only if needed (something before and after) *)
	     if abb_name <> "" && was_smthg_before then fprintf ppf ";@ " ;
	     fprintf ppf "%s" abb_name



and pp_presence ppf pr =
 let pr = Typecore.presence_repr pr in
 match pr.Typecore.pre_desc with
  | Typecore.Prpre -> (* Don't print "Pre" by default *) ()
  | Typecore.Prvar ->
      match pr.Typecore.pre_info with
       | Typecore.Notseen | Typecore.Aliased _ -> assert false
       | Typecore.Abbreviated abb_name -> fprintf ppf ":%s" abb_name
       | Typecore.Seennotrec ->
	   let abb_name = create_presence_variable_name
	                   (pr.Typecore.pre_level = Typecore.generic_level) in
	   fprintf ppf ":%s" abb_name
       | Typecore.Seen xtime ->
	   if !xtime <= 0 then assert false ;
	   let abb_name = create_presence_variable_name
	                   (pr.Typecore.pre_level = Typecore.generic_level) in
	   pr.Typecore.pre_info <- Typecore.Abbreviated abb_name ;
	   fprintf ppf ":%s" abb_name




and ipp_abbreviations pcontext ppf abbreviated_tys =
  (* Clear the found abbreviations list *)
  abbreviations := [] ;
  List.iter (fun abbreviated_ty ->
               (* repr has already be done *)
               match abbreviated_ty.Typecore.mte_info with
	        | Typecore.Abbreviated name ->
		    fprintf ppf "@\n  @[@<0>%swhere@<0>%s %s = %a@]"
		                  "\006WHERE\007" "\006\007"
                                  name
                                  (iexplain_ml_type_abbrev pcontext 0)
                                  abbreviated_ty
		| _ -> assert false)
  abbreviated_tys ;
  (* Check if during abbrv printing we have found hidden new abbrevs *)
  let abbreviated_tys2 = !abbreviations in
  (* If yes, go on printing. This must terminate because each time *)
  (* we find something new, we mark it as Abbreviated so when all  *)
  (* is marked Abbreviated, we do not have anything to do...       *)
  if abbreviated_tys2 <> [] then
    ipp_abbreviations pcontext ppf abbreviated_tys2



(* pp_ml_type_scheme: Format.formatter -> Typecore.ml_types_scheme -> unit *)
(* Pretty print a ml types scheme *)
and pp_ml_type_scheme pcontext ppf scheme =
  abbr_counter := 0 ;
  var_type_counter := 0 ;
  var_row_counter := 0 ;
  var_pre_counter := 0 ;
  abbreviations := [] ;
  let pcontext' = { pcontext with 
         Printcontext.root_type = Printcontext.Ml scheme.Typecore.mts_body } in
  update_position_ml_type 1 scheme.Typecore.mts_body ;
  Typecore.clean_ml_type scheme.Typecore.mts_body ;
  update_empty_flag_ml_type scheme.Typecore.mts_body ;
  Typecore.clean_ml_type scheme.Typecore.mts_body ;
  fake_pp_ml_type scheme.Typecore.mts_body ;
  ipp_ml_type pcontext' 0 ppf scheme.Typecore.mts_body ;
  ipp_abbreviations pcontext' ppf (List.rev !abbreviations) ;
  Typecore.clean_ml_type scheme.Typecore.mts_body



(* pp_ml_type: Format.formatter -> Typecore.ml_type_expr -> unit  *)
(* Pretty print a type expresion *)
and pp_ml_type pcontext ppf ty =
  abbr_counter := 0 ;
  var_type_counter := 0 ;
  var_row_counter := 0 ;
  var_pre_counter := 0 ;
  abbreviations := [] ;
  update_position_ml_type 1 ty ;
  Typecore.clean_ml_type ty ;
  update_empty_flag_ml_type ty ;
  Typecore.clean_ml_type ty ;
  fake_pp_ml_type ty ;
  ipp_ml_type pcontext 0 ppf ty ;
  ipp_abbreviations pcontext ppf (List.rev !abbreviations) ;
  Typecore.clean_ml_type ty



(* pp_phi_type: pp_phi_type : Format.formatter -> Typecore.phi_expr -> unit *)
(* Pretty print an approx (phi_expr) expresion *)
and pp_phi_type pcontext ppf phi =
  abbr_counter := 0 ;
  var_type_counter := 0 ;
  var_row_counter := 0 ;
  var_pre_counter := 0 ;
  abbreviations := [] ;
  update_position_phi_type 1 phi ;
  Typecore.clean_phi_type phi ;
  update_empty_flag_phi_type phi ;
  Typecore.clean_phi_type phi ;
  fake_pp_phi_type phi ;
  ipp_phi_type "" pcontext ppf phi ;
  ipp_abbreviations pcontext ppf (List.rev !abbreviations) ;
  Typecore.clean_phi_type phi
;;



let pp_type_declaration pcontext ppf (ty_name, ty_decl) =
  abbr_counter := 0 ;
  var_type_counter := 0 ;
  var_row_counter := 0 ;
  var_pre_counter := 0 ;
  abbreviations := [] ;
  if ty_decl.Typedtree.type_arity <> 0 then
    begin
    (* Ne pas effacer les compteurs entre chaque variable *)
    let iter_pp_ty = Printbasic.iter_pp ","
           (fun ppf t -> fprintf ppf "%a" (ipp_ml_type pcontext 0) t) in
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
                            (ipp_ml_type pcontext 0) sc.Typecore.mts_body)
                 cstrs ;
       ipp_abbreviations pcontext ppf (List.rev !abbreviations) ;
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
       ipp_ml_type pcontext 0 ppf scheme.Typecore.mts_body ;
       ipp_abbreviations pcontext ppf (List.rev !abbreviations) ;
       Typecore.clean_ml_type scheme.Typecore.mts_body
  end ;
  (* Maintenant, on peut nettoyer les paramètres puisque leur nom    *)
  (* a bien été entre la liste préfixe et le corps de la définition. *)
  List.iter Typecore.clean_ml_type ty_decl.Typedtree.type_params
;;

