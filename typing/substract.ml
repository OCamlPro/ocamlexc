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





exception Unused_pattern ;;


(* ************************************************************************* *)
(* Syntactic absorbance stuff                                                *)




let rec is_pattern_absorbant pattern =
 match pattern.Scopedtree.ppat_desc with
  | Scopedtree.Ppat_any -> true
  | Scopedtree.Ppat_var _ -> true
  | Scopedtree.Ppat_alias (pat, _) -> is_pattern_absorbant pat
  | Scopedtree.Ppat_constant _ -> false
  | Scopedtree.Ppat_tuple arg_pats ->
      List.for_all is_pattern_absorbant arg_pats
  | Scopedtree.Ppat_construct (_, _) -> false
  | Scopedtree.Ppat_record labels ->
      List.for_all (fun (_, pat) -> is_pattern_absorbant pat) labels
  | Scopedtree.Ppat_array _ -> false
  | Scopedtree.Ppat_or (pat1, pat2) ->
      is_pattern_absorbant pat1 && is_pattern_absorbant pat2
  | Scopedtree.Ppat_constraint (pat, _) -> is_pattern_absorbant pat
;;




(* ************************************************************************* *)
(* Subtraction stuff                                                         *)




(* Repr is assumed to be done before on the phi containing this row *)
let row_remove_constant string_of_constant (pel, rv) =
 let rec rec_remove = function
  | [] -> raise Unused_pattern
  | elem :: rem ->
      match elem with
       | Typecore.Constant (val_name, _)
                       when val_name = string_of_constant ->
	   let phi_elem = Typecore.constant_phielem_variable val_name in
	   phi_elem :: rem
       | bof -> bof :: rec_remove rem in
 (rec_remove pel, rv)
;;



let string_of_constant = function
  | Asttypes.Const_int i -> string_of_int i
  | Asttypes.Const_char c -> Char.escaped c
  | Asttypes.Const_string s -> s
  | Asttypes.Const_float flstr -> flstr
;;



let substract_constant_approx constant_string phi =
 let phi = Typecore.phi_repr phi in
 match phi.Typecore.phi_value with
  | Typecore.Pexplicit row ->
      { Typecore.phi_value =
	  Typecore.Pexplicit (row_remove_constant constant_string row) ;
	Typecore.phi_print = false ; Typecore.phi_empty = true }
  | Typecore.Pbottom ->
      { Typecore.phi_value = Typecore.Pbottom ;
	Typecore.phi_print = false ; Typecore.phi_empty = true }
;;



(* Repr is assumed to be done before on the phi containing this row *)
let rec row_remove_param env constant_string pat (pel, rv) =
 let rec rec_remove = function
  | [] -> raise Unused_pattern
  | elem :: rem -> 
      match elem with
       | Typecore.Param (val_name, ty)
	 when val_name = constant_string ->
	   let (bindings, new_ty) = substract_ml_type env ty pat in
	   (bindings,
	    (Typecore.Param (val_name, new_ty)) :: rem)
       | _ ->
	   let (bindings, rem') = rec_remove rem in
	   (bindings, elem :: rem') in
 let (b, l) = rec_remove pel in
 (b, (l, rv))




and substract_param_approx env constant_string approx pat =
 let approx = Typecore.phi_repr approx in
 match approx.Typecore.phi_value with
   | Typecore.Pexplicit row ->
       let (bindings, new_row) = 
	 row_remove_param env constant_string pat row in
       (bindings, { Typecore.phi_value = Typecore.Pexplicit new_row ;
		    Typecore.phi_print = false ; Typecore.phi_empty = true })
  | Typecore.Pbottom ->
      (* On the approx side, nothing will change because we can't  *)
      (* subtract anything from an unknown set of values...        *)
      (* For bindings, we will use those found when we basically   *)
      (* typed the pattern. Indeed, it's difficult to re-construct *)
      (* bindings for subpatterns because bottom doen't contains   *)
      (* support types... So at each sub pattern recursion, we     *)
      (* know that approx will be bottom, but we don't know for    *)
      (* which support type.                                       *)
      ([], { Typecore.phi_value = Typecore.Pbottom ;
	     Typecore.phi_print = false ; Typecore.phi_empty = true })



(* Only returns bindings found. We treat record the same way than tuples  *)
(* So we don't return a new approximation (subtracted) because it will    *)
(* only be significant if the record pattern is totally absorbant. And in *)
(* this case, we know that the new approx is a type variable. So we check *)
(* this afterward.                                                        *)
(* Repr is assumed to be done before on the phi containing this row *)
and row_remove_record env field_pats (pel, _) =
 match pel with
  | [] -> raise Unused_pattern
  | [Typecore.Record fld_approx] ->
      List.flatten
        (List.map (fun (name, pattern) ->
                    (* Looks for the approx of the current field *)
                    let fld_ty = List.assoc (Path.end_name name) fld_approx in
                    let (bnds, _) = substract_ml_type env fld_ty pattern in
		    bnds)
                  field_pats)
  | _ -> (* Record approxs are always of length 1 at max, and the only *)
         (* element it contains must invariantly be a Record (_, _) .  *)
         assert false



(* Tries to subtract a list of patterns (assumed to belong to a record *)
(* pattern) from an approx (assumed to be a record approx).            *)
(* Only returns bindings, no new approximation.                        *)
and substract_record_approx env approx field_pats =
 let approx = Typecore.phi_repr approx in
 match approx.Typecore.phi_value with
  | Typecore.Pexplicit extrec ->
      row_remove_record env field_pats extrec
  | Typecore.Pbottom ->
      (* No particular bindings. Those infered by the first *)
      (* typing pass will be sufficient. Same problem than  *)
      (* for construct pattern when used with bottom.       *)
      []



and substract_ml_type env ty pattern =
 let ty = Typecore.ml_type_repr ty in
 match ty.Typecore.mte_desc with
  | Typecore.Tconstr (path, args, approx) ->
      begin
      (* Only authorized patterns : Any, Var, Constant, Construct, *)
      (* Or, Alias, Record, Constraint,Record                      *)
      match pattern.Scopedtree.ppat_desc with
       | Scopedtree.Ppat_any ->
	   let new_ty = Typecore.type_variable () in
	   ([], new_ty)
       | Scopedtree.Ppat_var var_ident ->
	   let new_ty = Typecore.type_variable () in
	   (* Binding is still done with the old type, then we return *)
	   (* the new type, suitable for the next pattern.            *)
	   ([var_ident, ty], new_ty)
       | Scopedtree.Ppat_constant constant ->
	   (* A global invariant waranties that if pattern is a basic *)
	   (* constant, then its type has no parameters ! So we can   *)
           (* always subtract.                                        *)
	   let constant_string = string_of_constant constant in
	   let new_approx = substract_constant_approx constant_string approx in
	   (* Because of the invariant above, args are always [] *)
	   let new_ty = Typecore.type_constr path [] new_approx in
	   ([], new_ty)
       | Scopedtree.Ppat_construct (cstr_path, pat_arg_opt) ->
	   (* Retrieve the complete internal name for this constructor *)
           (* because if it is an exception constructor, its name is a *)
           (* bit modified compared to its path.                       *)
	   let cstr_name = Envtype.get_constructor_real_name cstr_path env in
	   (match pat_arg_opt with
             | None ->
		 (* Works the same way as simple other constants *)
		 (* if the type has no parameters.               *)
		 let new_ty =
		   (if args = [] then
		     let new_approx =
		       substract_constant_approx cstr_name approx in
		       Typecore.type_constr path [] new_approx
		   else ty) in
		 ([], new_ty)
	     | Some pat ->
		 let (rec_bindings, new_approx) = substract_param_approx env
		                                       cstr_name approx pat in
		 let new_ty =
		   (if args = [] then
		     Typecore.type_constr path [] new_approx
		    else ty) in
		 (rec_bindings, new_ty))
       | Scopedtree.Ppat_or (pat1, pat2) ->
	   (* We just act as if this was 2 consecutive different patterns *)
           (* Note that because Or_pat can't bind variables, returned     *)
           (* bindings must be empty. We don't check this invariant...    *)
	   let (bnds1, remain_ty) = substract_ml_type env ty pat1 in
	   let (bnds2, remain_ty') = substract_ml_type env remain_ty pat2 in
	   ((bnds1 @ bnds2), remain_ty')
       | Scopedtree.Ppat_alias (pat, alias) ->
	   (* The simpler way to process would be to bind the alias *)
	   (* to the values that can flow until here. We could be   *)
           (* more precise by checking if the pattern 'pat' is a    *)
	   (* constant and in this case to bind the alias only to   *)
	   (* this constant. In fact we claim that not useful to    *)
	   (* have pain, because this would only be finer in cases  *)
	   (* where user aliased a constant pattern, which is       *)
	   (* rather non commom !                                   *)
	   let (bnds, remain_ty) = substract_ml_type env ty pat in
	   ((alias, ty) :: bnds, remain_ty)
       | Scopedtree.Ppat_constraint (pat, _) ->
	   (* At subtraction time, constraints have already be taken    *)
           (* in account, so all happens as if it was a simple pattern. *)
	   substract_ml_type env ty pat
       | Scopedtree.Ppat_record field_pats ->
	   (* Treated like tuples *)
	   let bindings = substract_record_approx env approx field_pats in
	   (* If pattern is irrefutable, we don't need to descent in the  *)
           (* record structure to clear it's type because records already *)
	   (* are approximated by one row, so by generating a new row     *)
           (* variable, we still avoid inserting ML type variable, and    *)
	   (* this was our aim !                                          *)
	   let new_ty =
	     (if List.for_all (fun (_, p) -> is_pattern_absorbant p) field_pats
                 && args = []
	      then Typecore.type_constr path args (Typecore.empty_phi ())
	      else ty) in
	   (bindings, new_ty)
       | Scopedtree.Ppat_array pat_lst ->
	   (* We can't do any subtraction, we just want bindings. *)
	   (* Generate a type variable to retrieve the array elems type. *)
           let array_elem_ty = Typecore.type_variable () in
	   (* Just get elements type. *)
	   Typecore.unify_ml_type (Typecore.type_array array_elem_ty)
                                  ty ;
	   (* Just get bindings. *)
	   let bindings = List.map
                         (fun p -> fst (substract_ml_type env array_elem_ty p))
	                 pat_lst in
	   (List.flatten bindings, ty)
       | _ -> assert false
      end
  | Typecore.Tvar ->
      begin
      (* We can't subtract anything from a variable. Note that if type  *)
      (* is a variable, then either there is only one pattern var, and  *)
      (* then subtraction will not serve because there won't be any     *)
      (* next pattern. Either there are several patterns, but all vars, *)
      (* so there are unuseful, and then, basta, that's not a damn if   *)
      (* we don't reflect any subtraction...                            *)

      (* Only authorized patterns : Any, Var, Or, Alias, Constraint. *)
      (* Inded if we had found an other pattern, then we wouldn't    *)
      (* anymore be of type variable or that's because the           *)
      (* subtract has suppressed everything and repaced it by a type *)
      (* variable. In this case, that means the pattern is unused.   *)
      match pattern.Scopedtree.ppat_desc with
       | Scopedtree.Ppat_any ->
	   let new_ty = Typecore.type_variable () in
	   ([], new_ty)
       | Scopedtree.Ppat_var var_ident ->
	   let new_ty = Typecore.type_variable () in
	   ([var_ident, ty], new_ty)
       | Scopedtree.Ppat_or (pat1, pat2) ->
	   let (bnds1, remain_ty) = substract_ml_type env ty pat1 in
	   let (bnds2, remain_ty') = substract_ml_type env remain_ty pat2 in
	   ((bnds1 @ bnds2), remain_ty')
       | Scopedtree.Ppat_alias (pat, alias) ->
	   let (bnds, remain_ty) = substract_ml_type env ty pat in
	   ((alias, ty) :: bnds, remain_ty)
       | Scopedtree.Ppat_constraint (pat, _) -> substract_ml_type env ty pat
       | _ -> raise Unused_pattern         (* Check above remark *)
      end
  | Typecore.Tarrow (_, _, _) ->
      (* Because functions can't be explicitely named in patterns, the *)
      (* only basic patterns which can happen are Any, Var, Or, Alias, *)
      (* Constraint.                                                   *)
      begin
      match pattern.Scopedtree.ppat_desc with
       | Scopedtree.Ppat_any ->
	   let new_ty = Typecore.type_variable () in
	   ([], new_ty)
       | Scopedtree.Ppat_var var_ident ->
	   (* Same than above in fact *)
	   let new_ty = Typecore.type_variable () in
	   ([var_ident, ty], new_ty)
       | Scopedtree.Ppat_or (pat1, pat2) ->
	   let (bnds1, remain_ty) = substract_ml_type env ty pat1 in
	   let (bnds2, remain_ty') = substract_ml_type env remain_ty pat2 in
	   ((bnds1 @ bnds2), remain_ty')
       | Scopedtree.Ppat_alias (pat, alias) ->
	   let (bnds, remain_ty) = substract_ml_type env ty pat in
	   ((alias, ty) :: bnds, remain_ty)
       | Scopedtree.Ppat_constraint (pat, _) -> substract_ml_type env ty pat
       | _ -> assert false
      end
  | Typecore.Ttuple arg_tys ->
      begin
      (* Only authorized patterns : Any, Var, Tuple, Or, Alias, Constraint *)
      match pattern.Scopedtree.ppat_desc with
       | Scopedtree.Ppat_any ->
           let new_ty = Typecore.type_variable () in
	   ([], new_ty)
       | Scopedtree.Ppat_var var_ident ->
           let new_ty = Typecore.type_variable () in
	   ([var_ident, ty], new_ty)
       | Scopedtree.Ppat_tuple arg_pats ->
	   (* Find bindings, and get components types once subtracted in. *)
           (* In fact, these will only be significant if the pattern is   *)
           (* irrefutable, otherwise, we abandon them.                    *)
	   let (bindings, new_arg_tys) =
                List.split
	            (List.map2 (substract_ml_type env) arg_tys arg_pats) in
	   (* If the pattern is completely absorbant (i.e with only Var and *)
           (* Any), then we can subtract all, i.e new type is a variable.   *)
	   (* Otherwise we don't subtract anything.                         *)
	   let new_ty = (if List.for_all is_pattern_absorbant arg_pats
	     then Typecore.type_tuple new_arg_tys else ty) in
	   (List.flatten bindings, new_ty)
       | Scopedtree.Ppat_or (pat1, pat2) ->
	   let (bnds1, remain_ty) = substract_ml_type env ty pat1 in
	   let (bnds2, remain_ty') = substract_ml_type env remain_ty pat2 in
	   ((bnds1 @ bnds2), remain_ty')
       | Scopedtree.Ppat_alias (pat, alias) ->
	   let (bnds, remain_ty) = substract_ml_type env ty pat in
	   ((alias, ty) :: bnds, remain_ty)
       | Scopedtree.Ppat_constraint (pat, _) -> substract_ml_type env ty pat
       | _ -> assert false
      end
;;
