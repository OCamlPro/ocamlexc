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




(* Must be re-initialized each time we try *)
(* to find non free variables of a type.   *)
let seen_ml_types = ref ([] : Typecore.ml_type_expr list) ;;
let seen_presence = ref ([] : Typecore.simple_presence list) ;;
let seen_row_vars = ref ([] : Typecore.row_variable list) ;;



(* We must remind non generalized ow variables and presence variables   *)
(* in the same structure, so we inject them into a same data structure. *)
type persistent_binding =
  | Persist_rowv of Typecore.row_variable
  | Persist_pres of Typecore.simple_presence
  | Persist_mlv of Typecore.ml_type_expr
;;



let nfrv_ml_types_scheme accu scheme =

  let rec nfrv_ml_type accu ty =
    let ty = Typecore.ml_type_repr ty in
    match ty.Typecore.mte_info with
     | Typecore.Seen _ -> accu
     | Typecore.Aliased _ | Typecore.Abbreviated _ | Typecore.Seennotrec ->
	 assert false
     | Typecore.Notseen ->
	 ty.Typecore.mte_info <- Typecore.Seen (ref (-2)) ;
	 match ty.Typecore.mte_desc with
          | Typecore.Tvar ->
	      if ty.Typecore.mte_level <> Typecore.generic_level
	      then (Persist_mlv ty) :: accu
	      else accu
	  | Typecore.Tarrow (neg, pos, eff) ->
	      let accu' = nfrv_ml_type accu neg in
	      let accu'' = nfrv_ml_type accu' pos in
	      nfrv_phi_expr accu'' eff
	  | Typecore.Ttuple args -> List.fold_left nfrv_ml_type accu args
	  | Typecore.Tconstr (_, args, appr) ->
	      let accu' = List.fold_left nfrv_ml_type accu args in
	      nfrv_phi_expr accu' appr


  and nfrv_phi_expr accu phi =
    let phi = Typecore.phi_repr phi in
    match phi.Typecore.phi_value with
     | Typecore.Pexplicit row -> nfrv_row accu row
     | Typecore.Pbottom -> accu


  and nfrv_row accu (pel, rv) =
    let accu' = List.fold_left nfrv_phi_elem accu pel in
    match rv.Typecore.row_info with
     | Typecore.Seen _ -> accu'
     | Typecore.Aliased _ | Typecore.Abbreviated _ | Typecore.Seennotrec ->
	 assert false
     | Typecore.Notseen ->
	 rv.Typecore.row_info <- Typecore.Seen (ref (-2)) ;
	 if rv.Typecore.row_level = Typecore.generic_level then accu'
	 else (Persist_rowv rv) :: accu'


  and nfrv_phi_elem accu = function
    | Typecore.Constant (_, presence) -> nfrv_presence accu presence
    | Typecore.Param (_, ty) -> nfrv_ml_type accu ty
    | Typecore.Record fields ->
	List.fold_left (fun accu (_, ty) -> nfrv_ml_type accu ty) accu fields


  and nfrv_presence accu pre =
    let pre = Typecore.presence_repr pre in
    match pre.Typecore.pre_info with
     | Typecore.Seen _ -> accu
     | Typecore.Aliased _ | Typecore.Abbreviated _ | Typecore.Seennotrec ->
	 assert false
     | Typecore.Notseen ->
	 pre.Typecore.pre_info <- Typecore.Seen (ref (-2)) ;
	 match pre.Typecore.pre_desc with
	  | Typecore.Prpre -> accu
	  | Typecore.Prvar ->
	      if pre.Typecore.pre_level = Typecore.generic_level then accu
	      else (Persist_pres pre) :: accu in


  let accu = nfrv_ml_type accu scheme.Typecore.mts_body in
  (* Don't forget to cleanup the type *)
  Typecore.clean_ml_type scheme.Typecore.mts_body ;
  accu
;;



let rec nfrv_signature accu signature =
 List.fold_left nfrv_signature_item accu signature



and nfrv_signature_item accu = function
  | Typedtree.Tsig_value (_, scheme) -> nfrv_ml_types_scheme accu scheme
  | Typedtree.Tsig_type (_, _) -> accu
  | Typedtree.Tsig_module (_, module_ty) ->
      nfrv_module_type accu module_ty
  | Typedtree.Tsig_constructor (_, constr_desc) ->
      nfrv_ml_types_scheme accu constr_desc.Typedtree.cstr_scheme
  | Typedtree.Tsig_label (_, lbl_descr) ->
      nfrv_ml_types_scheme accu lbl_descr.Typedtree.fld_scheme


and nfrv_module_type accu = function
  | Typedtree.Tmty_signature signature ->
      nfrv_signature accu signature
  | Typedtree.Tmty_functor (_, _) -> accu
;;
