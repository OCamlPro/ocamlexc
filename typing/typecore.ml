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




(* ************************************************************************* *)
(* Data structures                                                           *)


type 'a marker =
  | Notseen
  | Seen of int ref   (* # times the type was encountered. Used for printing *)
                    (* For position-update only : position the type was seen *)
  | Aliased of 'a
  | Abbreviated of string (* Shortcut name given for printing *)
  | Seennotrec            (* The type was seen but is not aliased   *)
                          (* because it is not recursive. Only used *)
                          (* during printing.                       *)
;;


(* ML type expressions *)
(* Must be repr'ed *)
type ml_type_expr = {
  mutable mte_desc: ml_type_desc ;
  mutable mte_level: int ;
  mutable mte_link: ml_type_expr option ;
  mutable mte_info: ml_type_expr marker ;
  mutable mte_position: int ;
  mutable mte_empty: bool
  }

and ml_type_desc =
  | Tvar
  | Tarrow of ml_type_expr * ml_type_expr * phi_expr
  | Ttuple of ml_type_expr list
  | Tconstr of Path.t ref * ml_type_expr list * phi_expr

(* Must be repr'ed *)
and phi_expr = {
  mutable phi_value: row ;
  mutable phi_print: bool ;
  mutable phi_empty: bool }

and row =
  | Pbottom
  | Pexplicit of (phi_elem list * row_variable)

and phi_elem =
  | Constant of string * simple_presence
  | Param of (string * ml_type_expr)
  | Record of (string * ml_type_expr) list (* Sorted by name on the strings *)

and row_variable = {
     mutable row_level: int ;
     mutable row_value: phi_expr option ;
     mutable row_info: row_variable marker ;
     mutable row_position: int
   }

(* Must be repr'ed *)
and simple_presence = {
  mutable pre_desc: simple_presence_desc ;
  mutable pre_level: int ;
  mutable pre_link: simple_presence option ;
  mutable pre_info: simple_presence marker ;
  mutable pre_position: int
  }

and simple_presence_desc =
  | Prpre
  | Prvar
;;



(* ML types schemes *)
type ml_types_scheme = {
  mutable mts_body : ml_type_expr ;
  mutable mts_count : int } ;;


type occurrence = {
  (* level = level auquel on a cree l'occurrence *)
  occ_level : int ;
  (* Type trouve pour l'occurrence *)
  occ_type : ml_type_expr
  } ;;

type value_binding =
 | Regular_ident of ml_types_scheme
 | Mu_ident of ml_types_scheme * occurrence list ref
;;



(* Incompatibility between type terms during unification *)
exception Conflict of ml_type_expr * ml_type_expr ;;

(* Incompatibility between type terms during unification due to    *)
(* circularization. This occurs because of a non regular datatype. *)
exception Non_regular_datatype of ml_type_expr * ml_type_expr ;;


(* ************************************************************************* *)
(* Types representation stuff                                                *)



(* ml_type_repr: ml_type_expr -> ml_type_expr *)
(* We use some path compression *)
let rec ml_type_repr ty =
 match ty with
  | { mte_link = Some t' } ->
      let t' = ml_type_repr t' in
      ty.mte_link <- Some t' ;
      t'
  | _ -> ty
;;



(* phi_repr: phi_expr -> phi_expr *)
(* Lazy on components *)
let rec phi_repr phi =
 match phi.phi_value with
  | Pbottom -> phi
  | Pexplicit (pel, rv) ->
      match rv.row_value with
       | None -> phi
       | Some phi' ->
	   let phi'_repr = phi_repr phi' in
	   match phi'_repr.phi_value with
	    | Pbottom ->
		(* Assume it's done during unification *)
		(* List.iter bottomize_phi_elem pel ; *)
		phi.phi_value <- Pbottom ;
		phi
	    | Pexplicit (pel', rv') ->
		(* Assume lists are sorted *)
		let elems = Sort.merge (<) pel pel' in
		phi.phi_value <- Pexplicit (elems, rv') ;
		phi
;;



(* presence_repr: simple_presence -> simple_presence *)
(* We use path compression *)
let rec presence_repr pre =
 match pre with
  | { pre_link = Some pre' } ->
      let pre' = presence_repr pre' in
      pre.pre_link <- Some pre' ;
      pre'
  | _ -> pre
 ;;



(* ************************************************************************* *)
(* Levels stuff                                                              *)



(* current_binding_level: int ref *)
let current_binding_level = ref 0 ;;

(* generic_level: int *)
let generic_level = 100000000 ;;

(* begin_definition: unit -> unit *)
let begin_definition () = incr current_binding_level ;;

(* end_definition: unit -> unit *)
let end_definition () = decr current_binding_level ;;



(* ml_type_level: int -> t_simple_type -> unit *)
let rec ml_type_level max_level ty =
  let ty = ml_type_repr ty in
  if ty.mte_level > max_level then
   begin
   ty.mte_level <- max_level ;
   match ty.mte_desc with
    | Tvar -> ()
    | Tarrow (neg, pos, effect) ->
	ml_type_level max_level neg ;
	ml_type_level max_level pos ;
	phi_type_level max_level effect ;
    | Ttuple args -> List.iter (ml_type_level max_level) args
    | Tconstr (_, args, appr) ->
	List.iter (ml_type_level max_level) args ;
	phi_type_level max_level appr
   end


and phi_type_level max_level phi =
 let phi = phi_repr phi in
 match phi.phi_value with
  | Pbottom -> ()
  | Pexplicit (pel, rv) ->
      List.iter (phi_elem_level max_level) pel ;
      if rv.row_level > max_level then rv.row_level <- max_level



and phi_elem_level max_level phi_elem =
 match phi_elem with
  | Constant (_, pre) -> presence_level max_level pre
  | Param (_, ty) -> ml_type_level max_level ty
  | Record fields ->
      List.iter (fun (_, ty) -> ml_type_level max_level ty) fields



and presence_level max_level pre =
  let pre = presence_repr pre in
  if pre.pre_level > max_level then pre.pre_level <- max_level
;;



(* ************************************************************************* *)
(* Basic types stuff                                                         *)



let row_variable () = {
  row_level = !current_binding_level ;
  row_value = None ;
  row_info = Notseen ;
  row_position = 1 }
;;

let presence_variable () = {
  pre_desc = Prvar ;
  pre_level = !current_binding_level ;
  pre_link = None ;
  pre_info = Notseen ;
  pre_position = 1 }
;;

let type_variable () = {
  mte_desc = Tvar ;
  mte_level = !current_binding_level ;
  mte_link = None ;
  mte_info = Notseen ;
  mte_position = 1 ;
  mte_empty = true } 
;;


(* presence_present: unit -> simple_presence *)
let presence_present () = {
  pre_desc = Prpre ;
  pre_level = !current_binding_level ;
  pre_link = None ;
  pre_info = Notseen ;
  pre_position = 1 }
;;



(* empty_phi: unit -> phi_expr *)
let empty_phi () = {
  phi_value = Pexplicit ([], row_variable ()) ;
  phi_print = false ;
  phi_empty = true
} ;;

(* bottom_phi: unit -> phi_expr *)
let bottom_phi () = {
  phi_value = Pbottom ;
  phi_print = false ;
  phi_empty = true
} ;;

(* constant_phi_pre: string -> phi_expr *)
let constant_phi_pre cstr_name =
 { phi_value =
      Pexplicit ([Constant (cstr_name, { pre_desc = Prpre ;
					 pre_level = !current_binding_level ;
					 pre_link = None ;
				         pre_info = Notseen ;
				         pre_position = 1 })],
		 row_variable ()) ;
 phi_print = false ;
 phi_empty = true }
;;

(* constant_phi_variable: string -> phi_expr *)
let constant_phi_variable cstr_name =
 { phi_value =
       Pexplicit ([Constant (cstr_name, presence_variable ())],
		  row_variable ()) ;
   phi_print = false ;
   phi_empty = true }
;;

(* param_phi: string -> ml_type_expr -> phi_expr *)
let param_phi cstr_name cstr_arg_ty =
 { phi_value = Pexplicit ([Param (cstr_name, cstr_arg_ty)], row_variable ()) ;
   phi_print = false ;
   phi_empty = true }
;;


(* record_phi: (string * ml_type_expr) list -> phi_expr *)
let record_phi fields_approxs =
  { phi_value = Pexplicit ([Record fields_approxs], row_variable ()) ;
    phi_print = false ;
    phi_empty = true }
;;



(* constant_phielem_variable: string -> phi_elem *)
let constant_phielem_variable cstr_name =
  Constant (cstr_name, presence_variable ())
;;



let int_path = Path.Pident Ident.int_ident ;;
let char_path = Path.Pident Ident.char_ident ;;
let string_path = Path.Pident Ident.string_ident ;;
let float_path = Path.Pident Ident.float_ident ;;
let unit_path = Path.Pident Ident.unit_ident ;;
let bool_path = Path.Pident Ident.bool_ident ;;
let exn_path = Path.Pident Ident.exn_ident ;;
let array_path = Path.Pident Ident.array_ident ;;



(* type_constr: Path.t ref -> ml_type_expr list -> phi_expr -> ml_type_expr *)
let type_constr ty_path ty_args ty_appr = {
  mte_desc = Tconstr (ty_path, ty_args, ty_appr) ;
  mte_level = !current_binding_level ;
  mte_link = None ;
  mte_info = Notseen ;
  mte_position = 1 ;
  mte_empty = true }
;;



(* uncaught_exception: ml_type_expr -> phi_expr *)
(* Extracts the exception approximation from a type. By   *)
(* construction this type <HAS TO BE> an exception type ! *)
(* We don't bother with path sharing because basic types  *)
(* pathes will never be modified.                         *)
let uncaught_exception ty =
 match (ml_type_repr ty).mte_desc with
  | Tconstr (path, [], approx)
        when Path.equal !path (Path.Pident Ident.exn_ident) -> approx
  | _ -> assert false
;;

(* caught_exception: phi_expr -> ml_type_expr *)
(* Make an exception type from a phi. *)
(* We don't bother with path sharing because basic types  *)
(* pathes will never be modified.                         *)
let caught_exception approx =
  type_constr (ref (Path.Pident Ident.exn_ident)) [] approx
;;



let type_int i =
  let pr = { pre_desc = Prpre ; pre_level = !current_binding_level ;
	     pre_link = None ; pre_info = Notseen ;
             pre_position = 1 } in
  let appr = Pexplicit ([Constant ((string_of_int i), pr)], row_variable ()) in
  (* We don't bother with path sharing because basic types  *)
  (* pathes will never be modified.                         *)
  type_constr (ref int_path) []
              { phi_value = appr ; phi_print = false ; phi_empty = true }
;;

let type_int_unknown () =
  let appr = Pexplicit ([], row_variable ()) in
  (* We don't bother with path sharing because basic types  *)
  (* pathes will never be modified.                         *)
  type_constr (ref int_path) []
              { phi_value = appr ; phi_print = false ; phi_empty = true }
;;



let type_char c =
  let pr = { pre_desc = Prpre ; pre_level = !current_binding_level ;
	     pre_link = None ; pre_info = Notseen ;
	     pre_position = 1 } in
  let appr = Pexplicit ([Constant ((Char.escaped c), pr)], row_variable ()) in
  (* We don't bother with path sharing because basic types  *)
  (* pathes will never be modified.                         *)
  type_constr (ref char_path) []
              { phi_value = appr ; phi_print = false ; phi_empty = true }
;;

let type_char_unknown () =
  let appr = Pexplicit ([], row_variable ()) in
  (* We don't bother with path sharing because basic types  *)
  (* pathes will never be modified.                         *)
  type_constr (ref char_path) []
              { phi_value = appr ; phi_print = false ; phi_empty = true }
;;



let type_string s =
  let pr = { pre_desc = Prpre ; pre_level = !current_binding_level ;
	     pre_link = None ; pre_info = Notseen ;
	     pre_position = 1 } in
  let appr = Pexplicit ([Constant (s, pr)], row_variable ()) in
  (* We don't bother with path sharing because basic types  *)
  (* pathes will never be modified.                         *)
  type_constr (ref string_path) []
              { phi_value = appr ; phi_print = false ; phi_empty = true }
;;

let type_string_unknown () =
  let appr = Pexplicit ([], row_variable ()) in
  (* We don't bother with path sharing because basic types  *)
  (* pathes will never be modified.                         *)
  type_constr (ref string_path) []
              { phi_value = appr ; phi_print = false ; phi_empty = true }
;;



let type_float _ =
  let appr = Pbottom in
  (* We don't bother with path sharing because basic types  *)
  (* pathes will never be modified.                         *)
  type_constr (ref float_path) []
              { phi_value = appr ; phi_print = false ; phi_empty = true }
;;

let type_float_unknown () =
  let appr = Pexplicit ([], row_variable ()) in
  (* We don't bother with path sharing because basic types  *)
  (* pathes will never be modified.                         *)
  type_constr (ref float_path) []
              { phi_value = appr ; phi_print = false ; phi_empty = true }
;;



let type_unit () =
  let pr = { pre_desc = Prpre ; pre_level = !current_binding_level ;
	     pre_link = None ; pre_info = Notseen ;
	     pre_position = 1 } in
  let appr = Pexplicit ([Constant ("()", pr)], row_variable ()) in
  (* We don't bother with path sharing because basic types  *)
  (* pathes will never be modified.                         *)
  type_constr (ref unit_path) []
              { phi_value = appr ; phi_print = false ; phi_empty = true }
;;



let type_bool b =
  let pr = { pre_desc = Prpre ; pre_level = !current_binding_level ;
	     pre_link = None ; pre_info = Notseen ;
	     pre_position = 1 } in
 let appr = Pexplicit ([Constant (string_of_bool b, pr)], row_variable ()) in
 (* We don't bother with path sharing because basic types  *)
 (* pathes will never be modified.                         *)
 type_constr (ref bool_path) []
             { phi_value = appr ; phi_print = false ; phi_empty = true }
;;

let type_bool_unknown () =
  let appr = Pexplicit ([], row_variable ()) in
  (* We don't bother with path sharing because basic types  *)
  (* pathes will never be modified.                         *)
  type_constr (ref bool_path) []
              { phi_value = appr ; phi_print = false ; phi_empty = true }
;;



let type_exception cstr_name arg_opt =
  (* We don't bother with path sharing because basic types  *)
  (* pathes will never be modified.                         *)
  let appr = (match arg_opt with
               | None -> constant_phi_pre cstr_name
               | Some ty -> param_phi cstr_name ty) in
  type_constr (ref exn_path) [] appr
;;

let type_exception_unknown () =
  let appr = Pexplicit ([], row_variable ()) in
  (* We don't bother with path sharing because basic types  *)
  (* pathes will never be modified.                         *)
  type_constr (ref exn_path) []
              { phi_value = appr ; phi_print = false ; phi_empty = true }
;;



let type_tuple args = {
  mte_desc = Ttuple args ;
  mte_level = !current_binding_level ;
  mte_link = None ;
  mte_info = Notseen ;
  mte_position = 1 ;
  mte_empty = true }
;;

let type_arrow neg_ty pos_ty effect =
{
  mte_desc = Tarrow (neg_ty, pos_ty, effect) ;
  mte_level = !current_binding_level ;
  mte_link = None ;
  mte_info = Notseen ;
  mte_position = 1 ;
  mte_empty = true }
;;

let type_array elem_ty =
 (* We don't bother with path sharing because basic types  *)
 (* pathes will never be modified.                         *)
 (* Approx is always bottom because type is parametrized. *)
 type_constr (ref array_path) [elem_ty] (bottom_phi ())
;;




(* ******************************************************************* *)
(* Cleanup stuff                                                       *)

let rec clean_ml_type ty =
 let ty = ml_type_repr ty in
 match ty.mte_info with
  | Notseen -> ()
  | Abbreviated _ | Aliased _ | Seen _ | Seennotrec ->
      begin
      ty.mte_info <- Notseen ;
      match ty.mte_desc with
       | Tvar -> ()
       | Tarrow (neg, pos, effect) ->
	   clean_ml_type neg ; clean_ml_type pos; clean_phi_type effect
       | Ttuple args -> List.iter clean_ml_type args
       | Tconstr (_, args, appr) ->
	   List.iter clean_ml_type args ;
	   clean_phi_type appr
      end


and clean_phi_type phi =
  let phi = phi_repr phi in
  match phi.phi_value with
   | Pbottom -> ()
   | Pexplicit (pel, rv) ->
       (match rv.row_info with
         | Notseen -> ()
 	 | Abbreviated _ | Seen _ | Aliased _ | Seennotrec ->
	     rv.row_info <- Notseen) ;
       List.iter clean_phi_elem pel
       

and clean_phi_elem = function
  | Constant (_, p) -> clean_presence p
  | Param (_, ty) -> clean_ml_type ty
  | Record fields -> List.iter (fun (_, ty) -> clean_ml_type ty) fields


and clean_presence pre =
  let pre = presence_repr pre in
  match pre.pre_info with
   | Notseen -> ()
   | Abbreviated _ | Aliased _ | Seen _ | Seennotrec -> pre.pre_info <- Notseen
;;




(* ************************************************************************* *)
(* Bottomization stuff                                                       *)




let bottomize_row r =

 let rec ibottomize_row (pel, rv) =
   match rv.row_info with
    | Aliased _ | Abbreviated _ | Seennotrec -> assert false
    | Seen _ -> ()
    | Notseen ->
	rv.row_info <- Seen (ref (-1)) ;
	rv.row_value <- Some
	      { phi_value = Pbottom ; phi_print = false ; phi_empty = true } ;
	List.iter
	(function
           | Constant (_, presence) -> bottomize_presence presence
           | Param (_, ty) -> bottomize_ml_type ty
           | Record fields -> List.iter (fun (_, ty) -> bottomize_ml_type ty)
                                         fields)
	pel

 and bottomize_phi_type phi =
  let phi = phi_repr phi in
   match phi.phi_value with
    | Pexplicit row -> ibottomize_row row
    | Pbottom -> ()


 and bottomize_presence pre =
  let pre = presence_repr pre in
  match pre.pre_desc with
   | Prpre -> ()
   | Prvar -> pre.pre_desc <- Prpre


 and bottomize_ml_type ty =
  let ty = ml_type_repr ty in
  match ty.mte_info with
   | Aliased _ | Abbreviated _ | Seennotrec -> assert false
   | Seen _ -> ()
   | Notseen ->
       ty.mte_info <- Seen (ref (-1)) ;
       match ty.mte_desc with
        | Tarrow (neg, pos, eff) ->
	    bottomize_ml_type neg ;
	    bottomize_ml_type pos ;
	    bottomize_phi_type eff
	| Ttuple args -> List.iter bottomize_ml_type args
	| Tconstr (_, args, appr) ->
	    List.iter bottomize_ml_type args ;
	    bottomize_phi_type appr
	| Tvar -> () in

 (* Let's bottomize really *)
 ibottomize_row r ;
 (* Inject the row in a phi type to clean it up *)
 clean_phi_type
    { phi_value = Pexplicit r ; phi_print = false ; phi_empty = true }
;;



(* ************************************************************************* *)
(* Unification stuff                                                         *)




(* unify_ml_type: ml_type_expr -> ml_type_expr -> unit *)
let rec unify_ml_type ty1 ty2 =
 let ty1 = ml_type_repr ty1 in
 let ty2 = ml_type_repr ty2 in
 if ty1 == ty2 then ()
 else
  match (ty1.mte_desc, ty2.mte_desc) with
   | (Tvar, _) ->
       ml_type_level ty1.mte_level ty2 ;
       ty1.mte_link <- Some ty2
   | (_, Tvar) ->
       ml_type_level ty2.mte_level ty1 ;
       ty2.mte_link <- Some ty1
   | (Tarrow (neg1, pos1, effect1), Tarrow (neg2, pos2, effect2)) ->
       ml_type_level ty1.mte_level ty2 ;
       ty1.mte_link <- Some ty2 ;
       unify_ml_type neg1 neg2 ;
       unify_ml_type pos1 pos2 ;
       unify_phi_type effect1 effect2
   | (Ttuple args1, Ttuple args2) ->
       ml_type_level ty1.mte_level ty2 ;
       ty1.mte_link <- Some ty2 ;
       List.iter2 unify_ml_type args1 args2
   | (Tconstr (name1, args1, appr1), Tconstr (name2, args2, appr2)) ->
       begin
       (* XXX May be, == *)
       if not (Path.equal !name1 !name2) then raise (Conflict (ty1, ty2)) ;
       try
         ml_type_level ty1.mte_level ty2 ;
	 ty1.mte_link <- Some ty2 ;
	 List.iter2 unify_ml_type args1 args2 ;
	 unify_phi_type appr1 appr2
       with Invalid_argument("List.iter2") ->
         (* Indeed it's an arity problem on the constructor *)
         raise (Conflict (ty1, ty2))
       end
   | (_, _) -> raise (Conflict (ty1, ty2))



and unify_phi_type phi1 phi2 =
 let phi1 = phi_repr phi1 in
 let phi2 = phi_repr phi2 in
 if phi1 == phi2 then ()
 else
  match (phi1.phi_value, phi2.phi_value) with
   | (Pexplicit row1, Pexplicit row2) -> unify_row row1 row2
   | (Pexplicit row1, Pbottom) -> bottomize_row row1
   | (Pbottom, Pexplicit row2) -> bottomize_row row2
   | (Pbottom, Pbottom) -> ()


and unify_row row1 row2 =
  (* repr at phi level is already done *)
  match (row1, row2) with
   | (([], rv1), ([], rv2)) when rv1 == rv2 -> ()
   | (([], rv1), (_, _)) ->
       (* Var (rv1) something *)
       phi_type_level rv1.row_level
                      { phi_value = Pexplicit row2 ; phi_print = false ;
		        phi_empty = true } ;
       rv1.row_value <- Some { phi_value = Pexplicit row2 ; phi_print = false ;
			       phi_empty = true }
   | ((_, _), ([], rv2)) ->
       (* Var (rv2) something *)
       phi_type_level rv2.row_level
                      { phi_value = Pexplicit row1 ; phi_print = false ;
		        phi_empty = true } ;
       rv2.row_value <- Some { phi_value = Pexplicit row1 ; phi_print = false ;
			       phi_empty = true }
   | ((pel1, rv1), (pel2, rv2)) ->
       let psi0 = List.hd pel1 in let phi0 = (List.tl pel1, rv1) in
       let psi1 = List.hd pel2 in let phi1 = (List.tl pel2, rv2) in
       if unify_phi_elements psi0 psi1
       then (* Si les constructeurs de tete sont egaux,   *)
            (* alors, il faut unifier leurs argument par  *)
            (* effet de bord, puis continuer sur la suite *)
            (* actuelle.                                  *)
            unify_phi_type { phi_value = Pexplicit phi0 ; phi_print = false ;
			     phi_empty = true }
                           { phi_value = Pexplicit phi1 ; phi_print = false ;
			     phi_empty = true }
       else
        begin
        let nv = row_variable () in
        (* Cree la nouvelle variable directement du niveau    *)
        (* minimal entre les 2 variables qu'il y avait avant. *)
	nv.row_level <- min rv1.row_level rv2.row_level ;
        unify_phi_type { phi_value = Pexplicit phi0 ; phi_print = false ;
			 phi_empty = true }
                       { phi_value = Pexplicit  ([psi1], nv) ;
			 phi_print = false ; phi_empty = true } ;
	unify_phi_type { phi_value = Pexplicit phi1 ; phi_print = false ;
			 phi_empty = true }
                       { phi_value = Pexplicit ([psi0], nv) ;
			 phi_print = false ; phi_empty = true }
        end



  and unify_phi_elements pel1 pel2 =
  (* No need to perform repr on each element, it has already be done *)
  (* unify_extensible_record. So DON'T call us without using the     *)
  (* 'unify_extensible_record' function before !                     *)
  match (pel1, pel2) with
    | (Constant (name1, pres1), Constant (name2, pres2)) ->
	if name1 = name2 then (unify_presence pres1 pres2 ; true)
                         else false
    | (Param (name1, st1), Param (name2, st2)) ->
        if name1 = name2 then (unify_ml_type st1 st2 ; true) else false
    | (Record fields1, Record fields2) ->
	(* First, we check that fields are well the same. Then we unify. *)
	(* We assume they are sorted in the same way.                    *)
	(* Because approximations are implicitely typed, this should     *)
	(* never fail. More over, by the way we unify, a record approx   *)
	(* should always be an extensible record of length = 1 at max.   *)
	if List.for_all2 (fun (n1, _) (n2, _) -> n1 = n2) fields1 fields2
	 then (List.iter2 (fun (_, ty1) (_, ty2) -> unify_ml_type ty1 ty2)
	                  fields1 fields2 ; true)
	 else false
    | _ -> false



  and unify_presence p1 p2 =
   let p1 = presence_repr p1 in
   let p2 = presence_repr p2 in
   if p1 == p2 then ()
   else
    match (p1.pre_desc, p2.pre_desc) with
     | (Prvar, _) ->
        presence_level p1.pre_level p2 ;
        p1.pre_link <- Some p2
     | (_, Prvar) ->
        presence_level p2.pre_level p1 ;
        p2.pre_link <- Some p1
     | (Prpre, Prpre) -> ()
;;




(* ************************************************************************* *)
(* Specialization stuff                                                      *)




(* specialize_ml_type: ml_type_expr -> ml_type_expr *)
let (specialize_ml_type, specialize_ml_type2) =

 (* Function which really recurse on type structure. All calls share *)
 (* the same seen_ml_types list.                                     *)
 let rec internal_copy_ml_type ty =
  let ty = ml_type_repr ty in
  match ty.mte_info with
   | Seen _ | Abbreviated _ | Seennotrec -> assert false
   | Aliased ty' -> ty'    (* Already seen *)
   | Notseen ->
       (* If type isn't generalized, result of the copy is the type itself *)
       if ty.mte_level <> generic_level
       then (ty.mte_info <- Aliased ty ; ty)
	 (* Else we must make a new instance of each generalized variables *)
       else
	 begin
	 (* We must say we already saw ourself before recursing on the type  *)
	 (* structure, otherwise we can loop. Because we don't still know    *)
	 (* our real value, we will refer ourself with a type variable. Then *)
	 (* when copy is finished, we'll put a link between this variable    *)
	 (* and the real result found for the copy.                          *)
	 let tmp_ty =
	   { mte_desc = Tvar ; mte_level = ty.mte_level ;
	     mte_link = None ; mte_info = Notseen ;
	     mte_position = 1 ; mte_empty = true } in
	 ty.mte_info <- Aliased tmp_ty ;
	 (* And now let's recurse to get the copy of our description... *)
	 let copied_ty_desc =
	 (match ty.mte_desc with
	  | Tvar -> Tvar
	  | Tarrow (neg, pos, effect) ->
              Tarrow (internal_copy_ml_type neg,
		      internal_copy_ml_type pos,
		      copy_phi_type effect)
	  | Ttuple args -> Ttuple (List.map internal_copy_ml_type args)
	  | Tconstr (name, args, appr) ->
	      let args' = List.map internal_copy_ml_type args in
              let appr' = copy_phi_type appr in
              Tconstr (name, args', appr')) in
	 (* Build the type expression copy of ourself *)
	 let copied_ty =
	   { mte_desc = copied_ty_desc ; mte_level = !current_binding_level ;
	     mte_link = None ; mte_info = Notseen ;
	     mte_position = 1 ; mte_empty = true } in
	 (* And now add the famous link *)
	 tmp_ty.mte_link <- Some copied_ty ;
	 (* Return our copy *)
	 copied_ty
	 end


 and copy_phi_type phi =
   match phi.phi_value with
   | Pbottom ->
       { phi_value = Pbottom ; phi_print = false ; phi_empty = true }
   | Pexplicit (pel, rv) ->
       let pel' = List.map copy_phi_elem pel in
       let rv' = (if rv.row_level <> generic_level then rv
	          else
	           match rv.row_info with
		    | Seen _ | Abbreviated _ | Seennotrec -> assert false
		    | Aliased rv'' -> rv''
		    | Notseen ->
			let rv'' = row_variable () in
			rv.row_info <- Aliased rv'' ;
			rv'') in
       { phi_value = Pexplicit (pel', rv') ; phi_print = false ;
	 phi_empty = true }


 and copy_phi_elem = function
   | Constant (name, p) -> Constant (name, copy_presence p)
   | Param (n, ty) -> Param (n, internal_copy_ml_type ty)
   | Record fields ->
       let copied_fields =
	 List.map (fun (n, ty) -> (n, internal_copy_ml_type ty)) fields in
       Record (copied_fields)


   and copy_presence pre =
    let pre = presence_repr pre in
    match pre.pre_info with
     | Seen _ | Abbreviated _ | Seennotrec -> assert false
     | Aliased pre' -> pre'
     | Notseen ->
         match pre.pre_desc with
          | Prpre -> pre
          | Prvar ->
	         (* Take an instance of the variable only *)
                 (* if it is generalized.                 *)
                 let n = if pre.pre_level <> generic_level then pre
		         else presence_variable () in
		 pre.pre_info <- Aliased n ; n in

 (* Generalisation d'un schema *)
 (fun ty_scheme_to_spec ->
   let instance = internal_copy_ml_type ty_scheme_to_spec.mts_body in
   clean_ml_type ty_scheme_to_spec.mts_body ;
   instance),

 (* Generalisation d'un schema et generalisation d'une liste de types      *)
 (* consideres comme des schemas en meme temps (par de rupture de partage) *)
 (fun ty_scheme_to_spec other_types ->
   let instance = internal_copy_ml_type ty_scheme_to_spec.mts_body in
   let other_types' = List.map internal_copy_ml_type other_types in
   clean_ml_type ty_scheme_to_spec.mts_body ;
   List.iter clean_ml_type other_types ;
   (instance, other_types'))
;;




(* ************************************************************************* *)
(* Generalization stuff                                                      *)




(* generalize_ml_type: ml_type_expr -> ml_types_scheme *)
let (generalize_ml_type, generalize_ml_type2) =
 let count = ref 0 in

 let rec find_ml_type_parameters ty =
  let ty = ml_type_repr ty in
  if ty.mte_level > !current_binding_level &&
     ty.mte_level <> generic_level then
    begin
    ty.mte_level <- generic_level ;
    match ty.mte_desc with
     | Tvar -> incr count
     | Tarrow (neg, pos, effect) ->
	 find_ml_type_parameters neg ;
	 find_ml_type_parameters pos ;
	 find_phi_type_parameters effect
     | Ttuple args -> List.iter find_ml_type_parameters args
     | Tconstr (_, args, appr) ->
	 List.iter find_ml_type_parameters args ;
	 find_phi_type_parameters appr
     end


  and find_phi_type_parameters phi =
   let phi = phi_repr phi in
   match phi.phi_value with
    | Pbottom -> ()
    | Pexplicit row -> find_row_parameters row


  and find_row_parameters (pel, rv) =
   List.iter find_phi_elem_parameters pel ;
   if rv.row_level > !current_binding_level &&
      rv.row_level <> generic_level then
     begin
     rv.row_level <- generic_level ;
     incr count
     end


 and find_phi_elem_parameters = function
   | Constant (_, presence) -> find_presence_parameters presence
   | Param (_, ty) -> find_ml_type_parameters ty
   | Record fields ->
       List.iter (fun (_, ty) -> find_ml_type_parameters ty) fields


 and find_presence_parameters presence =
   let presence = presence_repr presence in
   if presence.pre_level > !current_binding_level &&
      presence.pre_level <> generic_level then
     begin
     presence.pre_level <- generic_level ;
     incr count
     end in


 (fun ty_to_gen ->
   (* Levels specifying generalization are updated by side effect *)
   find_ml_type_parameters ty_to_gen ;
   { mts_body = ty_to_gen ; mts_count = !count }),

 (fun ty_to_gen other_types ->
   find_ml_type_parameters ty_to_gen ;
   List.iter find_ml_type_parameters other_types ;
   { mts_body = ty_to_gen ; mts_count = !count }, other_types)
;;



let trivial_ml_type_scheme ty_to_trivialize =
  { mts_body = ty_to_trivialize ; mts_count = 0 }
;;



(* ************************
val unify_all_type_occurences:
  (Path.t ref * ml_type_expr) list -> ml_type_expr -> unit
*)
(* ************************
Recherche toutes les occurences d'un type de nom 'current_pathref' et l'unifie
avec l'accu. En fait, l'accu est trouve s'il existe en consultant l'assoc list
qui mappe un nom de type sur un accu d'unification. Ainsi, si un nom de type
ne fait pas partie des types actuellement definis recursivement, il n'aura pas
de binding dans cette liste d'assoc, et rien ne sera fait pour le circulariser.
*)
let unify_all_type_occurences ass_list search_in_ty =

 let rec descent_ml_type search_in_ty =
  let search_in_ty = ml_type_repr search_in_ty in
  match search_in_ty.mte_info with
   | Aliased _ | Abbreviated _ | Seennotrec -> assert false
   | Seen _ -> ()
   | Notseen ->
       search_in_ty.mte_info <- Seen (ref (-1)) ;
       match search_in_ty.mte_desc with
        | Tvar -> ()
	| Tarrow (neg, pos, effect) ->
	    descent_ml_type neg ;
	    descent_ml_type pos ;
	    descent_phi_type effect
	| Ttuple args -> List.iter descent_ml_type args
	| Tconstr (pathref, args, approx) ->
	    (try
             let accu = List.assq pathref ass_list in
             unify_ml_type search_in_ty accu ;
	     (* Unification may have changed the type and we would *)
	     (* loose the marker. *)
	     (ml_type_repr search_in_ty).mte_info <- Seen (ref (-1)) ;
            with Not_found -> ()) ;
	    List.iter descent_ml_type args ;
	    descent_phi_type approx


 and descent_phi_type phi =
  let phi = phi_repr phi in
  match phi.phi_value with
    | Pexplicit (pel, rv) ->
	(match rv.row_info with
         | Seen _ -> ()
 	 | Aliased _ | Abbreviated _ | Seennotrec -> assert false
	 | Notseen -> rv.row_info <- Seen (ref (-1))) ;
	List.iter descent_phi_elem pel
    | Pbottom -> ()

 and descent_phi_elem = function
   | Constant (_, _) -> ()
   | Param (_, ty) -> descent_ml_type ty
   | Record fields -> List.iter (fun (_, ty) -> descent_ml_type ty) fields in

 descent_ml_type search_in_ty ;
 (* Cleanup is call by the caller !!! *)
;;



(* ************************
val circularize: (Path.t ref * ml_type_expr) list -> ml_type_expr -> unit
*)
(* ************************
Initie la circularisation sur le type d'un constructeur. S'appuie sur tous
les invariants inherents au type des constructeurs.
*)
let circularize accu_list ty =
 let ty = ml_type_repr ty in
 match ty.mte_desc with
  | Tarrow (_, _, _) ->
      (* On est sur que le constructeur a un argument *)
      (* Si la circularization foire a cause d'un pepin d'unification, *)
      (* c'est parce que le type est non regulier (comme par exemple   *)
      (* type 'a clist = Foo of (int clist) clist ;;)                  *)
      (* Si on laisse remonter l'exception Conflict, alors on va se    *)
      (* retrouver dans la merde car le type va etre dans un etat      *)
      (* bizarre, avec des Seen (-1) partout et ca pete les invariants *)
      (* que le printer de type attend. Donc lors de l'impression du   *)
      (* message d'erreur tout va peter. La solution est de rattraper  *)
      (* directement l'exception et d'en relancer une plus precise     *)
      (* adaptee au probleme. Plus proprement, on peut nettoyer les    *)
      (* types d'abord pour les re-injecter dans cette exception.      *)
      begin
      try unify_all_type_occurences accu_list ty with
       | Conflict (ty1, ty2) ->
	   clean_ml_type ty1 ; clean_ml_type ty2 ;
	   raise (Non_regular_datatype (ty1, ty2))
      end
  | _ -> ()
;;


(* ************* *)
(* Mu rule stuff *)
(* ************* *)


let copying_generalization ty =
 let count = ref 0 in

 let rec internal_copy_ml_type ty =
  let ty = ml_type_repr ty in
  match ty.mte_info with
   | Abbreviated _ | Seennotrec | Seen _ -> assert false
   | Aliased ty' -> ty'    (* Already seen *)
   | Notseen ->

   (* If type is not generalizable then let's return itself *)
   if ty.mte_level <= !current_binding_level
   then (ty.mte_info <- Aliased ty ; ty)
   else
    begin
    (* Type is generalizable *)
    let tmp_ty =
      { mte_desc = Tvar ; mte_level = generic_level ;
	mte_link = None ; mte_info = Notseen ;
        mte_position = 1 ; mte_empty = true } in
    ty.mte_info <- Aliased tmp_ty ;
    let copied_ty_desc =
      (match ty.mte_desc with
       | Tvar -> incr count ; Tvar
       | Tarrow (neg, pos, effect) ->
           Tarrow (internal_copy_ml_type neg,
		   internal_copy_ml_type pos,
		   copy_phi_type effect)
       | Ttuple args -> Ttuple (List.map internal_copy_ml_type args)
       | Tconstr (name, args, appr) ->
	   Tconstr (name, List.map internal_copy_ml_type args,
		    copy_phi_type appr)) in
    let copied_ty =
      { mte_desc = copied_ty_desc ; mte_level = generic_level ;
	mte_link = None ; mte_info = Notseen ;
	mte_position = 1 ; mte_empty = true } in
    tmp_ty.mte_link <- Some copied_ty ;
    copied_ty
    end


 and copy_phi_type phi =
  let phi = phi_repr phi in
  match phi.phi_value with
   | Pbottom -> { phi_value = Pbottom ; phi_print = false ; phi_empty = true }
   | Pexplicit (pel, rv) ->
       let pel' = List.map copy_phi_elem pel in
       let rv' =
	 (if rv.row_level <= !current_binding_level then rv
	  else
	    match rv.row_info with
 	     | Abbreviated _ | Seennotrec | Seen _ -> assert false
	     | Aliased rv'' -> rv''
	     | Notseen ->
		 let n = row_variable () in
		 (* Variable is generalized *)
		 incr count ;
		 n.row_level <- generic_level ;
		 rv.row_info <- Aliased n ;
		 n) in
       { phi_value = Pexplicit (pel', rv') ;
	 phi_print = false ; phi_empty = true }


 and copy_phi_elem = function
   | Constant (name, p) -> Constant (name, copy_presence p)
   | Param (n, ty) -> Param (n, internal_copy_ml_type ty)
   | Record fields ->
       let copied_fields =
	 List.map (fun (n, ty) -> (n, internal_copy_ml_type ty)) fields in
       Record (copied_fields)


   and copy_presence pre =
    let pre = presence_repr pre in
    match pre.pre_info with
     | Abbreviated _ | Seennotrec | Seen _ -> assert false
     | Aliased pre' -> pre'
     | Notseen ->
         if pre.pre_level <= !current_binding_level
	 then (pre.pre_info <- Aliased pre ; pre)
         else
          match pre.pre_desc with
           | Prpre -> pre
           | Prvar ->
                  let n = presence_variable () in
		  incr count ;
	          n.pre_level <- generic_level ;
                  pre.pre_info <- Aliased n ; n in


 let body = internal_copy_ml_type ty in
 (* Cleanup *)
 clean_ml_type ty ;
 { mts_body = body ; mts_count = !count }
;;




let rec mem_pair x y = function
 | [] -> false
 | (x', y') :: rem ->
     if x == x' || y == y' then true else mem_pair x y rem
;;



let type_scheme_equal scheme1 scheme2 =
 let seen_ml_pairs = ref [] in
 let seen_phivars_pairs = ref [] in
 let seen_presence_pairs = ref [] in

 if scheme1.mts_count <> scheme2.mts_count then false
 else

  let rec type_equal ty1 ty2 =
   let ty1 = ml_type_repr ty1 in
   let ty2 = ml_type_repr ty2 in
   if mem_pair ty1 ty2 !seen_ml_pairs then true
   else
     begin
     seen_ml_pairs := (ty1, ty2) :: !seen_ml_pairs ;
     match (ty1.mte_desc, ty2.mte_desc) with
       | (Tvar, Tvar) -> true
       | (Tarrow (neg1, pos1, effect1), Tarrow (neg2, pos2, effect2)) ->
	   type_equal neg1 neg2 && type_equal pos1 pos2 &&
	   phi_equal effect1 effect2
       | (Ttuple args1, Ttuple args2) ->
	   List.for_all2 type_equal args1 args2
       | (Tconstr (name1, args1, appr1), Tconstr (name2, args2, appr2)) ->
	   name1 = name2 && phi_equal appr1 appr2 &&
	   List.for_all2 type_equal args1 args2
       | (_, _) -> false
     end

   and phi_equal phi1 phi2 =
    let phi1 = phi_repr phi1 in
    let phi2 = phi_repr phi2 in
    match (phi1.phi_value, phi2.phi_value) with
     | (Pbottom, Pbottom) -> true
     | (Pexplicit (pel1, rv1), Pexplicit (pel2, rv2)) ->
	 if mem_pair rv1 rv2 !seen_phivars_pairs then
	   (try List.for_all2 phi_elem_equal pel1 pel2
	    with Invalid_argument "List.for_all2" -> false)
	 else
	  begin
	  seen_phivars_pairs := (rv1, rv2) :: !seen_phivars_pairs ;
	  (try List.for_all2 phi_elem_equal pel1 pel2
	   with Invalid_argument "List.for_all2" -> false)
	  end
     | (_, _) -> false

  and phi_elem_equal pe1 pe2 =
   match (pe1, pe2) with
    | (Constant (name1, presence1), Constant (name2, presence2)) ->
	name1 = name2 && presence_equal presence1 presence2
    | (Param (name1, ty1), Param (name2, ty2)) ->
	name1 = name2 && type_equal ty1 ty2
    | (Record fields1, Record fields2) ->
	(try List.for_all2 (fun (fname1, fty1) (fname2, fty2) ->
	                          fname1 = fname2 && type_equal fty1 fty2)
	                  fields1 fields2
	  with Invalid_argument "List.for_all2" -> false)
     | (_, _) -> false


  and presence_equal pre1 pre2 =
   let pre1 = presence_repr pre1 in
   let pre2 = presence_repr pre2 in
   match (pre1.pre_desc, pre2.pre_desc) with
    | (Prpre, Prpre) -> true
    | (Prvar, Prvar) ->
	if mem_pair pre1 pre2 !seen_presence_pairs then true
	else
	  begin
	  seen_presence_pairs := (pre1, pre2) :: !seen_presence_pairs ;
          true
	  end
    | (_, _) -> false in

  seen_ml_pairs := [] ;
  seen_phivars_pairs := [] ;
  seen_presence_pairs := [] ;
  let tmp = type_equal scheme1.mts_body scheme2.mts_body in
  tmp
;;




let get_current_binding_level () = !current_binding_level ;;

let specialize_ml_type_with_level level scheme =
  let old_level = !current_binding_level in
  current_binding_level := level ;
  let res = specialize_ml_type scheme in
  current_binding_level := old_level ;
  res
;;



let copying_generalization_with_level level ty =
   let old_level = !current_binding_level in
   current_binding_level := level ;
   let res = copying_generalization ty in
   current_binding_level := old_level ;
   res
;;


 let generalize_ml_type_with_level level ty =
   let old_level = !current_binding_level in
   current_binding_level := level ;
   let res = generalize_ml_type ty in
   current_binding_level := old_level ;
   res
;;

