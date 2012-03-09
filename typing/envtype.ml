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



type error =
  | Unbound_value of Path.t
  | Unbound_type of Path.t
  | Unbound_module of Path.t
  | Unbound_label of Path.t
  | Unbound_constructor of Path.t
  | Unbound_component of string
;;



exception Type_error of error ;;



(* empty: Typedtree.envt *)
(* Empty environment *)
let empty_env () = {
  Typedtree.values = Ident.emptytbl ;
  Typedtree.types = Ident.emptytbl ;
  Typedtree.modules = Ident.emptytbl ;
  Typedtree.constructors = Ident.emptytbl ;
  Typedtree.labels = Ident.emptytbl }
;;

let empty = empty_env () ;;


(* add_value: Ident.t -> Typecore.ml_types_scheme -> *)
(*            Typedtree.envt -> Typedtree.envt       *)
(* Add a value type in the environment *)
let add_value id ty_scheme env =
  { Typedtree.values = Ident.add id (Typecore.Regular_ident ty_scheme)
                                 env.Typedtree.values ;
    Typedtree.types = env.Typedtree.types ;
    Typedtree.modules = env.Typedtree.modules ;
    Typedtree.constructors = env.Typedtree.constructors ;
    Typedtree.labels = env.Typedtree.labels }
;;



(* add_type: Ident.t -> Scopedtree.type_declaration -> Typedtree.envt -> *)
(*           Typedtree.envt                                              *)
(* Add a type declaration in the environment *)
let add_type id decl env =
  { Typedtree.values = env.Typedtree.values ;
    Typedtree.types = Ident.add id decl env.Typedtree.types ;
    Typedtree.modules = env.Typedtree.modules ;
    Typedtree.constructors = env.Typedtree.constructors ;
    Typedtree.labels = env.Typedtree.labels }
;;



(* add_module: Ident.t -> Typedtree.module_type -> Typedtree.envt -> *)
(*             Typedtree.envt                                        *)
(* Add a module type in the environment *)
let add_module id mty env =
  { Typedtree.values = env.Typedtree.values ;
    Typedtree.types = env.Typedtree.types ;
    Typedtree.modules = Ident.add id mty env.Typedtree.modules ;
    Typedtree.constructors = env.Typedtree.constructors ;
    Typedtree.labels = env.Typedtree.labels }
;;



(* add_constructor: Ident.t -> Typedtree.constructor_description -> *)
(*                  Typedtree.envt -> Typedtree.envt                *)
(* Add a constructor in the environment *)
let add_constructor id cstr_desc env =
  { Typedtree.values = env.Typedtree.values ;
    Typedtree.types = env.Typedtree.types ;
    Typedtree.modules = env.Typedtree.modules ;
    Typedtree.constructors =
                     Ident.add id cstr_desc env.Typedtree.constructors ;
    Typedtree.labels = env.Typedtree.labels }
;;



(* add_label: Ident.t -> Typedtree.label_description -> Typedtree.envt -> *)
(*            Typedtree.envt                                              *)
(* Add a constructor in the environment *)
let add_label id cstr_desc env =
  { Typedtree.values = env.Typedtree.values ;
    Typedtree.types = env.Typedtree.types ;
    Typedtree.modules = env.Typedtree.modules ;
    Typedtree.constructors = env.Typedtree.constructors ;
    Typedtree.labels = Ident.add id cstr_desc env.Typedtree.labels }
;;



(* add_spec: Typedtree.signature_item -> Typedtree.envt -> Typedtree.envt *)
(* Add a general component in the current environment *)
let add_spec item env =
 match item with
  | Typedtree.Tsig_value (id, vty) -> add_value id vty env
  | Typedtree.Tsig_type (id, decl) -> add_type id decl env
  | Typedtree.Tsig_module (id, mty) -> add_module id mty env
  | Typedtree.Tsig_constructor (id, cstr_desc) ->
      add_constructor id cstr_desc env
  | Typedtree.Tsig_label (id, lbl_desc) -> add_label id lbl_desc env
;;



(* add_signature: Typedtree.signature_item list -> Typedtree.envt -> *)
(*                Typedtree.envt                                     *)
(* Add a whole module signature in the environment *)
let rec add_signature sg env =
  match sg with
   | [] -> env
   | item :: rem -> add_signature rem (add_spec item env)
;;



(* find_field_value: Path.t -> string -> Typedtree.signature -> *)
(*                   Typecore.ml_types_scheme                   *)
let rec find_field_value p field = function
  | [] -> raise (Type_error (Unbound_component field))
  | Typedtree.Tsig_value (id, ty_scheme) :: _ when Ident.name id = field ->
      Typecore.specialize_ml_type ty_scheme
  | Typedtree.Tsig_type (_, _) :: rem -> find_field_value p field rem
  | Typedtree.Tsig_module (_, _) :: rem -> find_field_value p field rem
  | _ :: rem -> find_field_value p field rem



(* find_field_type: Path.t -> string -> Typedtree.signature -> *)
(*                  Scopedtree.type_declaration                *)
and find_field_type p field = function
  | [] -> raise (Type_error (Unbound_component field))
  | Typedtree.Tsig_type (id, ty_decl) :: _ when Ident.name id = field ->
      ty_decl
  | Typedtree.Tsig_type (_, _) :: rem -> find_field_type p field rem
  | Typedtree.Tsig_module (_, _) :: rem -> find_field_type p field rem
  | _ :: rem -> find_field_type p field rem



(* find_field_module: Path.t -> string -> Typedtree.signature -> *)
(*                    Typedtree.module_type                      *)
and find_field_module p field = function
  | [] -> raise (Type_error (Unbound_component field))
  | Typedtree.Tsig_module (id, mty) :: _ when Ident.name id = field -> mty
  | Typedtree.Tsig_type (_, _) :: rem -> find_field_module p field rem
  | Typedtree.Tsig_module (_, _) :: rem -> find_field_module p field rem
  | _ :: rem -> find_field_module p field rem



(* find_field_constructor: Path.t -> string -> Typedtree.signature -> *)
(*                         Typedtree.constructor_description          *)
and find_field_constructor p field = function
  | [] -> raise (Type_error (Unbound_component field))
  | Typedtree.Tsig_constructor (id, cstr_desc) :: _
			       when Ident.name id = field ->
      cstr_desc
  | Typedtree.Tsig_type (_, _) :: rem ->
      find_field_constructor p field rem
  | Typedtree.Tsig_module (_, _) :: rem ->
      find_field_constructor p field rem
  | _ :: rem -> find_field_constructor p field rem



(* find_field_label: Path.t -> string -> Typedtree.signature -> *)
(*                   Typedtree.label_description                *)
and find_field_label p field = function
  | [] -> raise (Type_error (Unbound_component field))
  | Typedtree.Tsig_label (id, lbl_desc) :: _ when Ident.name id = field ->
      lbl_desc
  | Typedtree.Tsig_type (_, _) :: rem -> find_field_label p field rem
  | Typedtree.Tsig_module (_, _) :: rem -> find_field_label p field rem
  | _ :: rem -> find_field_label p field rem



(* find_value: Path.t -> Typedtree.envt -> Typecore.ml_type_expr *)
and find_value path env =
 match path with
  | Path.Pident id ->
      begin
      try let binding = Ident.find id env.Typedtree.values in
          match binding with
           | Typecore.Regular_ident ty_scheme ->
	       Typecore.specialize_ml_type ty_scheme
	   | Typecore.Mu_ident (scheme, occurences) ->
	       let t = Typecore.specialize_ml_type scheme in
	       let newocc = {
		 Typecore.occ_level = Typecore.get_current_binding_level () ;
		 Typecore.occ_type = t } in
	       occurences := newocc :: !occurences ;
	       t
      with Not_found -> raise (Type_error (Unbound_value path))
      end
  | Path.Pdot (root, field) ->
      match find_module root env with
       | Typedtree.Tmty_signature sg -> find_field_value root field
(List.rev sg) (* !!! *)
       | _ -> raise (Type_error (Unbound_module root))



(* find_type : Path.t -> Typedtree.envt -> Typedtree.type_declaration *)
and find_type path env =
 match path with
  | Path.Pident id ->
      begin
      try Ident.find id env.Typedtree.types
      with Not_found -> raise (Type_error (Unbound_type path))
      end
  | Path.Pdot (root, field) ->
      match find_module root env with
       | Typedtree.Tmty_signature sg -> find_field_type root field
(List.rev sg) (* !!! *)
       | _ -> raise (Type_error (Unbound_module root))



(* find_module : Path.t -> Typedtree.envt -> Typedtree.module_type *)
and find_module path env =
 match path with
  | Path.Pident id ->
      begin
      try Ident.find id env.Typedtree.modules
      with Not_found -> raise (Type_error (Unbound_module path))
      end
  | Path.Pdot (root, field) ->
      match find_module root env with
       | Typedtree.Tmty_signature sg -> find_field_module root field
(List.rev sg) (* !!! *)
       | _ -> raise (Type_error (Unbound_module root))



(* find_constructor:  Path.t -> Typedtree.envt ->       *)
(*                    Typedtree.constructor_description *)
and find_constructor path env =
 match path with
  | Path.Pident id ->
      begin
      try Ident.find id env.Typedtree.constructors
      with Not_found -> raise (Type_error (Unbound_constructor path))
      end
  | Path.Pdot (root, field) ->
      match find_module root env with
       | Typedtree.Tmty_signature sg -> find_field_constructor root field
(List.rev sg) (* !!! *)
       | _ -> raise (Type_error (Unbound_module root))



(* find_label: Path.t -> Typedtree.envt -> Typedtree.label_description *)
and find_label path env =
 match path with
  | Path.Pident id ->
      begin
      try Ident.find id env.Typedtree.labels
      with Not_found -> raise (Type_error (Unbound_label path))
      end
  | Path.Pdot (root, field) ->
      match find_module root env with
       | Typedtree.Tmty_signature sg -> find_field_label root field
(List.rev sg) (* !!! *)
       | _ -> raise (Type_error (Unbound_module root))
;;



(* Functions (used for subtraction) allowing to retrieve the name of  *)
(* a constructor according to wether it is an exception or a variant  *)
(* type constructor. This is needed because in approximation,         *)
(* exception names cannot be used directly. Instead we use the name   *)
(* of the exception followed by its stamp. Why ? Because if 2 modules *)
(* have the same exception name "Exc" then their approximation will   *)
(* be exn[Exc;rho] in both cases. So we can not anymore differentiate *)
(* them. Moreover if the exception has different arguments in these   *)
(* modules (for instance "Exc of int" in the first module and         *)
(* "Exc of string" in the second), if we try to unify 2 exceptions    *)
(* this would fail because we would see that "Exc" is used with int   *)
(* and string, and that would lead to a conflict in the approximation *)
(* part unification.                                                  *)
(* So suffixing the name of the constructor exception with its stamp  *)
(* makes clearly the 2 exceptions *values* different. But now the     *)
(* problem is when we see a constructor in substraction, what is its  *)
(* real name used in the approximation part ? This function answers.  *)
let rec get_field_constructor_real_name p field = function
  | [] -> assert false
  | Typedtree.Tsig_constructor (id, cstr_desc) :: _
				     when Ident.name id = field ->
	 (* Just hack the name if it is an exception *)
	 (match cstr_desc.Typedtree.cstr_kind with
	  | Typedtree.Exn file ->
	      file ^ "__" ^ (Ident.name id) ^ (string_of_int (Ident.stamp id))
	   | Typedtree.Sum -> Ident.name id)
  | _ :: rem -> get_field_constructor_real_name p field rem



(* get_constructor_real_name: Path.t -> Typedtree.envt -> string *)
and get_constructor_real_name path env =
 match path with
  | Path.Pident id ->
      (* Just hack the name if it is an exception *)
      let cstr_decl = Ident.find id env.Typedtree.constructors in
      (match cstr_decl.Typedtree.cstr_kind with
       | Typedtree.Exn file ->
	   file ^ "__" ^ (Ident.name id) ^ (string_of_int (Ident.stamp id))
       | Typedtree.Sum -> Ident.name id)
  | Path.Pdot (root, field) ->
      match find_module root env with
       | Typedtree.Tmty_signature sg ->
	   get_field_constructor_real_name root field sg
       | _ -> assert false
;;






(* global_type_env: Typedtree.envt ref *)
let global_type_env = ref empty ;;



(* load_types: unit -> unit *)
(* Initialize the typing environment with predefined stuff *)
let load_types () = 
  (* ****************************** *)
  (* Basic types with no parameters *)
  global_type_env :=
      List.fold_left (fun accu_env ident ->
                 add_type ident
                          { Typedtree.type_params = [] ;
			    Typedtree.type_arity = 0 ;
			    Typedtree.type_kind = Typedtree.Type_abstract ;
			    Typedtree.type_manifest = None ;
			    (* We don't bother with path sharing because  *)
			    (* basic types pathes will never be modified. *)
			    Typedtree.type_path = ref (Path.Pident ident) }
                          accu_env)
                 !global_type_env
          [ Ident.int_ident; Ident.char_ident; Ident.string_ident;
	    Ident.float_ident; Ident.unit_ident; Ident.bool_ident;
            Ident.exn_ident ] ;
  (* ********** *)
  (* array type *)
  Typecore.begin_definition () ;
  let tyvar = Typecore.type_variable () in
  Typecore.end_definition () ;
  global_type_env := add_type Ident.array_ident
		          { Typedtree.type_params = [tyvar] ;
			    Typedtree.type_arity = 1 ;
			    Typedtree.type_kind = Typedtree.Type_abstract ;
			    Typedtree.type_manifest = None ;
			    (* We don't bother with path sharing because  *)
			    (* basic types pathes will never be modified. *)
			    Typedtree.type_path =
			          ref (Path.Pident Ident.array_ident) }
		          !global_type_env ;
  (* ************************ *)
  (* ('a, 'b, 'c) format type *)
  Typecore.begin_definition () ;
  let tyvar0 = Typecore.type_variable () in
  let tyvar1 = Typecore.type_variable () in
  let tyvar2 = Typecore.type_variable () in
  let string_type = Typecore.type_string_unknown () in
  Typecore.end_definition () ;
  let string_sch = Typecore.generalize_ml_type string_type in
  global_type_env := add_type Ident.format_ident
		          { Typedtree.type_params = [tyvar0;tyvar1;tyvar2] ;
			    Typedtree.type_arity = 3 ;
			    Typedtree.type_kind = Typedtree.Type_abstract ;
			    Typedtree.type_manifest = Some string_sch ;
			    Typedtree.type_path =
				    ref (Path.Pident Ident.format_ident) }
		          !global_type_env ;
  (* ********* *)
  (* list type *)
  let list_path_ref = ref (Path.Pident Ident.list_ident) in
  Typecore.begin_definition () ;
  let tyvar = Typecore.type_variable () in
  Typecore.end_definition () ;
  global_type_env := add_type Ident.list_ident
                          { Typedtree.type_params = [tyvar] ;
			    Typedtree.type_arity = 1 ;
			    Typedtree.type_kind = Typedtree.Type_variant
(*
			          [(Ident.nil_ident, []);
				   (* Pour le type des arguments de ::, on *)
                                   (* ne met rien. En fait c'est faux mais *)
                                   (* on s'en fout car ce champ sert pour  *)
                                   (* générer automatiquement le type des  *)
                                   (* constructeurs d'un type somme, or là *)
                                   (* on va le générer directement a la    *)
                                   (* pogne.                               *)
				   (Ident.cons_ident, [(*See note above*)])] ;
*) [] ;
			    Typedtree.type_manifest = None ;
			    (* We don't bother with path sharing because  *)
			    (* basic types pathes will never be modified. *)
			    Typedtree.type_path = list_path_ref }
                     !global_type_env ;
  (* ******* *)
  (* [] type *)
  Typecore.begin_definition () ;
  let var = Typecore.type_variable () in
		   (* We don't bother with path sharing because basic types  *)
		   (* pathes will never be modified.                         *)
  let appr = Typecore.constant_phi_pre "[]" in
  let nil_ty = Typecore.type_constr
                        (ref (Path.Pident Ident.list_ident))
                        [var] appr in
  Typecore.end_definition () ;
  let nil_sc = Typecore.generalize_ml_type nil_ty in
  global_type_env := add_constructor Ident.nil_ident
                              { Typedtree.cstr_kind = Typedtree.Sum ;
		                Typedtree.cstr_arity = Typedtree.Zeroary ;
				Typedtree.cstr_scheme = nil_sc }
		     !global_type_env ;
  (* :: type *)
  Typecore.begin_definition () ;
  let var = Typecore.type_variable () in
  (* Second part of the type of the argument pair *)
  let snd_arg_ty = (Typecore.type_constr
		            (ref (Path.Pident Ident.list_ident))
		            [var]) (Typecore.empty_phi ()) in
  let arg_ty = Typecore.type_tuple
                [var;
		 snd_arg_ty] in
  let final_appr = Typecore.param_phi "::" arg_ty in
  let res_ty = Typecore.type_constr
                  (ref (Path.Pident Ident.list_ident))
                  [var] final_appr in
  let cons_ty = Typecore.type_arrow arg_ty res_ty (Typecore.empty_phi ()) in
  (* Make the type cyclic *)
  Typecore.unify_ml_type snd_arg_ty res_ty ;
  Typecore.end_definition () ;
  let cons_sc = Typecore.generalize_ml_type cons_ty in
  global_type_env := add_constructor Ident.cons_ident
                              { Typedtree.cstr_kind = Typedtree.Sum ;
                                Typedtree.cstr_arity = Typedtree.Unary ;
				Typedtree.cstr_scheme = cons_sc }
		     !global_type_env ;
  (* () type *)
  Typecore.begin_definition () ;
  let void_ty = Typecore.type_unit () in
  Typecore.end_definition () ;
  let void_sc = Typecore.generalize_ml_type void_ty in
  global_type_env := add_constructor Ident.void_ident
                              { Typedtree.cstr_kind = Typedtree.Sum ;
		                Typedtree.cstr_arity = Typedtree.Zeroary ;
				Typedtree.cstr_scheme = void_sc }
		     !global_type_env ;
  (* true, false type *)
  Typecore.begin_definition () ;
  let true_ty = Typecore.type_bool true in
  Typecore.end_definition () ;
  let true_sc = Typecore.generalize_ml_type true_ty in
  global_type_env := add_constructor Ident.true_ident
                              { Typedtree.cstr_kind = Typedtree.Sum ;
		                Typedtree.cstr_arity = Typedtree.Zeroary ;
				Typedtree.cstr_scheme = true_sc }
		     !global_type_env ;
  Typecore.begin_definition () ;
  let false_ty = Typecore.type_bool false in
  Typecore.end_definition () ;
  let false_sc = Typecore.generalize_ml_type false_ty in
  global_type_env := add_constructor Ident.false_ident
                              { Typedtree.cstr_kind = Typedtree.Sum ;
		                Typedtree.cstr_arity = Typedtree.Zeroary ;
				Typedtree.cstr_scheme = false_sc }
		     !global_type_env ;
  (* Failure type *)
  Typecore.begin_definition () ;
  let arg_ty = Typecore.type_string_unknown () in
  let ty =
    Typecore.type_arrow
              arg_ty
              (Typecore.type_exception
		  ("__Failure"^
                       (string_of_int (Ident.stamp Ident.failure_ident)))
	          (Some arg_ty))
              (Typecore.empty_phi ()) in
  Typecore.end_definition () ;
  let sc = Typecore.generalize_ml_type ty in
  global_type_env := add_constructor Ident.failure_ident
                              { Typedtree.cstr_kind = Typedtree.Exn "" ;
                                Typedtree.cstr_arity = Typedtree.Unary ;
				Typedtree.cstr_scheme = sc }
		     !global_type_env ;
  (* Invalid_argument type *)
  Typecore.begin_definition () ;
  let arg_ty = Typecore.type_string_unknown () in
  let ty =
    Typecore.type_arrow
              arg_ty
              (Typecore.type_exception
                   ("__Invalid_argument"^
                         (string_of_int (Ident.stamp Ident.invarg_ident)))
                   (Some arg_ty))
              (Typecore.empty_phi ()) in
  Typecore.end_definition () ;
  let sc = Typecore.generalize_ml_type ty in
  global_type_env := add_constructor Ident.invarg_ident
                              { Typedtree.cstr_kind = Typedtree.Exn "" ;
                                Typedtree.cstr_arity = Typedtree.Unary ;
				Typedtree.cstr_scheme = sc }
		     !global_type_env ;
  (* End_of_file type *)
  Typecore.begin_definition () ;
  let ty =
    Typecore.type_exception
              ("__End_of_file"^(string_of_int (Ident.stamp Ident.eof_ident)))
              None in
  Typecore.end_definition () ;
  let sc = Typecore.generalize_ml_type ty in
  global_type_env := add_constructor Ident.eof_ident
                              { Typedtree.cstr_kind = Typedtree.Exn "" ;
                                Typedtree.cstr_arity = Typedtree.Zeroary ;
				Typedtree.cstr_scheme = sc }
		     !global_type_env ;
  (* Not_found type *)
  Typecore.begin_definition () ;
  let ty =
    Typecore.type_exception
            ("__Not_found"^(string_of_int (Ident.stamp Ident.not_found_ident)))
            None in
  Typecore.end_definition () ;
  let sc = Typecore.generalize_ml_type ty in
  global_type_env := add_constructor Ident.not_found_ident
                              { Typedtree.cstr_kind = Typedtree.Exn "" ;
                                Typedtree.cstr_arity = Typedtree.Zeroary ;
				Typedtree.cstr_scheme = sc }
		     !global_type_env ;
  (* Division_by_zero type *)
  Typecore.begin_definition () ;
  let ty =
    Typecore.type_exception
           ("__Division_by_zero"^
	         (string_of_int (Ident.stamp Ident.division_by_zero_ident)))
           None in
  Typecore.end_definition () ;
  let sc = Typecore.generalize_ml_type ty in
  global_type_env := add_constructor Ident.division_by_zero_ident
                              { Typedtree.cstr_kind = Typedtree.Exn "" ;
                                Typedtree.cstr_arity = Typedtree.Zeroary ;
				Typedtree.cstr_scheme = sc }
		     !global_type_env ;
  (* Sys_error type *)
  Typecore.begin_definition () ;
  let arg_ty = Typecore.type_string_unknown () in
  let ty =
    Typecore.type_arrow
               arg_ty
               (Typecore.type_exception
		      ("__Sys_error"^
                           (string_of_int (Ident.stamp Ident.sys_error_ident)))
                      (Some arg_ty))
               (Typecore.empty_phi ()) in
  Typecore.end_definition () ;
  let sc = Typecore.generalize_ml_type ty in
  global_type_env := add_constructor Ident.sys_error_ident
                              { Typedtree.cstr_kind = Typedtree.Exn "" ;
                                Typedtree.cstr_arity = Typedtree.Unary ;
				Typedtree.cstr_scheme = sc }
		     !global_type_env ;
  (* Out_of_memory type *)
  Typecore.begin_definition () ;
  let ty =
    Typecore.type_exception
           ("__Out_of_memory"^
	         (string_of_int (Ident.stamp Ident.out_of_mem_ident)))
           None in
  Typecore.end_definition () ;
  let sc = Typecore.generalize_ml_type ty in
  global_type_env := add_constructor Ident.out_of_mem_ident
                              { Typedtree.cstr_kind = Typedtree.Exn "" ;
                                Typedtree.cstr_arity = Typedtree.Zeroary ;
				Typedtree.cstr_scheme = sc }
		     !global_type_env ;
  (* Stack_overflow type *)
  Typecore.begin_definition () ;
  let ty =
    Typecore.type_exception
           ("__Stack_overflow"^
	         (string_of_int (Ident.stamp Ident.stack_over_ident)))
           None in
  Typecore.end_definition () ;
  let sc = Typecore.generalize_ml_type ty in
  global_type_env := add_constructor Ident.stack_over_ident
                              { Typedtree.cstr_kind = Typedtree.Exn "" ;
                                Typedtree.cstr_arity = Typedtree.Zeroary ;
				Typedtree.cstr_scheme = sc }
		     !global_type_env ;
  (* Match_failure type *)
  Typecore.begin_definition () ;
  let arg_ty = Typecore.type_tuple [ Typecore.type_string_unknown ();
				     Typecore.type_int_unknown ();
				     Typecore.type_int_unknown ()] in
  let ty =
    Typecore.type_arrow
              arg_ty
              (Typecore.type_exception
		  ("__Match_failure"^
                       (string_of_int (Ident.stamp Ident.match_failure_ident)))
	          (Some arg_ty))
              (Typecore.empty_phi ()) in
  Typecore.end_definition () ;
  let sc = Typecore.generalize_ml_type ty in
  global_type_env := add_constructor Ident.match_failure_ident
                              { Typedtree.cstr_kind = Typedtree.Exn "" ;
                                Typedtree.cstr_arity = Typedtree.Unary ;
				Typedtree.cstr_scheme = sc }
		     !global_type_env
;;


(* For mu rule *)

(* add_value_rec: Ident.t -> (int * Typecore.ml_type_expr list ref) -> *)
(*                Typedtree.envt -> Typedtree.envt                     *)
(* Add a value type in the environment *)
let add_value_rec id (scheme, occs) env =
  { Typedtree.values = Ident.add id (Typecore.Mu_ident (scheme, occs))
                                 env.Typedtree.values ;
    Typedtree.types = env.Typedtree.types ;
    Typedtree.modules = env.Typedtree.modules ;
    Typedtree.constructors = env.Typedtree.constructors ;
    Typedtree.labels = env.Typedtree.labels }
;;


(* val append: Typedtree.envt -> Typedtree.envt -> Typedtree.envt *)
(* Appends 2 environments, the first one beeing in head of the second one *)
let append first second = {
  Typedtree.values =
      Ident.append first.Typedtree.values second.Typedtree.values ;
  Typedtree.types =
      Ident.append first.Typedtree.types second.Typedtree.types ;
  Typedtree.modules =
      Ident.append first.Typedtree.modules second.Typedtree.modules ;
  Typedtree.constructors =
      Ident.append first.Typedtree.constructors second.Typedtree.constructors ;
  Typedtree.labels =
      Ident.append first.Typedtree.labels second.Typedtree.labels }
;;


let add_value_binding id value_binding env =
  { Typedtree.values = Ident.add id value_binding env.Typedtree.values ;
    Typedtree.types = env.Typedtree.types ;
    Typedtree.modules = env.Typedtree.modules ;
    Typedtree.constructors = env.Typedtree.constructors ;
    Typedtree.labels = env.Typedtree.labels }
;;


let get_raw_value_binding id env =
  try Ident.find id env.Typedtree.values
  with Not_found -> assert false
;;
