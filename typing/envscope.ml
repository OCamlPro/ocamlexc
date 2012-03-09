(***********************************************************************)
(*                                                                     *)
(*                           Ocamlexc                                  *)
(*                                                                     *)
(*        Francois Pessaux, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(* Syntax identifier scoping stuff. Translate Longidents to Paths.     *)
(***********************************************************************)



let to_load = ref ([] : Ident.t list) ;;


let add_compil_unit ident =
 if not (List.mem ident !to_load) then to_load := ident :: !to_load
;;


let get_compil_units_to_load () =
 (* We must reverse the list because we consed in head each new module we *)
 (* needed. In this way the first module encountered will be loaded first *)
 let r = List.rev !to_load in
 to_load := [] ;
 r
;;


type error =
  | Unbound_value of string
  | Unbound_type of string
  | Unbound_module of string
  | Unbound_label of string
;;


exception Scope_error of error ;;


type scope_binding = Absolute of Ident.t | Opened of Path.t ;;

(* Type of scoping environments : binds string to idents *)
(* We can't use generic environments because they bind   *)
(* idents (not strings) to generic data.                 *)
type t = {
   values : (string * scope_binding) list ;
   types : (string * scope_binding) list ;
   modules : (string * (scope_binding * t)) list ;
   labels : (string * scope_binding) list }
;;


(* val empty: t *)
(* Empty environments *)
let empty = { values = []; types = []; modules = []; labels = [] } ;;


(* val enter_value: string -> t -> t *)
(* Bind a new value ident *)
let enter_value idname sc =
  { values = (idname, Absolute (Ident.create idname)) :: sc.values ;
    types = sc.types ;
    modules = sc.modules ;
    labels = sc.labels }
;;


(* val enter_type: Ident.t -> t -> t *)
(* Bind a new type ident *)
let enter_type idname sc =
  { types = (idname , Absolute (Ident.create idname)) :: sc.types ;
    values = sc.values ;
    modules = sc.modules ;
    labels = sc.labels }
;;


(* val enter_module: Ident.t -> t -> t *)
(* Bind a new module ident *)
let enter_module idname modcomps sc =
  let new_entry = (Absolute (Ident.create idname), modcomps) in
  { modules = (idname, new_entry) :: sc.modules ;
    values = sc.values ;
    types = sc.types ;
    labels = sc.labels }
;;


(* val enter_label: string -> t -> t *)
(* Bind a new field ident *)
let enter_label idname sc =
  { values = sc.values ;
    types = sc.types ;
    modules = sc.modules ;
    labels = (idname, Absolute (Ident.create idname)) :: sc.labels }
;;


(* val scope_value: string -> t -> scope_binding *)
let scope_value idname sc =
  try List.assoc idname sc.values
  with Not_found -> raise (Scope_error (Unbound_value idname))
;;
let retrieve_scoped_value n sc = (* Used to retrieve ident from string *)
 match scope_value n sc with
  | Absolute i -> i
  | Opened _ -> assert false
;;


(* val scope_type: string -> t -> scope_binding *)
let scope_type idname sc =
  try List.assoc idname sc.types
  with Not_found -> raise (Scope_error (Unbound_type idname))
;;
let retrieve_scoped_type n sc = (* Used to retrieve ident from string *)
 match scope_type n sc with
  | Absolute i -> i
  | Opened _ -> assert false
;;



(* val scope_module: string -> t -> scope_binding *)
let scope_module idname sc =
  try fst (List.assoc idname sc.modules)
  with Not_found -> raise (Scope_error (Unbound_module idname))
;;
let retrieve_scoped_module n sc = (* Used to retrieve ident from string *)
 match scope_module n sc with
  | Absolute i -> i
  | _ -> assert false
;;


(* val scope_field: string -> t -> scope_binding *)
let scope_label idname sc =
  try List.assoc idname sc.labels
  with Not_found -> raise (Scope_error (Unbound_label idname))
;;
let retrieve_scoped_label n sc = (* Used to retrieve ident from string *)
 match scope_label n sc with
  | Absolute i -> i
  | Opened _ -> assert false
;;



(* scope_longident: (string -> t -> scope_binding) -> Longident.t ->   *)
(*                  t -> Path.t                                        *)
let rec scope_longident long_ident path sc =
  match path with
   | Longident.Lident id ->
       begin
       try (match long_ident id sc with
            | Absolute ident -> Path.Pident ident | Opened path -> path)
       with Scope_error _ as except ->
	 (* If not found, let's assume it's a compilation unit *)
         (* If not, this will be detected while accessing type environment *)
         try
 	  let global_ident = Ident.create_global id in
	  (* Schedule loading of the compilation unit *)
	  add_compil_unit global_ident ;
	  Path.Pident global_ident
        with Sys_error _ ->
	  (* In fact, there was no file with the module name, so it's *)
	  (* an unbound stuff.                                        *)
	  raise except
       end
   | Longident.Ldot (root, field) ->
       Path.Pdot (scope_longident scope_module root sc, field)
;;



(* value_longident: Longident.t -> t -> Path.t *)
(* Scope a whole value long identifier *)
let value_longident = scope_longident scope_value ;;



(* type_longident: Longident.t -> t -> Path.t *)
(* Scope a whole type long identifier *)
let type_longident = scope_longident scope_type ;;



(* module_longident: Longident.t -> t -> Path.t *)
(* Scope a whole module long identifier *)
let module_longident = scope_longident scope_module ;;



(* label_longident: Longident.t -> t -> Path.t *)
(* Scope a whole module long identifier *)
let label_longident = scope_longident scope_label ;;


(* append: t -> t -> t *)
(* A an extension 'env_exten' of environment to the environment 'sc' *)
let append env_exten sc =
  { modules = env_exten.modules @ sc.modules ;
    values = env_exten.values @ sc.values ;
    types = env_exten.types @ sc.types ;
    labels = env_exten.labels @ sc.labels }
;;



(* global_scope_env: t ref *)
let global_scope_env = ref empty ;;



(* load_scope: unit -> unit *)
let load_scopes () =
 global_scope_env := {
     values = List.combine
		     ["[]"; "::"; "Failure"; "Invalid_argument"; "()";
		      "End_of_file"; "true"; "false"; "Not_found";
		      "Division_by_zero"; "Sys_error"; "Out_of_memory";
		      "Stack_overflow" ; "Match_failure" ]
		     [Absolute Ident.nil_ident; Absolute Ident.cons_ident;
		      Absolute Ident.failure_ident;
                      Absolute Ident.invarg_ident;
		      Absolute Ident.void_ident; Absolute Ident.eof_ident;
		      Absolute Ident.true_ident; Absolute Ident.false_ident;
		      Absolute Ident.not_found_ident;
		      Absolute Ident.division_by_zero_ident ;
                      Absolute Ident.sys_error_ident ;
		      Absolute Ident.out_of_mem_ident ;
		      Absolute Ident.stack_over_ident ;
		      Absolute Ident.match_failure_ident ];
		   (* Basic types *)
     types = List.combine
                ["int"; "char"; "string"; "float"; "unit"; "bool" ; "exn" ;
		 "list"; "array"; "format" ]
              [ Absolute Ident.int_ident; Absolute Ident.char_ident;
		Absolute Ident.string_ident; Absolute Ident.float_ident;
		Absolute Ident.unit_ident; Absolute Ident.bool_ident;
                Absolute Ident.exn_ident ; Absolute Ident.list_ident ;
		Absolute Ident.array_ident ; Absolute Ident.format_ident] ;
     modules = [];
     labels = [] }
;;



(* diff: t -> t -> t *)
(* Return the environment compound of the bindings present in envbig but not *)
(* in envlittle. Assuming envbig is an extension of envlittle, thos returns  *)
(* only bindings that have been added to envlittle so that now it looks like *)
(* envbig. More over, filter components to forget those coming from an open, *)
(* ie those of the form Opened ...                                           *)
let diff envbig envlittle =
 let rec forget_opened = function
   | [] -> []
   | (_, Opened _) :: rem -> forget_opened rem
   | whatever :: rem -> whatever :: forget_opened rem in

 let rec forget_opened2 = function
   | [] -> []
   | (_, (Opened _, _)) :: rem -> forget_opened2 rem
   | whatever :: rem -> whatever :: forget_opened2 rem in

 let rec rec_diff l1 l2 =
  match (l1, l2) with
   | (_, []) -> forget_opened l1
   | (h1::q1, h2::_) ->
       if h1 = h2 then []
       else
         (* Check if the component comes from an open. If yes then forget it *)
	 (match h1 with 
 	   | (_, Opened _) -> rec_diff q1 l2
	   | _ -> h1 :: rec_diff q1 l2)
   | (_, _) -> assert false in

 (* Same than above, but used for modules because not the same type *)
 let rec rec_diff_modules l1 l2 =
  match (l1, l2) with
   | (_, []) -> forget_opened2 l1
   | (h1::q1, h2::_) ->
       if h1 = h2 then []
       else
         (* Check if the component comes from an open. If yes then forget it *)
	 (match h1 with 
 	   | (_, (Opened _, _)) -> rec_diff_modules q1 l2
	   | _ -> h1 :: rec_diff_modules q1 l2)
   | (_, _) -> assert false in

 { values = rec_diff envbig.values envlittle.values ;
   types = rec_diff envbig.types envlittle.types ;
   modules =  rec_diff_modules envbig.modules envlittle.modules ;
   labels = rec_diff envbig.labels envlittle.labels } 
;;



(* To avoid name collision when internal module has the same name than an    *)
(* external one. This is because we made only one environment to map strings *)
(* to scoped ident and to map module names to their internal components list *)
let rec assoc_check_stamp ident = function
  | [] -> raise Not_found
  | (name, (binding, t)) :: rem ->
      if name <> (Ident.name ident) then assoc_check_stamp ident rem
      else
	match binding with
	| Absolute ident' -> if Ident.equal ident ident' then (binding, t)
	                     else assoc_check_stamp ident rem
	| Opened _ -> assoc_check_stamp ident rem
;;



(* input: string -> t *)
(* Loads scope info from the external compilation units called 'module_name' *)
let input module_name =
 (* Creates the name of the corresponding file *)
 let persist_name = (String.uncapitalize  module_name) ^ ".cme" in
 (* First try current directory, then stdlib directory *)
 let in_channel = Stdlibpath.open_in_with_path persist_name in
 let (_,        (* Source file of the module *)
      _,        (* Non free variables for the loaded compilation unit *)
      (s : t),  (* Scope info for the loaded compilation unit *)
      _,        (* Signature for the loaded compilation unit *)
      _,        (* Assoc list recording which other compilation units   *)
		(* this compilation unit needed and what instanciations *)
		(* happened on their non free variables.                *)
      _         (* Syntax tree of the module *)
      ) = input_value in_channel in
 close_in in_channel ;
 s
;;



(* handle_open_directive: t -> Path.t -> t *)
(* Returns an extended scoping environment where components of the *)
(* opened module are present with their good scoping binding.      *)
let handle_open_directive scenv path =
 (* fetch: t -> Path.t -> t *)
 let rec fetch env = function
   | Path.Pident id ->
       begin
       try snd (assoc_check_stamp id env.modules)
       with Not_found ->
          (* Check if it is on the disk as external compilation unit *)
          try input (Ident.name id)
          with Sys_error _ ->
              (* If not, then it's a missing definition *)
              raise (Scope_error (Unbound_module (Ident.name id)))
       end
   | Path.Pdot (path, suffix) ->
       try
	let env' = fetch env path in
	snd (List.assoc suffix env'.modules)
       with Not_found -> raise (Scope_error (Unbound_module suffix)) in
 let opened_env = fetch scenv path in

 (* Now we must take each component and map its name to the full path *)
 let prefix_by_path =
   List.map (fun (n, _) -> (n, Opened (Path.Pdot (path, n)))) in

 let opened_env' = {
   values = prefix_by_path opened_env.values ;
   types = prefix_by_path opened_env.types ;
   modules =
      List.map (fun (n, (_, rec_env)) -> (n, ((Opened (Path.Pdot (path, n))),
					      rec_env)))
               opened_env.modules ;
   labels = prefix_by_path opened_env.labels } in
 append opened_env' scenv
;;



(* mod_longident_evt: Longident.t -> t -> t *)
(* Retrieve internal environment associated with a longident (assumed to *)
(* be a structure's longident) *)
let rec mod_longident_evt lident sc =
  match lident with
   | Longident.Lident id ->
       (* If not found, try on the disk *)
       (try snd (List.assoc id sc.modules)
        with Not_found  -> try input id
                           with Sys_error _ ->
			     raise (Scope_error (Unbound_module id)))
   | Longident.Ldot (root, _) -> mod_longident_evt root sc
;;

