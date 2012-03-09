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




type 'a assoclist = (Ident.t * 'a) list ;;


(* loaded_compunits_nf_variables:                       *)
(*       Freevars.persistent_binding list assoclist ref *)
(* Assoc list recording all loaded compilation *)
(* units and non free vars they contain.       *)
let loaded_compunits_nf_variables =
  ref ([] : (Freevars.persistent_binding list) assoclist)
;;



(* input: string -> Freevars.persistent_binding list * Typedtree.signature *)
let input module_name =
 (* Creates the name of the corresponding file *)
 let persist_name = Files.cme_filename_from_module_name module_name in
 (* First try current directory, then stdlib directory *)
 let in_channel = Stdlibpath.open_in_with_path persist_name in
 let ((_ : string), 
      (non_free_vars : Freevars.persistent_binding list),
      (_ : Envscope.t),
      (s : Typedtree.signature),
      _,
      (_ : Typedtree.structure)) = input_value in_channel in
 close_in in_channel ;
 (non_free_vars, s)
;;



(* Loads all modules interfaces nofified in the list 'mods'.  *)
(* Avoid loading them several times. Binds their signature in *)
(* the toplevel typing environment.                           *)
(* load : Ident.t list -> unit *)
let load mods =
 List.iter
   (fun mod_id ->
       (* First, check if the module is already loaded *)
       try let _ =
	 Envtype.find_module (Path.Pident mod_id) !Envtype.global_type_env in
         ()
       with Envtype.Type_error _ ->
        (* Not already loaded, so try to load it *)
        try
        let (non_free_vars, sign) = input (Ident.name mod_id) in
	(* Add the module definition to the global typing environment *)
        Envtype.global_type_env := Envtype.add_module
                                      mod_id
                                      (Typedtree.Tmty_signature sign)
   				      !Envtype.global_type_env ;
	(* Update the list of compulation units  *)
        (* that the current analysed file needs. *)
	loaded_compunits_nf_variables := (mod_id, non_free_vars)
                                  :: !loaded_compunits_nf_variables
        with Sys_error _ ->
         (* If no file, do nothing it will be detected later *) ())
   mods
;;
