open Format ;;

let link_files = ref ([] : string list) ;;




let finish_unification (mod_ident, instanciations) =
 let original_module_vars =
   (try List.assoc mod_ident !Inputpt.loaded_compunits_nf_variables
    with Not_found ->
     fprintf std_formatter "Please load module %s first.\n"
                           (Ident.name mod_ident) ;
     exit (-1)) in
 List.iter2
   (fun bnd1 bnd2 ->
     match (bnd1, bnd2) with
      | (Freevars.Persist_rowv v1, Freevars.Persist_rowv v2) ->
	  Typecore.unify_phi_type
  	              { Typecore.phi_value = Typecore.Pexplicit ([], v1) ;
		        Typecore.phi_print = false ;
			Typecore.phi_empty = true }
	              { Typecore.phi_value = Typecore.Pexplicit ([], v2) ;
			Typecore.phi_print = false ;
			Typecore.phi_empty = true }
      | (Freevars.Persist_pres v1, Freevars.Persist_pres v2) ->
	  Typecore.unify_presence v1 v2
      |	(Freevars.Persist_mlv ty1, Freevars.Persist_mlv ty2) ->
	  Typecore.unify_ml_type ty1 ty2
      |	(_, _) -> assert false)
  original_module_vars instanciations
;;




(* Now the job is :                                                       *)
(*  - load each module (the .cme file)                                    *)
(*  - get its interface and insert it a a module in the environment       *)
(*  - get its non free variables and add it to the global assoc list      *)
(*  - get its instanciations on external compilation units                *)
(*  - for each of these external compilation units, unify these           *)
(*      instanciations with the non instanciated version of the variables *)
(*  At the end of the work, signatures in the typing environment are      *)
(* modified by side effect, and we can read them as final result.         *)
let treat_file filename =
 fprintf std_formatter "...Loading file %s\n" filename ;
 let module_name =
   String.capitalize (Filename.basename (Filename.chop_extension filename)) in
 let module_ident = Ident.create_global module_name in
 let in_channel = Stdlibpath.open_in_with_path filename in
 let (
      (_ : string),
      (non_free_variables :  Freevars.persistent_binding list),
      _,
      (module_sig : Typedtree.signature),
      (external_instanciations :
	         (Freevars.persistent_binding list) Inputpt.assoclist),
      (_ : Typedtree.structure)
     ) = input_value in_channel in
 (* Update the global typing environment *)
 Envtype.global_type_env := Envtype.add_module
	  		                  module_ident
                                          (Typedtree.Tmty_signature module_sig)
		  		          !Envtype.global_type_env ;
 fprintf std_formatter "Module %s added to environment\n" (Ident.name module_ident) ;
 (* Update the assoc list binding modules (compilation units) *)
 (* to their non free variables.                              *)
 Inputpt.loaded_compunits_nf_variables :=
                             (module_ident, non_free_variables)
                             :: !Inputpt.loaded_compunits_nf_variables ;
 (* For all external compilation units, unify the *)
 (* original variables with those we just found.  *)
 List.iter finish_unification external_instanciations
;;
   


let final_result () =
  try
  List.iter
    (fun mod_name ->
      fprintf std_formatter "*********************************\n" ;
      fprintf std_formatter "*** %s *** \n" mod_name ;
      fprintf std_formatter "*********************************\n\n" ;
      let mod_ident = Ident.create_global (String.capitalize mod_name) in
      let mod_path = Path.Pident mod_ident in
      let mod_sig = Envtype.find_module mod_path !Envtype.global_type_env in
      fprintf std_formatter "%a\n" Printmod.pp_module_type mod_sig)
    (List.map (fun n -> Filename.basename (Filename.chop_extension n)) !link_files)
  with Envtype.Type_error err -> Error.handle_typing_error err
;;



let main () =
 Arg.parse
  [ ("-I", Arg.String
                (fun path ->
                  let path = path ^ "/" in
                  Stdlibpath.std_lib_path := path :: !Stdlibpath.std_lib_path),
             "Path to stdlib") ]
  (fun filename -> link_files := filename :: !link_files)
  "Ocamlexc 1.0.1 - Uncaught exceptions analyser - Second pass - (Text output)" ;
  (* Reverse files list to get dependancies order *)
  link_files := List.rev !link_files ;
  (* Initialize environments with builtin definitions *)
  Envscope.load_scopes () ;
  Envtype.load_types () ;
  Stdlibpath.std_lib_path := List.rev ("./" :: !Stdlibpath.std_lib_path) ;
  List.iter treat_file !link_files ;
  final_result ()
;;



main () ;;
