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




open Tk ;;
open Format ;;



let tk_report_error msg =
 Wm.title_set Global.top_w "Error report" ;
 let label_w = Label.create Global.top_w [Text msg] in
 let button_w = Button.create Global.top_w [Text "Quit";
	 				    Command (fun _ -> exit (-1))] in
 pack [label_w; button_w] [] ;
 mainLoop ()
;;




let finish_unification (mod_ident, instanciations) =
 let original_module_vars =
   (try List.assoc mod_ident !Inputpt.loaded_compunits_nf_variables
    with Not_found ->
      tk_report_error ("Please load module "^(Ident.name mod_ident)^" first") ;
      []) in
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
let load_cme_file_in_linker cme_filename =
 let module_name = Files.module_name_from_cme_filename cme_filename in
 let module_ident = Ident.create_global module_name in
 let in_channel = Stdlibpath.open_in_with_path cme_filename in
 (* Loading ... *)
 let (
      (_ : string),
      (non_free_variables :  Freevars.persistent_binding list),
      _,
      (module_sig : Typedtree.signature),
      (external_instanciations :
	         (Freevars.persistent_binding list) Inputpt.assoclist),
      (syntax_tree : Typedtree.structure)
     ) = input_value in_channel in
 close_in in_channel ;
 (* Update the global typing environment *)
 Envtype.global_type_env := Envtype.add_module
	  		                  module_ident
                                          (Typedtree.Tmty_signature module_sig)
		  		          !Envtype.global_type_env ;
 (* Update the assoc list recording trees of modules *)
 Global.syntax_trees := ((Path.Pident module_ident), syntax_tree)
			:: !Global.syntax_trees ;
 (* Update the assoc list recording sources of modules *)
 Global.modules_sources :=
            (module_name , Files.ml_filename_from_cme_filename cme_filename)
            :: !Global.modules_sources ;
 (* Update the assoc list binding modules (compilation units) *)
 (* to their non free variables.                              *)
 Inputpt.loaded_compunits_nf_variables :=
                             (module_ident, non_free_variables)
                             :: !Inputpt.loaded_compunits_nf_variables ;
 (* For all external compilation units, unify the *)
 (* original variables with those we just found.  *)
 List.iter finish_unification external_instanciations
;;
   


let main () =
 Arg.parse
  [ ("-I", Arg.String
                (fun path ->
                  let path = path ^ "/" in
                  Stdlibpath.std_lib_path := path :: !Stdlibpath.std_lib_path),
             "Path to stdlib") ;
    ("-forcevars", Arg.Unit (fun () -> Tkprinttypes.force_vars_print := true),
     "Force \"empty\" vars to be displayed") ]
  (fun filename -> Global.link_files := filename :: !Global.link_files)
  "Ocamlexc 1.0 - Uncaught exceptions analyser - Second pass - (Graphical interface)" ;
  (* Reverse files list to get dependancies order *)
  Global.link_files := List.rev !Global.link_files ;
  try
    (* Initialize environments with builtin definitions *)
    Envscope.load_scopes () ;
    Envtype.load_types () ;
    Stdlibpath.std_lib_path := List.rev ("./" :: !Stdlibpath.std_lib_path) ;
    List.iter load_cme_file_in_linker !Global.link_files ;
    Preferences.load_preferences () ;
    Modselector.create Global.top_w ;
    mainLoop ()
  with
   | Files.Not_ml_file f -> tk_report_error ("File '"^f^"' is not a ML file")
   | Files.Not_cme_file f -> tk_report_error ("File '"^f^"' is not a CME file")
   | Sys_error msg -> tk_report_error msg
;;



main () ;;
