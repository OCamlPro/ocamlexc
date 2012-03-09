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



(* Bool telling wether we need to print expressions just after parsing *)
let debug_level1 = ref false ;;
(* Bool telling wether we need to print expressions handler extension *)
let debug_level2 = ref false ;;
(* Bool telling wether we need to print expressions just after labeling *)
let debug_level3 = ref false ;;
(* Bool telling wether we need to print expressions just after scoping *)
let debug_level4 = ref false ;;
(* Bool telling if we must load pervasives in standard or not *)
let std_perv = ref true ;;



(* Main loop *)
let main channel =
  fprintf std_formatter "1.0.1 - Eh, blocking with your head again ?\n" ;
  pp_print_flush std_formatter () ;
  let in_channel = ref channel in
  let lexbuf = ref (Lexing.from_channel !in_channel) in
  let channel_stack = ref ([] : (in_channel * Lexing.lexbuf) list) in
  let inclusions = ref 0 in
  (* First, load Pervasives if needed *)
  if !std_perv then
   begin
   (try
     Envscope.global_scope_env :=
         Envscope.handle_open_directive
                   !Envscope.global_scope_env
                   (Path.Pident (Ident.create_global "Pervasives"))
     with Envscope.Scope_error _ ->
       fprintf std_formatter "Can't find pervasives. Please use -I option.\n" ;
       exit (-1)) ;
    Inputpt.load [Ident.create_global "Pervasives"]
   end ;
  while true do
   try
    fprintf std_formatter "# " ; pp_print_flush std_formatter () ;
    let phrase = Parse.toplevel_phrase !lexbuf in
    (* If debug, print expression before any work *)
    if !debug_level1 then
      begin
      fprintf std_formatter "P> %a\n" Printparse.pp_toplevel_phrase phrase ;
      print_flush ()
      end ;
    (* Extend handlers *)
    let extended_phrase =
               Extendhandler.extend_toplevel_phrase_handler phrase in
    if !debug_level2 then
      begin
      fprintf std_formatter "E> %a\n" Printparse.pp_toplevel_phrase phrase ;
      print_flush ()
      end ;
    (* Only scope the phrase *)
    let (scoped_phrase, new_scenv) = Topscope.scope_toplevel_phrase
				                !Envscope.global_scope_env
					        extended_phrase in
    (* Load external signatures found during scoping.            *)
    (* This update the global typing environment by side effect. *)
    Inputpt.load (Envscope.get_compil_units_to_load ()) ;
    (* If debug, print expression after scoping *)
    if !debug_level4 then
      begin
      fprintf std_formatter "S> %a\n"
                              Printscope.pp_toplevel_phrase scoped_phrase ;
      print_flush ()
      end ;
    (* Now let's type... *)
    let (typed_phrase, phrase_ty) = Infermod.infer_toplevel_phrase
                                                 !Envtype.global_type_env
                                                 scoped_phrase in
    (* Build the new typing environment by extending the old one *)
    let new_type_env =
      List.fold_left (fun env_accu si -> Envtype.add_spec si env_accu)
                     !Envtype.global_type_env phrase_ty in
    (* Update global environments *)
    Envscope.global_scope_env := new_scenv ;
    Envtype.global_type_env := new_type_env ;
    (* Now print types infered *)
    fprintf std_formatter "%a@." Printmod.pp_toplevel_toplevel
                                 (typed_phrase, phrase_ty)
   with
    | Infermod.Push_file filename ->
	begin
	incr inclusions ;
	(* Push current parsing context *)
	channel_stack := (!in_channel, !lexbuf) :: !channel_stack ;
        try
        in_channel :=  open_in filename ;
	lexbuf := Lexing.from_channel !in_channel
	with Sys_error _ ->
	  (* Pop old parsing context *)
	  fprintf std_formatter "Cannot find file %s\n" filename ;
	  decr inclusions ;
	  in_channel := fst (List.hd !channel_stack) ;
          lexbuf := snd (List.hd !channel_stack) ;
          channel_stack := List.tl !channel_stack
	end
    | End_of_file ->
	if !inclusions = 0 then (fprintf std_formatter "\n" ; exit 0) ;
	(* Pop old parsing context *)
	decr inclusions ;
	in_channel := fst (List.hd !channel_stack) ;
        lexbuf := snd (List.hd !channel_stack) ;
        channel_stack := List.tl !channel_stack
    | Lexer.Error (err, start, stop) ->
	Error.handle_lexing_error err start stop
    | Syntaxerr.Error err ->
	Error.handle_parsing_error err
    | Envscope.Scope_error err ->
	Error.handle_scoping_error err
    | Typecore.Conflict (ty0, ty1) ->
	Error.handle_unification_error ty0 ty1
    | Typecore.Non_regular_datatype (ty0, ty1) ->
	Error.handle_circularization_error ty0 ty1
    | Envtype.Type_error err ->
	Error.handle_typing_error err
    | Infercore.Constructor_arity_error path ->
	fprintf std_formatter "Invalid arity for constructor %a use\n"
	        Path.pp_path path
    | Infercore.Unbound_type_variable var_name ->
	fprintf std_formatter "Unbound type variable %s\n" var_name
    | Infercore.Type_arity_error path ->
	fprintf std_formatter "Bad arity for type '%a' use\n" Path.pp_path path
    | Infercore.Bad_labels_number ->
	fprintf std_formatter "Bad # of labels in record\n"
    | Infercore.Field_not_mutable lbl_path ->
	fprintf std_formatter "Label %a is not mutable\n" Path.pp_path lbl_path
    | Substract.Unused_pattern ->
	fprintf std_formatter "A pattern is unused.\n"
    | Corescope.Or_pattern_must_not_bind ->
	fprintf std_formatter "Or patterns must not bind variables.\n"
    | Infermod.Unknown_directive dir_name ->
	fprintf std_formatter "Unknown directive '%s'.\n" dir_name
    | Infercore.Expansion_required_in_primitive path ->
	fprintf std_formatter "Please expand beforehand abbreviation for type '%a' in external definition\n" Path.pp_path path
    | Infercore.Invalid_equiv_in_variant_type ->
	fprintf std_formatter "Invalid type equivalence in manifest sum\n"
  done
;;




(* Let's run... *)
let channel = ref stdin ;;
Arg.parse
    [ ("-I", Arg.String
                (fun path ->
                  let path = path ^ "/" in
                  Stdlibpath.std_lib_path := path :: !Stdlibpath.std_lib_path),
             "Path to stdlib") ;
      ("-noperv", Arg.Unit (fun () -> std_perv := false), "Disable pervs") ;
      ("-fpvars", Arg.Unit (fun () -> Printtypes.forget_pvars := true),
       "Remove empty row vars in printing") ;
      ("-d1", Arg.Unit (fun () -> debug_level1 := true), "Debug level 1") ;
      ("-d2", Arg.Unit (fun () -> debug_level2 := true), "Debug level 2") ;
      ("-d3", Arg.Unit (fun () -> debug_level3 := true), "Debug level 3") ;
      ("-d4", Arg.Unit (fun () -> debug_level4 := true), "Debug level 4") ;
      ("-dall", Arg.Unit (fun () -> debug_level1 := true ;
                                    debug_level2 := true ;
                                    debug_level3 := true ;
	                            debug_level4 := true), "All debug levels")]
    (fun fname -> channel := open_in fname)
    "Ocamlexc 1.0 - Uncaught exceptions analyser experimental toplevel" ;;
Envscope.load_scopes () ;;
Envtype.load_types () ;;
Stdlibpath.std_lib_path := List.rev ("./" :: !Stdlibpath.std_lib_path) ;;
let _ = main !channel ;;
