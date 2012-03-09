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
       exit (-1)) ;
    Inputpt.load [Ident.create_global "Pervasives"]
   end ;
  while true do
   try
    let phrase = Parse.toplevel_phrase !lexbuf in
    (* Extend handlers *)
    let extended_phrase =
               Extendhandler.extend_toplevel_phrase_handler phrase in
    (* Only scope the phrase *)
    let (scoped_phrase, new_scenv) = Topscope.scope_toplevel_phrase
				                !Envscope.global_scope_env
					        extended_phrase in
    (* Load external signatures found during scoping.            *)
    (* This update the global typing environment by side effect. *)
    Inputpt.load (Envscope.get_compil_units_to_load ()) ;
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
    Envtype.global_type_env := new_type_env
   with
    | Infermod.Push_file filename ->
	begin
	incr inclusions ;
	(* Push current parsing context *)
	channel_stack := (!in_channel, !lexbuf) :: !channel_stack ;
        try
        in_channel :=  open_in filename ;
	lexbuf := Lexing.from_channel !in_channel
	with Sys_error msg ->
	  (* Pop old parsing context *)
	  decr inclusions ;
	  in_channel := fst (List.hd !channel_stack) ;
          lexbuf := snd (List.hd !channel_stack) ;
          channel_stack := List.tl !channel_stack
	end
    | End_of_file ->
	if !inclusions = 0 then exit 0 ;
	(* Pop old parsing context *)
	decr inclusions ;
	in_channel := fst (List.hd !channel_stack) ;
        lexbuf := snd (List.hd !channel_stack) ;
        channel_stack := List.tl !channel_stack
    | Lexer.Error (err, start, stop) ->
	()
    | Syntaxerr.Error err ->
	()
    | Envscope.Scope_error err ->
	()
    | Typecore.Conflict (ty0, ty1) ->
	()
    | Envtype.Type_error err ->
	()
    | Infercore.Constructor_arity_error path ->
	()
    | Infercore.Unbound_type_variable var_name ->
	()
    | Infercore.Type_arity_error path ->
	()
    | Infercore.Bad_labels_number ->
	()
    | Infercore.Field_not_mutable lbl_path ->
	()
    | Substract.Unused_pattern ->
	()
    | Corescope.Or_pattern_must_not_bind ->
	()
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
      ("-d1", Arg.Unit (fun () -> debug_level1 := true), "Debug level 1") ;
      ("-d2", Arg.Unit (fun () -> debug_level2 := true), "Debug level 2") ;
      ("-d3", Arg.Unit (fun () -> debug_level3 := true), "Debug level 3") ;
      ("-d4", Arg.Unit (fun () -> debug_level4 := true), "Debug level 4") ;
      ("-dall", Arg.Unit (fun () -> debug_level1 := true ;
                                    debug_level2 := true ;
                                    debug_level3 := true ;
	                            debug_level4 := true), "All debug levels")]
    (fun fname -> channel := open_in fname)
    "This program, if it works, should help me to get my Phd thesis..." ;;
Envscope.load_scopes () ;;
Envtype.load_types () ;;
Stdlibpath.std_lib_path := List.rev ("./" :: !Stdlibpath.std_lib_path) ;;
let _ = main !channel ;;
