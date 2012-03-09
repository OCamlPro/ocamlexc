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


let search_string_bnd text_w =
  let requester_w = Toplevel.create text_w [] in
  Wm.title_set requester_w "Search string" ;
  let from_index = ref (TextIndex (LineChar (1, 1), [])) in
  let to_index = ref (TextIndex (End, [])) in
  (* Text entry to search *)
  let entry0_w = Entry.create requester_w [] in
  (* Search flags stuff *)
  let frame0_w = Frame.create requester_w [Relief Raised;
					   BorderWidth (Pixels 2)] in
  let var0_w = Textvariable.create_temporary frame0_w in
  let radiob0_w = Radiobutton.create frame0_w [Text ">>"; Variable var0_w;
					       Value ">>";
      Command (fun _ -> to_index := (TextIndex (End, [])))] in
  let radiob1_w = Radiobutton.create frame0_w [Text "<<"; Variable var0_w;
					       Value "<<";
      Command (fun _ -> to_index := (TextIndex (LineChar (1, 1), [])))] in
  let frame1_w = Frame.create requester_w [Relief Raised;
					   BorderWidth (Pixels 2)] in
  let var1_w = Textvariable.create_temporary frame1_w in
  let radiob2_w = Radiobutton.create frame1_w [Text "a=A"; Variable var1_w;
					       Value "a=A"] in
  let radiob3_w = Radiobutton.create frame1_w [Text "a<>A"; Variable var1_w;
					       Value "a<>A"] in
  let frame2_w = Frame.create requester_w [] in
  let butt0_w = Button.create frame2_w [Text "Abort";
					Command (fun _ ->
					   Text.tag_delete text_w ["SEARCH"] ;
					   destroy requester_w)] in
  (* Select insensitive forward search by default *)
  Radiobutton.select radiob0_w ;
  Radiobutton.select radiob2_w ;
  let search _ =
    try
     (* Recover search options *)
     let search_dir =
       if Textvariable.get var0_w = "<<" then Backwards else Forwards in
     let search_opt =
       [search_dir ;
	(if  Textvariable.get var1_w = "a=A" then Nocase else Exact)] in
     let i =
       if search_dir = Forwards then
	 Text.search text_w search_opt (Entry.get entry0_w)
                           !from_index !to_index
       else
	 Text.search text_w search_opt (Entry.get entry0_w)
                            !from_index !to_index in
     Text.tag_delete text_w ["SEARCH"] ;
     Text.tag_add text_w "SEARCH" (TextIndex (i, []))
                  (TextIndex (i, [WordEnd])) ;
     Text.tag_configure text_w "SEARCH" 
                        [Relief Raised; BorderWidth (Pixels 1);
       	       	         Background Red] ;
     (* Make the found point visible *)
     Text.see text_w (TextIndex (i, [])) ;
     if search_dir = Forwards then
       from_index := (TextIndex (i, [WordEnd]))
     else from_index := (TextIndex (i, [WordStart]))
    with Invalid_argument _ -> Bell.ring () in
  (* Enter : search next *)
  bind entry0_w [([], KeyPressDetail "Return")] (BindSet ([], search)) ;
  (* ^g : abandon search *)
  bind entry0_w [([Control], KeyPressDetail "g")]
       (BindSet ([], (fun _ -> Text.tag_delete text_w ["SEARCH"] ;
	                       destroy requester_w))) ;
  pack [entry0_w] [Side Side_Left; Fill Fill_X; Expand true] ;
  pack [frame0_w; frame1_w; frame2_w] [Side Side_Left] ;
  pack [radiob0_w; radiob1_w] [Side Side_Left] ;
  pack [radiob2_w; radiob3_w] [Side Side_Left] ;
  pack [butt0_w] [] ;
  Focus.set entry0_w
;;



let create parent_w module_path module_source mode =
  let bookmarks = ref 0 in
  Wm.title_set parent_w (Path.end_name module_path) ;
  (* Containers *)
  let frame0_w = Frame.create parent_w [Relief Raised;
					BorderWidth (Pixels 3) ] in
  let frame1_w = Frame.create parent_w [] in
  let butt0_w = Button.create frame0_w [Text "Close" ;
			       Command (fun _ -> destroy parent_w)] in
  let butt1_w = Button.create frame0_w [Text "Search"] in
  let butt2_w = Button.create frame0_w [Text "Quit";
					Command (fun _-> exit 0)] in
  let butt3_w = Button.create frame0_w [Text "Set Mark"] in
  let mbut0_w = Menubutton.create frame0_w [Text "Jump Mark"] in
  let menu0_w = Menu.create mbut0_w [] in
  Menubutton.configure mbut0_w [Menu menu0_w] ;
  (* Main text widget. No write possible by default. *)
  let text0_w =
    Text.create frame1_w
                [Background (Preferences.tk_color_of_int
	           Preferences.global_prefs.Preferences.background_tag_color) ;
		   Cursor (XCursor "hand2"); State Disabled] in
  (* Right scrollbar *)
  let scroll0_w = Scrollbar.create frame1_w [Orient Vertical] in
  (* Link scrollbar and text widgets *)
  Text.configure text0_w [YScrollCommand (Scrollbar.set scroll0_w)] ;
  Scrollbar.configure scroll0_w [ScrollCommand (Text.yview text0_w)] ;
  (* Bindings for the text widget *)
  bind text0_w [([Control], KeyPressDetail "s") ]
	       (BindSet ([], fun _ -> search_string_bnd text0_w)) ;
  bind text0_w [([Alt], KeyPressDetail "q") ] (BindSet ([], fun _ -> exit 0)) ;
  (* Configure toolbar buttons and the generic "bookmark" tag. *)
  Text.tag_configure text0_w "bookmarked" [Relief Sunken;
					   BorderWidth (Pixels 1)] ;
  Button.configure butt1_w [Command (fun _ -> search_string_bnd text0_w)] ;
  Button.configure butt3_w
    [Command (fun _ ->
               try
                 let i = Text.index text0_w (TextIndex (TagFirst "sel", [])) in
		 let i = TextIndex (i, []) in
                 let j = Text.index text0_w (TextIndex (TagLast "sel", [])) in
		 let j = TextIndex (j, []) in
		 let bm_num_str = string_of_int !bookmarks in
                 Text.mark_set text0_w bm_num_str i ;
		 Menu.add_command menu0_w
		   [ Label bm_num_str ;
		     Command (fun _-> Text.see text0_w i) ] ;
		 Text.tag_add text0_w "bookmarked" i j ;
		 incr bookmarks
               with Protocol.TkError _ ->
                 (* No selection in widget *) Bell.ring ())] ;
  (* Finally pack the whole stuff *)
  pack [frame0_w] [Fill Fill_X] ;
  pack [frame1_w] [Expand true; Fill Fill_Both] ;
  pack [butt1_w; butt0_w; butt3_w; mbut0_w] [Side Side_Left] ;
  pack [butt2_w] [Side Side_Right] ;
  pack [text0_w] [Side Side_Left; Expand true; Fill Fill_Both] ;
  pack [scroll0_w] [Expand true; Fill Fill_Y] ;
  (* Set the focus to the text widget *)
  Focus.set text0_w ;
  (* Create standard tags *)
  Text.tag_add_char text0_w "VAL" (TextIndex (End, [])) ;
  Text.tag_configure text0_w "VAL"
                    [Foreground
		      (Preferences.tk_color_of_int
		        Preferences.global_prefs.Preferences.val_tag_color)] ;
  Text.tag_add_char text0_w "TYPE" (TextIndex (End, [])) ;
  Text.tag_configure text0_w "TYPE"
                     [Foreground
		      (Preferences.tk_color_of_int
			Preferences.global_prefs.Preferences.type_tag_color)] ;
  Text.tag_add_char text0_w "MODULE" (TextIndex (End, [])) ;
  Text.tag_configure text0_w "MODULE"
                   [Foreground
	            (Preferences.tk_color_of_int
	    	      Preferences.global_prefs.Preferences.module_tag_color)] ;
  Text.tag_add_char text0_w "CONSTRUCTOR" (TextIndex (End, [])) ;
  Text.tag_configure text0_w "CONSTRUCTOR"
             [Foreground
	       (Preferences.tk_color_of_int
		 Preferences.global_prefs.Preferences.constructor_tag_color)] ;
  Text.tag_add_char text0_w "LABEL" (TextIndex (End, [])) ;
  Text.tag_configure text0_w "LABEL"
                   [Foreground
	             (Preferences.tk_color_of_int
		       Preferences.global_prefs.Preferences.label_tag_color)] ;
  Text.tag_add_char text0_w "WHERE" (TextIndex (End, [])) ;
  Text.tag_configure text0_w "WHERE"
                  [Background
		    (Preferences.tk_color_of_int
		      Preferences.global_prefs.Preferences.where_tag_color)] ;
  (* Alias printing functions to Tk's ones *)
  let pcontext = {
    Printcontext.widget = text0_w ;
    Printcontext.root_type = Printcontext.Nothing ;
    Printcontext.left_indent = 0 ;
    Printcontext.mark_radical = "" ;
    Printcontext.tag_buffer = ref "" ;
    Printcontext.tag_scan_flag = ref false } in
  let (old_print, old_flush) = Format.get_formatter_output_functions () in
  Format.set_formatter_output_functions
      (fun s pos num ->
	Tklowprint.scan_string pcontext s pos num)
      (fun () -> ()) ;
  (* Enable temporarily writes in the text widget *)
  Text.configure text0_w [State Normal] ;
  begin
  match mode with
   | Global.Typedisplay ->
       (* Print the module signature *)
       let mod_sig =
	 Envtype.find_module module_path !Envtype.global_type_env in
       fprintf std_formatter "%a@\n@."
               (Tkprintmod.pp_module_type pcontext) mod_sig
   | Global.Syntaxdisplay ->
       (* Print the module syntax tree *)
       let tree = List.assoc module_path !Global.syntax_trees in
       Tkloadsrc.load_source text0_w module_source tree
  end ;
  (* Re-disable writes in the text widget *)
  Text.configure text0_w [State Disabled] ;
  (* Restore printing functions *)
  Format.set_formatter_output_functions old_print old_flush
;;

