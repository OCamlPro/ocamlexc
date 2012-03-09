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
open Tk ;;


exception Found of Typedtree.expression ;;



let rec find_loc_expression char_start char_stop expr =
  if char_start = expr.Typedtree.exp_loc.Location.loc_start &&
     char_stop = expr.Typedtree.exp_loc.Location.loc_end
  then raise (Found expr) ;
  if char_start >= expr.Typedtree.exp_loc.Location.loc_start &&
     char_stop <= expr.Typedtree.exp_loc.Location.loc_end
  then find_loc_expression_desc char_start char_stop expr.Typedtree.exp_desc


and find_loc_expression_desc char_start char_stop = function
  | Typedtree.Texp_ident _ -> ()
  | Typedtree.Texp_constant _ -> ()
  | Typedtree.Texp_let (_, bindings, expr) ->
      List.iter
         (fun (_, expr) -> find_loc_expression char_start char_stop expr)
         bindings ;
      find_loc_expression char_start char_stop expr
  | Typedtree.Texp_function bindings ->
      List.iter
         (fun (_, expr) -> find_loc_expression char_start char_stop expr)
         bindings
  | Typedtree.Texp_apply (expr0, exprs) ->
      find_loc_expression char_start char_stop expr0 ;
      List.iter (find_loc_expression char_start char_stop) exprs
  | Typedtree.Texp_match (expr, bindings) ->
      find_loc_expression char_start char_stop expr ;
      List.iter
         (fun (_, expr) -> find_loc_expression char_start char_stop expr)
         bindings
  | Typedtree.Texp_try (expr, bindings) ->
      find_loc_expression char_start char_stop expr ;
      List.iter
         (fun (_, expr) -> find_loc_expression char_start char_stop expr)
         bindings
  | Typedtree.Texp_tuple exprs ->
      List.iter (find_loc_expression char_start char_stop) exprs
  | Typedtree.Texp_construct (_, expr_opt) ->
      (match expr_opt with None -> ()
       | Some e -> find_loc_expression char_start char_stop e)
  | Typedtree.Texp_record (rec_fields, expr_opt) ->
      List.iter
            (fun (_, e) -> find_loc_expression char_start char_stop e)
            rec_fields ;
      (match expr_opt with None -> ()
       | Some e -> find_loc_expression char_start char_stop e)
  | Typedtree.Texp_field (expr, _) ->
      find_loc_expression char_start char_stop expr
  | Typedtree.Texp_setfield (expr0, _, expr1) ->
      find_loc_expression char_start char_stop expr0 ;
      find_loc_expression char_start char_stop expr1
  | Typedtree.Texp_array exprs ->
      List.iter (find_loc_expression char_start char_stop) exprs
  | Typedtree.Texp_ifthenelse (expr0, expr1, expr2_opt) ->
      find_loc_expression char_start char_stop expr0 ;
      find_loc_expression char_start char_stop expr1 ;
      (match expr2_opt with None -> ()
       | Some e -> find_loc_expression char_start char_stop e)
  | Typedtree.Texp_sequence (expr0, expr1) ->
      find_loc_expression char_start char_stop expr0 ;
      find_loc_expression char_start char_stop expr1
  | Typedtree.Texp_while  (expr0, expr1) ->
      find_loc_expression char_start char_stop expr0 ;
      find_loc_expression char_start char_stop expr1
  | Typedtree.Texp_for (_, expr0, expr1, _, expr2) ->
      find_loc_expression char_start char_stop expr0 ;
      find_loc_expression char_start char_stop expr1 ;
      find_loc_expression char_start char_stop expr2
  | Typedtree.Texp_constraint (expr, _, _) ->
      find_loc_expression char_start char_stop expr
  | Typedtree.Texp_when (expr0, expr1) ->
      find_loc_expression char_start char_stop expr0 ;
      find_loc_expression char_start char_stop expr1
  | Typedtree.Texp_letmodule (_, _, _) ->
      failwith "tkloadsrc.ml: Texp_letmodule never handled since years :/"



and find_loc_module_expr char_start char_stop mod_expr =
  if char_start = mod_expr.Typedtree.mod_loc.Location.loc_start &&
     char_stop = mod_expr.Typedtree.mod_loc.Location.loc_end
    then () (* We don't take module expressions in account *)
  else
  if char_start >= mod_expr.Typedtree.mod_loc.Location.loc_start &&
     char_stop <= mod_expr.Typedtree.mod_loc.Location.loc_end
  then
    find_loc_module_expr_desc char_start char_stop mod_expr.Typedtree.mod_desc
  else ()


and find_loc_module_expr_desc char_start char_stop = function
  | Typedtree.Tmod_ident _ -> ()
  | Typedtree.Tmod_structure structure ->
      find_loc_structure char_start char_stop structure
  | Typedtree.Tmod_functor (_, _) -> ()  (* Not a typed tree *)
  | Typedtree.Tmod_apply (mod_expr0, mod_expr1) ->
      find_loc_module_expr char_start char_stop mod_expr0 ;
      find_loc_module_expr char_start char_stop mod_expr1


and find_loc_structure char_start char_stop structure =
  List.iter (find_loc_structure_item char_start char_stop) structure



and find_loc_structure_item char_start char_stop = function
  | Typedtree.Tstr_eval expr -> find_loc_expression char_start char_stop expr
  | Typedtree.Tstr_value (_, bindings) ->
      List.iter
         (fun (_, expr) -> find_loc_expression char_start char_stop expr)
         bindings
  | Typedtree.Tstr_primitive (_, _) -> ()
  | Typedtree.Tstr_type _ -> ()
  | Typedtree.Tstr_exception _ -> ()
  | Typedtree.Tstr_module (_, mod_expr) ->
      find_loc_module_expr char_start char_stop mod_expr
;;



let line_column_to_char_number lines_mappping line char =
  (lines_mappping.(line)) + char
;;


let char_number_to_line_column lines_mappping char_number =
  let max_l = Array.length lines_mappping in
  let rec find l =
    if l > max_l then assert false ;
    if char_number < lines_mappping.(l) then
      begin
      (* The char is in the previous line *)
      let line = l - 1 in
      let char = char_number - lines_mappping.(l - 1) in
      (line, char)
      end
    else
      find (l + 1) in
  find 1 (* Start at line 1, not 0 *)
;;
      
  
let create_mark_radical =
 let cpt = ref 0 in
 function () ->
  let name = "MARK" ^ (string_of_int !cpt) in
  incr cpt ;
  name
;;


let create_popup parent_w expression =
  let new_window = Toplevel.create parent_w [] in
  Wm.title_set new_window "Zoom" ;
  let label0_w = Label.create new_window [Text "Type of expression"] in
  let text0_w =
    Text.create new_window [Background (NamedColor "LightSteelBlue");
		     Cursor (XCursor "hand2"); State Disabled; TextHeight 5] in
  let label1_w = Label.create new_window [Text "Effect of expression"] in
  let text1_w =
    Text.create new_window [Background (NamedColor "LightSteelBlue");
		     Cursor (XCursor "hand2"); State Disabled; TextHeight 5] in
  (* Button to close the window *)
  let butt0_w = Button.create new_window
                              [Text "Close" ;
			       Command (fun _ -> destroy new_window)] in
  (* Right-clic shortcut to close the window *)
  bind text0_w [([], ButtonPressDetail 3) ]
       (BindSet ([], fun _ -> destroy new_window)) ;
  bind text1_w [([], ButtonPressDetail 3) ]
       (BindSet ([], fun _ -> destroy new_window)) ;
  (* Create the tag for "where" construction *)
  Text.tag_add_char text0_w "WHERE" (TextIndex (End, [])) ;
  Text.tag_configure text0_w "WHERE" [Background White] ;
  (* Display the whole stuff *)
  pack [label0_w] [Side Side_Top; Fill Fill_Both] ;
  pack [text0_w] [Side Side_Top; Expand true; Fill Fill_Both] ;
  pack [label1_w] [Side Side_Top; Fill Fill_Both] ;
  pack [text1_w] [Side Side_Top; Expand true; Fill Fill_Both] ;
  pack [butt0_w] [Fill Fill_Both] ;
  (* Print expression type in upper window *)
  let ty_expr = expression.Typedtree.exp_type in
  let mark_r = create_mark_radical () in
  let pcontext = {
    Printcontext.widget = text0_w ;
    Printcontext.root_type = Printcontext.Ml ty_expr ;
    Printcontext.left_indent = 0 ;
    Printcontext.mark_radical = mark_r ;
    Printcontext.tag_buffer = ref "" ;
    Printcontext.tag_scan_flag = ref false } in
  let (old_print, old_flush) =
    pp_get_formatter_output_functions std_formatter () in
  pp_set_formatter_output_functions std_formatter
      (fun s pos num ->
	Tklowprint.scan_string pcontext s pos num)
      (fun () -> ()) ;
  Text.configure text0_w [State Normal] ;
  pp_print_as std_formatter 0 ("\006START"^mark_r^"\008") ;
  fprintf std_formatter "%a@\n@." (Tkprinttypes.pp_ml_type pcontext) ty_expr ;
  pp_print_as std_formatter 0 ("\006STOP"^mark_r^"\008") ;
  fprintf std_formatter "@?" ;
  Text.configure text0_w [State Disabled] ;
  pp_set_formatter_output_functions std_formatter old_print old_flush ;
  (* Print expression effect in lower window *)
  let effect_expr = expression.Typedtree.exp_exn in
  let mark_r = create_mark_radical () in
  let pcontext = {
    Printcontext.widget = text1_w ;
    Printcontext.root_type = Printcontext.Phi effect_expr ;
    Printcontext.left_indent = 0 ;
    Printcontext.mark_radical = mark_r ;
    Printcontext.tag_buffer = ref "" ;
    Printcontext.tag_scan_flag = ref false } in
  let (old_print, old_flush) =
    pp_get_formatter_output_functions std_formatter () in
  pp_set_formatter_output_functions std_formatter
      (fun s pos num ->
	Tklowprint.scan_string pcontext s pos num)
      (fun () -> ()) ;
  Text.configure text1_w [State Normal] ;
  pp_print_as std_formatter 0 ("\006START"^mark_r^"\008") ;
  (* Make the effect printable *)
  effect_expr.Typecore.phi_print <- true ;
  fprintf std_formatter "%a@\n@."
          (Tkprinttypes.pp_phi_type pcontext) effect_expr ;
  pp_print_as std_formatter 0 ("\006STOP"^mark_r^"\008") ;
  fprintf std_formatter "@?" ;
  Text.configure text1_w [State Disabled] ;
  pp_set_formatter_output_functions std_formatter old_print old_flush
;;



(* Creates the callback associated with click in the source *)
let make_callback widget lines_mappping syntax_tree _ =
  try
   begin
   let (line, char) =
     (match Text.index widget (TextIndex (TagFirst "sel", [])) with
       LineChar (l, c) -> (l - 1, c) | _ -> assert false) in
   let char_start = line_column_to_char_number lines_mappping line char in
   let (line', char') =
     (match Text.index widget (TextIndex (TagLast "sel", [])) with
       LineChar (l, c) -> (l - 1, c) | _ -> assert false) in
   let char_stop = line_column_to_char_number lines_mappping line' char' in
   (* Find the expression associated to the character location *)
   try find_loc_structure char_start char_stop syntax_tree ; Bell.ring ()
   with Found expr ->
     (* Highliht the expression *)
     let (start_l, start_c) =
       char_number_to_line_column lines_mappping
                                 (expr.Typedtree.exp_loc.Location.loc_start) in
     let (stop_l, stop_c) = 
       char_number_to_line_column lines_mappping
                                 (expr.Typedtree.exp_loc.Location.loc_end) in
     Text.tag_delete widget ["EXPR"] ;
     (* Don't forget that lines are numbered from 1 in Tk *)
     Text.tag_add widget "EXPR" (TextIndex (LineChar (start_l+1, start_c), []))
                                (TextIndex (LineChar (stop_l+1, stop_c), [])) ;
     Text.tag_configure widget "EXPR"
                        [Relief Raised; BorderWidth (Pixels 1);
       	       	         Background Red] ;
     (* Pop a window with expression node information *)
     create_popup widget expr
   end
  with Protocol.TkError _ -> (* No selection in widget *) Bell.ring ()
;;



(* Line and columns begin at 0 *)
let load_source widget filename syntax_tree =
 let in_channel = Stdlibpath.open_in_with_path filename in
 (* Character # of the beginning of the current line *)
 let char_of_begin_line = ref 0 in
 (* List recording the character # of each new line *)
 let current_mapping = ref [] in
 try
 while true do
   current_mapping := !char_of_begin_line :: !current_mapping ;
   let l = (input_line in_channel)^"\n" in
   char_of_begin_line := !char_of_begin_line + String.length l ;
   Text.insert widget (TextIndex (End, [])) l []
   done
 with End_of_file ->
   close_in in_channel ;
   let array_of_list = Array.of_list (List.rev !current_mapping) in
   let callback = make_callback widget array_of_list syntax_tree in
   bind widget [([], ButtonReleaseDetail 1)] (BindSet ([], callback))
;;
