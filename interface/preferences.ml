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


type preferences = {
  mutable val_tag_color: int ;
  mutable type_tag_color: int ;
  mutable module_tag_color: int ;
  mutable constructor_tag_color: int ;
  mutable label_tag_color: int ;
  mutable where_tag_color: int ;
  mutable background_tag_color: int
} ;;



(* Fonction chargee de retrouver le home directory de l'utilisateur.   *)
(* On recherche sont uid et on va parser le /etc/passwd pour retrouver *)
(* le champ homedir correspondant.                                     *)
exception No_home_dir ;;
let gethomedir () =
  let uid = Unix.getuid () in
  let etc_passwd = try open_in "/etc/passwd" with _ -> raise No_home_dir in
  let home_dir = ref "" in
  try
    while !home_dir = "" do
      let line = input_line etc_passwd in
      (* Saute au password *)
      let i = String.index line ':' in
      (* Saute au debut de l'uid *)
      let i' = String.index_from line (i + 1) ':' in
      (* Saute au debut du gid *)
      let i'' = String.index_from line (i' + 1) ':' in
      let id_string = String.sub line (i' + 1) (i'' - i' - 1) in
      let id = int_of_string id_string in
      if id = uid then
        begin
        (* Saute a la fin du gid *)
        let i3 = String.index_from line (i'' + 1) ':' in
        (* Saute au debut du homedir *)
        let i4 = String.index_from line (i3 + 1) ':' in
        (* Fin du homedir *)
        let i5 = String.index_from line (i4 + 1) ':' in
        home_dir := String.sub line (i4 + 1) (i5 - i4 - 1) ;
        end
    done ;
    close_in etc_passwd ;
    !home_dir
  with _ -> raise No_home_dir
;;



let tk_color_of_int i =
  NamedColor (Printf.sprintf "#%06x" i)
;;



let global_prefs = {
  val_tag_color = 0x00FF00 ;  type_tag_color = 0x0000FF ;
  module_tag_color = 0xFFFF00 ; constructor_tag_color = 0xFFFFFF ;
  label_tag_color = 0xAF2F5F ;  where_tag_color = 0xFFFFFF ;
  background_tag_color = 0xC9E0FF }
;;


let copy_prefs from_prefs to_prefs =
  to_prefs.val_tag_color <- from_prefs.val_tag_color ;
  to_prefs.type_tag_color <- from_prefs.type_tag_color ;
  to_prefs.module_tag_color <- from_prefs.module_tag_color ;
  to_prefs.constructor_tag_color <- from_prefs.constructor_tag_color ;
  to_prefs.label_tag_color <- from_prefs.label_tag_color ;
  to_prefs.where_tag_color <- from_prefs.where_tag_color ;
  to_prefs.background_tag_color <- from_prefs.background_tag_color
;;


let load_preferences () =
  try
    (* let pref_hd = open_in "/tmp/.ocamlexcrc" in *)
    let pref_hd = open_in ((gethomedir ()) ^ "/.ocamlexcrc") in
    let (loaded_prefs : preferences) = input_value pref_hd in
    close_in pref_hd ;
    copy_prefs loaded_prefs global_prefs
  with
   | Sys_error _ | No_home_dir -> ()
;;



let save_preferences () =
  try
    (* let pref_hd = open_out "/tmp/.ocamlexcrc" in *)
    let pref_hd = open_out  ((gethomedir ()) ^ "/.ocamlexcrc") in
    output_value pref_hd global_prefs ;
    close_out pref_hd ;
    true
  with
   | Sys_error _ | No_home_dir -> false
;;



let open_pref_window parent_w =
  (* Where to store current onfiguration *)
  let local_prefs = {
    val_tag_color = 0 ; type_tag_color = 0 ; module_tag_color = 0 ;
    constructor_tag_color = 0 ; label_tag_color = 0 ;
    where_tag_color = 0 ; background_tag_color = 0 } in
  copy_prefs global_prefs local_prefs ;
  (* Now the graphical stuff *)
  let requester_w = Toplevel.create parent_w [] in
  Wm.title_set requester_w "Preferences" ;
  let frame0_w = Frame.create requester_w [] in
  let frame1_w = Frame.create requester_w [Relief Raised;
					BorderWidth (Pixels 2)] in
  let frame2_w = Frame.create frame0_w [Relief Raised;
					BorderWidth (Pixels 2)] in
  let frame3_w = Frame.create frame0_w [Relief Raised;
					BorderWidth (Pixels 2)] in
  let frame4_w = Frame.create frame3_w [] in

  let var0_w = Textvariable.create_temporary frame0_w in
  let radiob0_w = Radiobutton.create frame2_w [Text "Val"; Variable var0_w;
					       Value "val" ] in
  let radiob1_w = Radiobutton.create frame2_w [Text "Type"; Variable var0_w;
					       Value "type"] in
  let radiob2_w = Radiobutton.create frame2_w [Text "Module"; Variable var0_w;
					       Value "module"] in
  let radiob3_w = Radiobutton.create frame2_w [Text "Constructor";
    				     Variable var0_w; Value "constructor"] in
  let radiob4_w = Radiobutton.create frame2_w [Text "Label"; Variable var0_w;
					       Value "label" ] in
  let radiob5_w = Radiobutton.create frame2_w [Text "Where"; Variable var0_w;
					       Value "where" ] in
  let radiob6_w = Radiobutton.create frame2_w [Text "Background";
				     Variable var0_w; Value "background"] in
  let canvas0_w = Canvas.create frame3_w [Width (Pixels 50);
					  BorderWidth (Pixels 3);
					  Height (Pixels 50); Relief Raised] in
  let scaler_w = Scale.create frame4_w [From 0.0; To 255.0; Digits 0] in
  let scaleg_w = Scale.create frame4_w [From 0.0; To 255.0; Digits 0] in
  let scaleb_w = Scale.create frame4_w [From 0.0; To 255.0; Digits 0] in
  (* Callback for the scalers *)
  let scale_callback _ =
    let col = (truncate (Scale.get scaler_w)) lsl 16 +
              (truncate (Scale.get scaleg_w)) lsl 8 +
              (truncate (Scale.get scaleb_w)) in
    let coltxt = tk_color_of_int col in
    let current_selection = Textvariable.get var0_w in
    (match current_selection with
      | "val" -> local_prefs.val_tag_color <- col
      | "type" -> local_prefs.type_tag_color <- col
      | "module" -> local_prefs.module_tag_color <- col
      | "constructor" -> local_prefs.constructor_tag_color <- col
      | "label" -> local_prefs.label_tag_color <- col
      | "where" -> local_prefs.where_tag_color <- col
      | "background" -> local_prefs.background_tag_color <- col
      | _ -> ()) ;
    if current_selection <> "" then
      Canvas.configure canvas0_w [Background coltxt] in
  Scale.configure scaler_w [ScaleCommand scale_callback] ;
  Scale.configure scaleg_w [ScaleCommand scale_callback] ;
  Scale.configure scaleb_w [ScaleCommand scale_callback] ;
  (* Callback for the radio buttons *)
  let radio_callback _ =
    let col = (match Textvariable.get var0_w with
     | "val" -> local_prefs.val_tag_color
     | "type" -> local_prefs.type_tag_color
     | "module" -> local_prefs.module_tag_color
     | "constructor" -> local_prefs.constructor_tag_color
     | "label" -> local_prefs.label_tag_color
     | "where" -> local_prefs.where_tag_color
     | "background" -> local_prefs.background_tag_color
     | _ -> assert false) in
    let coltxt = tk_color_of_int col  in
    let r = float ((col land 0xFF0000) lsr 16) in
    let g = float ((col land 0x00FF00) lsr 8) in
    let b = float ((col land 0x0000FF)) in
    Scale.set scaler_w r ;
    Scale.set scaleg_w g ;
    Scale.set scaleb_w b ;
    Canvas.configure canvas0_w [Background coltxt] in
  List.iter (fun rb -> Radiobutton.configure rb [Command radio_callback])
            [radiob0_w;radiob1_w;radiob2_w;radiob3_w;radiob4_w;radiob5_w;
	     radiob6_w] ;
  (* Callback for the buttons *)
  let apply_callback _ =
    copy_prefs local_prefs global_prefs ;
    destroy requester_w in
  let save_callback _ =
    copy_prefs local_prefs global_prefs ;
    if not (save_preferences ()) then
      begin
      let error_w = Toplevel.create parent_w [] in
      Wm.title_set error_w "Error report" ;
      let label_w = Label.create error_w [Text ("Can't save prefs in "^
                                               "/tmp/.ocamlexcrc")] in
      let button_w = Button.create error_w [Text "So bad";
	 			  Command (fun _ -> destroy error_w)] in
      pack [label_w; button_w] []
      end ;
    destroy requester_w in
  let reset_callback _ =
    copy_prefs global_prefs local_prefs ;
    List.iter Radiobutton.deselect
              [radiob0_w;radiob1_w;radiob2_w;radiob3_w;radiob4_w;radiob5_w;
	       radiob6_w] ;
    Scale.set scaler_w 0.0 ;
    Scale.set scaleg_w 0.0 ;
    Scale.set scaleb_w 0.0 in
  let button0_w = Button.create frame1_w [Text "Apply";
					  Command apply_callback] in
  let button1_w = Button.create frame1_w [Text "Save";
					  Command save_callback] in
  let button2_w = Button.create frame1_w [Text "Cancel";
				  Command (fun _ -> destroy requester_w)] in
  let button3_w = Button.create frame1_w [Text "Reset";
					  Command reset_callback] in
  pack [frame0_w; frame1_w] [Side Side_Top; Fill Fill_X] ;
  pack [frame2_w; frame3_w] [Fill Fill_Y; Side Side_Left] ;
  pack [frame4_w] [] ;
  pack [radiob0_w;radiob1_w;radiob2_w;radiob3_w;radiob4_w;radiob5_w;
        radiob6_w] [Side Side_Top; Anchor W] ;
  pack [scaler_w; scaleg_w; scaleb_w] [Side Side_Left] ;
  pack [canvas0_w] [Side Side_Left; Fill Fill_X; Expand true] ;
  pack [button0_w;button1_w;button2_w;button3_w] [Side Side_Left;Expand true]
;;
