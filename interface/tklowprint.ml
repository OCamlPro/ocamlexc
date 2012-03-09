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


(* Used for pretty-printing a whole signature *)
let scan_string pcontext s pos num =
 let text_w = pcontext.Printcontext.widget in
 let tag_buffer = pcontext.Printcontext.tag_buffer in
 let tag_scan_flag = pcontext.Printcontext.tag_scan_flag in
 let start = ref pos in
 for i = pos to pos + num - 1 do
   match s.[i] with
    | '\006' ->
	(* Begin of tag *)
	Text.insert text_w (TextIndex (End, []))
                    (String.sub s !start (i - !start)) [!tag_buffer] ;
	tag_buffer := "" ;
	tag_scan_flag := true ;
    | '\007' ->
	(* End of tag *)
	tag_scan_flag := false ;
	start := i + 1
    | '\008' ->
	(* End of mark *)
	tag_scan_flag := false ;
	start := i + 1 ;
	Text.mark_set text_w !tag_buffer (TextIndex (Mark "insert", [])) ;
	Text.mark_gravity_set text_w !tag_buffer Mark_Left
    | whatever ->
	if !tag_scan_flag then
	  tag_buffer := !tag_buffer ^ (Char.escaped whatever)
 done ;
 if not !tag_scan_flag then
   Text.insert text_w (TextIndex (End, []))
               (String.sub s !start (num - !start)) [!tag_buffer]
;;



(* Used for pretty-printing a type component in a callback *)
let scan_string_at pcontext index s pos num =
 let text_w = pcontext.Printcontext.widget in
 let tag_buffer = pcontext.Printcontext.tag_buffer in
 let tag_scan_flag = pcontext.Printcontext.tag_scan_flag in
 let left_indent = pcontext.Printcontext.left_indent in
 let start = ref pos in
 for i = pos to pos + num - 1 do
   match s.[i] with
    | '\006' ->
	(* Begin of tag *)
	Text.insert text_w index
                    (String.sub s !start (i - !start)) [!tag_buffer] ;
	tag_buffer := "" ;
	tag_scan_flag := true
    | '\007' ->
	(* End of tag *)
	tag_scan_flag := false ;
	start := i + 1
    | '\008' ->
	(* End of mark *)
	tag_scan_flag := false ;
	start := i + 1 ;
	Text.mark_set text_w !tag_buffer (TextIndex (Mark "insert", [])) ;
	Text.mark_gravity_set text_w !tag_buffer Mark_Left
    | '\010' ->
	Text.insert text_w index
                    (String.sub s !start (i - !start)) [!tag_buffer] ;
	let pad = String.make left_indent ' ' in
	Text.insert text_w index ("\n"^pad) [!tag_buffer] ;
        start := i + 1
    | whatever ->
	if !tag_scan_flag then
	  tag_buffer := !tag_buffer ^ (Char.escaped whatever)
 done ;
 if not !tag_scan_flag then
   Text.insert text_w index
               (String.sub s !start (num - !start)) [!tag_buffer]
;;
