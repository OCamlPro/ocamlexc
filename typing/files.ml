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


(* Name of the current analysed compilation unit *)
let current_comp_unit = ref "" ;;

exception Not_ml_file of string ;;
exception Not_cme_file of string ;;


(* cme_filename_from_ml_filename: string -> string *)
(* Build the .cme filename according to the original .ml filename. *)
(* This uses the full filesystem-path given in the filename. I.e   *)
(* "/usr/foo/bar.ml" will be mapped to "/usr/foo/bar.cme"          *)
let cme_filename_from_ml_filename filename =
  if Filename.check_suffix filename ".ml" then
    (Filename.chop_extension filename)^".cme"
  else raise (Not_ml_file filename)
;;


let ml_filename_from_cme_filename filename =
  if Filename.check_suffix filename ".cme" then
    (Filename.chop_extension filename)^".ml"
  else raise (Not_cme_file filename)
;;



(* module_name_from_ml_filename : string -> string *)
(* Create the string name of a module (seen as an external compilation *)
(* unit) from the name of the file it is in. This takes the full       *)
(* filename (with its path) and cut it to obtain only the ending name. *)
(* Then it removes the extension and capitalize the remaining string.  *)
let module_name_from_ml_filename filename =
  if Filename.check_suffix filename ".ml" then
    String.capitalize (Filename.chop_extension (Filename.basename filename))
  else raise (Not_ml_file filename)
;;


let module_name_from_cme_filename filename =
  if Filename.check_suffix filename ".cme" then
    String.capitalize (Filename.chop_extension (Filename.basename filename))
  else raise (Not_cme_file filename)
;;


(* cme_filename_from_module_name: string -> string *)
(* Create the ending filename corresponding to a module name.  *)
(* This does not add any file-system path to this ending name. *)
let cme_filename_from_module_name module_name =
  (String.uncapitalize  module_name) ^ ".cme"
;;
