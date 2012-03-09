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



let top_w = Tk.openTk () ;;

let link_files = ref ([] : string list) ;;
let syntax_trees = ref ([] : (Path.t * Typedtree.structure) list) ;;
let modules_sources = ref ([] : (string * string) list) ;;


type mode =
  | Syntaxdisplay
  | Typedisplay
;;
