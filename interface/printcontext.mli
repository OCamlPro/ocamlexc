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


type root_type =
  | Nothing
  | Ml of Typecore.ml_type_expr
  | Phi of Typecore.phi_expr

;;

type print_context = {
    widget : Widget.widget ;         (* Text widget where to display *)
    root_type : root_type ;          (* Type currently printed *)
    left_indent : int ;  (* Left offset for indenting when expanding the *)
                         (* type expression text. In fact, it represent  *)
                         (* the number of chars from the left border of  *)
                         (* window to the ":" after the component name. *)
    mark_radical : string ; (* Mark indicating the beginning / end of the *)
                            (* text representing the printed type expr.   *)
    tag_buffer : string ref ;  (* Buffer for tag/mark *)
    tag_scan_flag : bool ref   (* Bool telling if we are parsing a tag/mark *)
  } ;;
