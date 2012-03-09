(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id: obj.ml,v 1.2 1998/10/13 17:11:07 pessaux Exp $ *)

(* Operations on internal representations of values *)

type t ;;

external repr : 'a <[`a]-> t[_] = "%identity" ;;
external obj : t[`a] <[`b]-> 'a = "%identity" ;;
external magic : 'a <[`a]-> 'b = "%identity" ;;
external is_block : t[`a] <[`b]-> bool[true:Pre;false:Pre;`c]
  = "obj_is_block" ;;
external tag : t[`a] <[`b]-> int[_] = "obj_tag" ;;

external size : t[`a] <[`b]-> int[_] = "%obj_size" ;;
external field : t[`a] <[`b]-> int[`c] <[`d]-> t[_] = "%obj_field" ;;
external set_field : t[`a] <[`b]-> int[`c] <[`d]-> t[`e] <[`f]->
                     unit[() :Pre; `g] = "%obj_set_field" ;;
external new_block : int[`a] <[`b]-> int[`c] <[`d]-> t[_] = "obj_block" ;;

let marshal (obj: t) =
  Marshal.to_string obj [] ;;
let unmarshal str pos =
  (Marshal.from_string str pos, pos + Marshal.total_size str pos) ;;

