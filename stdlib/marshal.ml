(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id: marshal.ml,v 1.2 1998/10/13 17:11:07 pessaux Exp $ *)

type extern_flags =
    No_sharing
  | Closures ;;

external to_channel: out_channel[`a] <[`b]-> 'a <[`c]->
                     extern_flags[`d] list[`e] <[`f]-> unit[() :Pre; `g]
                 = "output_value"  ;;
external to_string: 'a <[`a]-> extern_flags[`b] list[`c] <[`d]-> string[_]
                 = "output_value_to_string" ;;
external to_buffer_unsafe:
      string[`a] <[`b]-> int[`c] <[`d]-> int[`e] <[`f]-> 'a <[`g]->
      extern_flags[`h] list[`i] <[`j]-> int[_]
                 = "output_value_to_buffer" ;;

let to_buffer buff ofs len v flags =
  if ofs < 0 or len < 0 or ofs + len > String.length buff
  then invalid_arg "Marshal.to_buffer: substring out of bounds"
  else to_buffer_unsafe buff ofs len v flags ;;

external from_channel: in_channel[`a] <[`b]-> 'a = "input_value" ;;
external from_string_unsafe: string[`a] <[`b]-> int[`c] <[`d]-> 'a
                 = "input_value_from_string" ;;
external data_size_unsafe: string[`a] <[`b]-> int[`c] <[`d]-> int[_]
                 = "marshal_data_size" ;;

let header_size = 20 ;;
let data_size buff ofs =
  if ofs < 0 || ofs + header_size > String.length buff
  then invalid_arg "Marshal.data_size"
  else data_size_unsafe buff ofs ;;
let total_size buff ofs = header_size + data_size buff ofs ;;

let from_string buff ofs =
  if ofs < 0 || ofs + header_size > String.length buff
  then invalid_arg "Marshal.from_size"
  else begin
    let len = data_size_unsafe buff ofs in
    if ofs + header_size + len > String.length buff
    then invalid_arg "Marshal.from_string"
    else from_string_unsafe buff ofs
  end ;;

