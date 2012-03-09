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

(* $Id: digest.ml,v 1.1 1998/08/19 13:29:03 pessaux Exp $ *)

(* Message digest (MD5) *)

type t = string ;;

(* XXX Because we don't expand type name in external, we must do *)
(* it by hand !!! That's why we must replace t by string !       *)
(*
external unsafe_string: string[`a] <[`b]-> int[`c] <[`d]->
                        int[`e] <[`f]-> t[_] = "md5_string" ;;
external channel: in_channel[`a] <[`b]-> int[`c] <[`d]-> t[_] = "md5_chan" ;;
*)
external unsafe_string: string[`a] <[`b]-> int[`c] <[`d]->
                        int[`e] <[`f]-> string[_] = "md5_string" ;;
external channel: in_channel[`a] <[`b]-> int[`c] <[`d]-> string[_]
                       = "md5_chan" ;;

let string str =
  unsafe_string str 0 (String.length str) ;;

let substring str ofs len =
  if ofs < 0 or ofs + len > String.length str
  then invalid_arg "Digest.substring"
  else unsafe_string str ofs len ;;

let file filename =
  let ic = open_in_bin filename in
  let d = channel ic (in_channel_length ic) in
  close_in ic;
  d ;;

let output chan digest =
  output chan digest 0 16

let input chan =
  let digest = String.create 16 in
  really_input chan digest 0 16;
  digest ;;

