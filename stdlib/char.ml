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

(* $Id: char.ml,v 1.2 1998/10/13 17:11:06 pessaux Exp $ *)

(* Character operations *)


external code: char[`a] <[`b]-> int[_] = "%identity" ;;
external unsafe_chr: int[`a] <[`b]-> char[_] = "%identity" ;;

let chr n =
  if n < 0 or n > 255 then invalid_arg "Char.chr" else unsafe_chr n ;;

external is_printable: char[`a] <[`b]-> bool[true:Pre;false:Pre;`c]
  = "is_printable" ;;

external string_create: int[`a] <[`b]-> string[_] = "create_string" ;;
external string_unsafe_get : string[`a] <[`b]-> int[`c] <[`d]->
                             char[_] = "%string_unsafe_get" ;;
external string_unsafe_set : string[`a] <[`b]-> int[`c] <[`d]->
                             char[`e] <[`f]-> unit[() :Pre; `g]
                           = "%string_unsafe_set" ;;

let escaped = function
    '\'' -> "\\'"
  | '\\' -> "\\\\"
  | '\n' -> "\\n"
  | '\t' -> "\\t"
  | c ->  if is_printable c then begin
            let s = string_create 1 in
            string_unsafe_set s 0 c;
            s
          end else begin
            let n = code c in
            let s = string_create 4 in
            string_unsafe_set s 0 '\\';
            string_unsafe_set s 1 (unsafe_chr (48 + n / 100));
            string_unsafe_set s 2 (unsafe_chr (48 + (n / 10) mod 10));
            string_unsafe_set s 3 (unsafe_chr (48 + n mod 10));
            s
          end ;;

let lowercase c =
  if (c >= 'A' && c <= 'Z')
  || (c >= '\192' && c <= '\214')
  || (c >= '\216' && c <= '\222')
  then unsafe_chr(code c + 32)
  else c ;;

let uppercase c =
  if (c >= 'a' && c <= 'z')
  || (c >= '\224' && c <= '\246')
  || (c >= '\248' && c <= '\254')
  then unsafe_chr(code c - 32)
  else c ;;

