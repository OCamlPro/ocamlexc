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

(* $Id: pervasives.ml,v 1.12 1999/10/12 15:57:53 pessaux Exp $ *)

type 'a option = None | Some of 'a ;;

(* Exceptions *)

external raise : exn[`a] <[`a]-> 'a = "%raise" ;;

let failwith s = raise(Failure s) ;;
let invalid_arg s = raise(Invalid_argument s) ;;

exception Exit ;;
exception Assert_failure of (string * int * int) ;;

(* Comparisons *)

external (=) : 'a <[`a]-> 'a <[`b]-> bool[false:Pre;true:Pre;`c]
  = "%equal" ;;
external (<>) : 'a <[`a]-> 'a <[`b]-> bool[false:Pre;true:Pre;`c]
  = "%notequal" ;;
external (<) : 'a <[`a]-> 'a <[`b]-> bool[false:Pre;true:Pre;`c]
  = "%lessthan" ;;
external (>) : 'a <[`a]-> 'a <[`b]-> bool[false:Pre;true:Pre;`c]
  = "%greaterthan" ;;
external (<=) : 'a <[`a]-> 'a <[`b]-> bool[false:Pre;true:Pre;`c]
  = "%lessequal" ;;
external (>=) : 'a <[`a]-> 'a <[`b]-> bool[false:Pre;true:Pre;`c]
  = "%greaterequal" ;;
external compare: 'a <[`a]-> 'a <[`b]-> int[_] = "compare" "noalloc" ;;

let min x y = if x <= y then x else y ;;
let max x y = if x >= y then x else y ;;

external (==) : 'a <[`a]-> 'a <[`b]-> bool[false:Pre;true:Pre;`c]
  = "%eq" ;;
external (!=) : 'a <[`a]-> 'a <[`b]-> bool[false:Pre;true:Pre;`c]
  = "%noteq" ;;

(* Boolean operations *)

external not : bool[`a] <[`b]-> bool[false:Pre;true:Pre;`c] = "%boolnot" ;;
external (&) : bool[`a] <[`b]-> bool[`c] <[`d]->
               bool[false:Pre;true:Pre;`e]
  = "%sequand" ;;
external (&&) : bool[`a] <[`b]-> bool[`c] <[`d]->
                bool[false:Pre;true:Pre;`e]
  = "%sequand" ;;
external (or) : bool[`a] <[`b]-> bool[`c] <[`d]->
                bool[false:Pre;true:Pre;`e]
  = "%sequor" ;;
external (||) : bool[`a] <[`b]-> bool[`c] <[`d]->
                bool[false:Pre;true:Pre;`e]
  = "%sequor" ;;

(* Integer operations *)

external (~-) : int[`a] <[`b]-> int[_] = "%negint" ;;
external succ : int[`a] <[`b]-> int[_] = "%succint" ;;
external pred : int[`a] <[`b]-> int[_] = "%predint" ;;
external (+) : int[`a] <[`b]-> int[`c] <[`d]-> int[_] = "%addint" ;;
external (-) : int[`a] <[`b]-> int[`c] <[`d]-> int[_] = "%subint" ;;
external ( * ) : int[`a] <[`b]-> int[`c] <[`d]-> int[_] = "%mulint" ;;
external (/) : int[`a] <[`b]-> int[`c]
                       <[__Division_by_zero23: Pre;`d]-> int [_]= "%divint" ;;
external (mod) : int[`a] <[`b]-> int[`c]
                       <[__Division_by_zero23: Pre;`d]-> int[_] = "%modint" ;;

let abs x = if x >= 0 then x else -x ;;

external (land) : int[`a] <[`b]-> int[`c] <[`d]-> int[_] = "%andint" ;;
external (lor) : int[`a] <[`b]-> int[`c] <[`d]-> int[_] = "%orint" ;;
external (lxor) : int[`a] <[`b]-> int[`c] <[`d]-> int[_] = "%xorint" ;;

let lnot x = x lxor (-1) ;;

external (lsl) : int[`a] <[`b]-> int[`c] <[`d]-> int[_] = "%lslint" ;;
external (lsr) : int[`a] <[`b]-> int[`c] <[`d]-> int[_] = "%lsrint" ;;
external (asr) : int[`a] <[`b]-> int[`c] <[`d]-> int[_] = "%asrint" ;;

let min_int = 1 lsl (if 1 lsl 31 = 0 then 30 else 62) ;;
let max_int = min_int - 1 ;;

(* Floating-point operations *)

external (~-.) : float[`a] <[`b]-> float[_] = "%negfloat" ;;
external (+.) : float[`a] <[`b]-> float[`c] <[`d]-> float[_] = "%addfloat" ;;
external (-.) : float[`a] <[`b]-> float[`c] <[`d]-> float[_] = "%subfloat" ;;
external ( *. ) : float[`a] <[`b]-> float[`c] <[`d]-> float[_] = "%mulfloat" ;;
external (/.) : float[`a] <[`b]-> float[`c] <[`d]-> float[_] = "%divfloat" ;;
external ( ** ) : float[`a] <[`b]-> float[`c]
                            <[`d]-> float[_] = "power_float" "pow" "float" ;;
external exp : float[`a] <[`b]-> float[_] = "exp_float" "exp" "float" ;;
external acos : float[`a] <[`b]-> float[_] = "acos_float" "acos" "float" ;;
external asin : float[`a] <[`b]-> float[_] = "asin_float" "asin" "float" ;;
external atan : float[`a] <[`b]-> float[_] = "atan_float" "atan" "float" ;;
external atan2 : float[`a] <[`b]-> float[`c]
                           <[`d]-> float[_] = "atan2_float" "atan2" "float" ;;
external cos : float[`a] <[`b]-> float[_] = "cos_float" "cos" "float" ;;
external cosh : float[`a] <[`b]-> float[_] = "cosh_float" "cosh" "float" ;;
external log : float[`a] <[`b]-> float[_] = "log_float" "log" "float" ;;
external log10 : float[`a] <[`b]-> float[_] = "log10_float" "log10" "float" ;;
external sin : float[`a] <[`b]-> float[_] = "sin_float" "sin" "float" ;;
external sinh : float[`a] <[`b]-> float[_] = "sinh_float" "sinh" "float" ;;
external sqrt : float[`a] <[`b]-> float[_] = "sqrt_float" "sqrt" "float" ;;
external tan : float[`a] <[`b]-> float[_] = "tan_float" "tan" "float" ;;
external tanh : float[`a] <[`b]-> float[_] = "tanh_float" "tanh" "float" ;;
external ceil : float[`a] <[`b]-> float[_] = "ceil_float" "ceil" "float" ;;
external floor : float[`a] <[`b]-> float[_] = "floor_float" "floor" "float" ;;
external abs_float : float[`a] <[`b]-> float[_] = "%absfloat" ;;
external mod_float : float[`a] <[`b]-> float[`c]
                              <[`d]-> float[_] = "fmod_float" "fmod" "float" ;;
external frexp : float[`a] <[`b]-> float[_] * int[_] = "frexp_float" ;;
external ldexp : float[`a] <[`b]-> int[`c] <[`d]-> float[_] = "ldexp_float" ;;
external modf : float[`a] <[`b]-> float[_] * float[_] = "modf_float" "modf" ;;
external float : int[`a] <[`b]-> float[_] = "%floatofint" ;;
external truncate : float[`a] <[`b]-> int[_] = "%intoffloat" ;;

(* String operations -- more in module String *)

external string_length : string[`a] <[`b]-> int[_] = "ml_string_length" ;;
external string_create :
  int[`a] <[__Invalid_argument14(string[string_create:Pre;`b]);`c]->
  string[_] = "create_string" ;;
external string_blit : string[`a] <[`b]-> int[`c] <[`d]-> string[`e]
              <[`f]-> int[`g] <[`h]-> int[`i]
              <[__Invalid_argument14(string[string_blit:Pre;`j]);`k]->
              unit["()":Pre; `l]
                     = "blit_string" ;;

(* Character operations -- more in module Char *)

external int_of_char : char[`a] <[`b]-> int[_] = "%identity"
external unsafe_char_of_int : int[`a] <[`b]-> char[_] = "%identity"
let char_of_int n =
  if n < 0 or n > 255 then invalid_arg "char_of_int" else unsafe_char_of_int n


let (^) s1 s2 =
  let l1 = string_length s1 and l2 = string_length s2 in
  let s = string_create (l1 + l2) in
  string_blit s1 0 s 0 l1;
  string_blit s2 0 s l1 l2;
  s ;;


(* Unit operations *)

external ignore : 'a <[`a]-> unit[() : Pre;`b] = "%ignore"


(* Pair operations *)

external fst : 'a * 'b <[`a]-> 'a = "%field0" ;;
external snd : 'a * 'b <[`a]-> 'b = "%field1" ;;

(* String conversion functions *)

external format_int: string[`a] <[`b]-> int[`c]
                                <[`d]-> string[_] = "format_int" ;;
external format_float: string[`a] <[`b]-> float[`c]
                                  <[`d]-> string[_] = "format_float" ;;

let string_of_bool b =
  if b then "true" else "false" ;;

let string_of_int n =
  format_int "%d" n ;;

external int_of_string : string[`a]
                       <[__Failure13(string[int_of_string:Pre;`b]);`c]-> int[_]
			 = "int_of_string" ;;

let string_of_float f =
  format_float "%.12g" f ;;

external float_of_string :
  string[`a] <[__Failure13(string[float_of_string:Pre;`b]);`c]-> float[_]
    = "float_of_string" ;;

(* List operations -- more in module List *)

let rec rev_append accu = function
  | [] -> accu
  | a::l -> rev_append (a :: accu) l ;;

let (@) l1 l2 = rev_append l2 (rev_append [] l1);;

(* I/O operations *)

type in_channel ;;
type out_channel ;;

external open_descriptor_out: int[`a] <[`b]->
                              out_channel[_] = "caml_open_descriptor" ;;
external open_descriptor_in: int[`a] <[`b]->
                              in_channel[_] = "caml_open_descriptor" ;;

let stdin = open_descriptor_in 0 ;;
let stdout = open_descriptor_out 1 ;;
let stderr = open_descriptor_out 2 ;;

(* General output functions *)

type open_flag =
    Open_rdonly | Open_wronly | Open_append
  | Open_creat | Open_trunc | Open_excl
  | Open_binary | Open_text | Open_nonblock ;;

external open_desc: string[`a] <[`b]-> open_flag[`c] list[`d] <[`e]->
                    int[`f] <[`g]-> int[_] = "sys_open" ;;

let open_out_gen mode perm name =
  open_descriptor_out(open_desc name mode perm) ;;

let open_out name =
  open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_text] 0o666 name ;;

let open_out_bin name =
  open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] 0o666 name ;;

external flush : out_channel[`a] <[`b]-> unit[() : Pre;`c] = "caml_flush" ;;

external unsafe_output : out_channel[`a] <[`b]-> string[`c] <[`d]->
                         int[`e] <[`f]-> int[`g] <[`h]-> unit[() :Pre;`i]
                       = "caml_output" ;;

external output_char : out_channel[`a] <[`b]-> char[`c] <[`e]->
                       unit[():Pre;`f] = "caml_output_char" ;;

let output_string oc s =
  unsafe_output oc s 0 (string_length s) ;;

let output oc s ofs len =
  if ofs < 0 or ofs + len > string_length s
  then invalid_arg "output"
  else unsafe_output oc s ofs len ;;

external output_byte : out_channel[`a] <[`b]-> int[`c] <[`d]->
                       unit[() :Pre;`e] = "caml_output_char" ;;
external output_binary_int : out_channel[`a] <[`b]-> int[`c] <[`d]->
                             unit[() :Pre;`e] = "caml_output_int" ;;
(*
external marshal_to_channel : out_channel[`a] <[`b]-> 'a <[`c]->
                              unit["()":Pre;`d] list <[`e]->
                              unit["()":Pre;`f]
			= "output_value" ;;
*)
(* SIMULATION WHILE WE DON'T HAVE A TYPE SYNTAX POWERFUL ENOUGHT *)
let marshal_to_channel (oc : out_channel) x l =
 match l with [] -> () | () :: _ -> ()
;;

let output_value chan v = marshal_to_channel chan v [] ;;

external seek_out : out_channel[`a] <[`b]-> int[`c] <[`d]-> unit[() :Pre;`e]
                   = "caml_seek_out" ;;
external pos_out : out_channel[`a] <[`b]-> int[_] = "caml_pos_out" ;;
external out_channel_length : out_channel[`a] <[`b]-> int[_]
                     = "caml_channel_size" ;;
external close_out_channel : out_channel[`a] <[`b]-> unit[() :Pre;`c]
                     = "caml_close_channel" ;;
let close_out oc = flush oc; close_out_channel oc

(* General input functions *)

let open_in_gen mode perm name =
  open_descriptor_in(open_desc name mode perm) ;;

let open_in name =
  open_in_gen [Open_rdonly; Open_text] 0 name ;;

let open_in_bin name =
  open_in_gen [Open_rdonly; Open_binary] 0 name ;;

external input_char : in_channel[`a] <[`b]-> char[_] = "caml_input_char" ;;

external unsafe_input : in_channel[`a] <[`b]-> string[`c] <[`d]->
                        int[`e] <[`f]-> int[`g] <[`h]-> int[_]
                      = "caml_input" ;;

let input ic s ofs len =
  if ofs < 0 or ofs + len > string_length s
  then invalid_arg "input"
  else unsafe_input ic s ofs len ;;

let rec unsafe_really_input ic s ofs len =
  if len <= 0 then () else begin
    let r = unsafe_input ic s ofs len in
    if r = 0
    then raise End_of_file
    else unsafe_really_input ic s (ofs+r) (len-r)
  end ;;

let really_input ic s ofs len =
  if ofs < 0 or ofs + len > string_length s
  then invalid_arg "really_input"
  else unsafe_really_input ic s ofs len ;;

external input_scan_line : in_channel[`a] <[`b]-> int[_]
             = "caml_input_scan_line" ;;

let rec input_line chan =
  let n = input_scan_line chan in
  if n = 0 then                         (* n = 0: we are at EOF *)
    raise End_of_file
  else if n > 0 then begin              (* n > 0: newline found in buffer *)
    let res = string_create (n-1) in
    unsafe_input chan res 0 (n-1);
    input_char chan;                    (* skip the newline *)
    res
  end else begin                        (* n < 0: newline not found *)
    let beg = string_create (-n) in
    unsafe_input chan beg 0 (-n);
    try
      beg ^ input_line chan
    with End_of_file ->
      beg
  end ;;

external input_byte : in_channel[`a] <[`b]-> int[_] = "caml_input_char" ;;
external input_binary_int : in_channel[`a] <[`b]-> int[_] = "caml_input_int" ;;
external input_value : in_channel[`a] <[`b]-> 'a = "input_value" ;;
external seek_in : in_channel[`a] <[`b]-> int[`c] <[`d]->
                   unit[() :Pre;`e] = "caml_seek_in" ;;
external pos_in : in_channel[`a] <[`b]-> int[_] = "caml_pos_in" ;;
external in_channel_length : in_channel[`a] <[`b]->
                             int[_] = "caml_channel_size" ;;
external close_in : in_channel[`a] <[`b]-> unit[() :Pre;`c]
                   = "caml_close_channel" ;;

(* Output functions on standard output *)

let print_char c = output_char stdout c ;;
let print_string s = output_string stdout s ;;
let print_int i = output_string stdout (string_of_int i) ;;
let print_float f = output_string stdout (string_of_float f) ;;
let print_endline s = output_string stdout s; output_char stdout '\n' ;;
let print_newline () = output_char stdout '\n'; flush stdout ;;

(* Output functions on standard error *)

let prerr_char c = output_char stderr c ;;
let prerr_string s = output_string stderr s ;;
let prerr_int i = output_string stderr (string_of_int i) ;;
let prerr_float f = output_string stderr (string_of_float f) ;;
let prerr_endline s =
  output_string stderr s; output_char stderr '\n'; flush stderr ;;
let prerr_newline () = output_char stderr '\n'; flush stderr ;;

(* Input functions on standard input *)

let read_line () = flush stdout; input_line stdin ;;
let read_int () = int_of_string(read_line()) ;;
let read_float () = float_of_string(read_line()) ;;

(* References *)
(*
Here is an example of external definition :
 external v : int["6":Pre;`a] ref [{ contents : int["6":Pre;`a] } ; `b] = "" ;;
*)

type 'a ref = { mutable contents: 'a } ;;
external ref : 'a <[`a]-> 'a ref [{ contents : 'a }; `b] = "%makemutable" ;;
external (!): 'a ref[{ contents : 'a }; `a] <[`b]-> 'a = "%field0" ;;
external (:=): 'a ref[{ contents : 'a }; `a] <[`b]-> 'a <[`c]->
    unit[() :Pre; `d] = "%setfield0" ;;

let incr x = x := !x + 1 ;;
external incr: int[_] ref[{ contents : int[_] }; `a] <[`b]->
  unit[() :Pre; `c] = "%incr" ;;
external decr: int[_] ref[{ contents : int[_] }; `a] <[`b]->
  unit[() :Pre; `c] = "%decr" ;;

(* Miscellaneous *)

external sys_exit : int[`a] <[`b]-> 'a = "sys_exit" ;;


let exit_function = ref (fun () -> flush stdout; flush stderr) ;;

let at_exit f =
  let g = !exit_function in
  exit_function := (fun () -> f(); g()) ;;

(* Protection Wrapper to localize accumulations due to the reference *)
exception ExitWrapper of exn ;;
let do_at_exit () =
  try (!exit_function) ()
  with whatever -> raise (ExitWrapper whatever) ;;

let exit retcode =
  do_at_exit ();
  sys_exit retcode ;;

(*
let at_exit f = () ;;

let exit retcode =
  sys_exit retcode ;;
*)
