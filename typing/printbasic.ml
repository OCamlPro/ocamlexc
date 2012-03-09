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




open Format ;;



let rec pp_longident ppf = function
  | Longident.Lident name -> fprintf ppf "%s" name
  | Longident.Ldot (lident, name) ->
      fprintf ppf "%a.%s" pp_longident lident name
;;



let pp_constant ppf = function
  | Asttypes.Const_int i -> fprintf ppf "%d" i
  | Asttypes.Const_char c -> fprintf ppf "'%s'" (Char.escaped c)
  | Asttypes.Const_string s -> fprintf ppf "\"%s\"" s
  | Asttypes.Const_float float_string -> fprintf ppf "%s" float_string
;;



let pp_rec_flag ppf = function
  | Asttypes.Nonrecursive -> ()
  | Asttypes.Recursive -> fprintf ppf "rec "
;;



let pp_direction_flag ppf = function
  | Asttypes.Upto -> fprintf ppf "to"
  | Asttypes.Downto -> fprintf ppf "downto"
;;



let pp_mutable_flag ppf = function
  | Asttypes.Immutable -> ()
  | Asttypes.Mutable -> fprintf ppf "mutable "
;;


let pp_private_flag ppf = function
  | Asttypes.Private -> fprintf ppf "private"
  | Asttypes.Public -> ()
;;



let pp_virtual_flag ppf = function
  | Asttypes.Virtual -> fprintf ppf "virtual"
  | Asttypes.Concrete -> ()
;;



(* val iter_pp: string ->                                                    *)
(*   (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit *)
(* Iterate a print function on a list, using 'seperator' as delimiter *)
let rec iter_pp separator pp_fun ppf = function
  | [] -> ()
  | h :: [] -> fprintf ppf "%a" pp_fun h
  | h :: rem -> fprintf ppf "%a%s@ " pp_fun h separator ;
                iter_pp separator pp_fun ppf rem
;;
