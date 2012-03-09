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

type t =
  | Pident of Ident.t                   (* Identifier *)
  | Pdot of t * string                  (* Access to a module component *)
;;


(* path_equal: t -> t -> bool *)
(* Equality on paths *)
let rec equal p1 p2 =
  match (p1, p2) with
   | (Pident id1, Pident id2) -> Ident.equal id1 id2
   | (Pdot(r1, field1), Pdot(r2, field2)) -> equal r1 r2 & field1 = field2
   | (_, _) -> false
;;



(* val pp_path: Format.formatter -> t -> unit *)
(* Print a path *)
let rec pp_path ppf = function
  | Pident ident -> fprintf ppf "%s" (Ident.name ident)
  | Pdot (path, str) -> fprintf ppf "%a.%s" pp_path path str
;;


(* val pp_path_debug: Format.formatter -> t -> unit *)
(* Print a path with its stamp. Mainly for debug purpose *)
let rec pp_path_debug ppf = function
  | Pident ident ->
      let id_name = Ident.name ident in
      let id_stamp = Ident.stamp ident in
      fprintf ppf "%s(%d)" id_name id_stamp
  | Pdot (path, str) -> fprintf ppf "%a.%s" pp_path_debug path str
;;


(* val end_name: t -> string *)
(* Gives the name ending the path. I.e: "Foo.Bar.name" --> "name" *)
let end_name = function
  | Pident ident -> Ident.name ident
  | Pdot (_, name) -> name
;;

