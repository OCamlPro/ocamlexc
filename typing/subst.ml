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




(* Type of substitutions : map a path to a path *)
type t = Path.t Ident.tbl ;;


(* val identity: t *)
(* Empty substitution *)
let identity = Ident.emptytbl ;;


(* val add: Ident.t -> path -> t -> t *)
(* Add a new mapping to the substitution and return *)
(* the extended substitution *)
let add = Ident.add ;;


(* val path: Path.t -> t -> Path.t *)
(* Apply the substitution 'sub' to the path 'p' *)
let rec path p sub =
  match p with
   | Path.Pident id -> (try Ident.find id sub with Not_found -> p)
   | Path.Pdot (root, field) -> Path.Pdot (path root sub, field)
;;
