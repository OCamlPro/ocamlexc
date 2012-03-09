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




(* Type for identifiers : includes name and stamp to disambiguate *)
(* identifiers having the same name                               *)
type t = { name : string; stamp : int } ;;


let currstamp = ref 0 ;;


(* create: string -> t                     *)
(* Generate a new ident with base name 's' *)
let create s =
  currstamp := !currstamp + 1 ;
  { name = s ; stamp = !currstamp }
;;



(* create_global: string -> t *)
(* Generate a new ident with base name 's' at toplevel (stamp = 0) *)
let create_global s =
  { name = s ; stamp = 0 }
;;



(* name: t -> string                  *)
(* Retrieve ident name (forget stamp) *)
let name id = id.name ;;



(* stamp: t -> int       *)
(* Retrieve ident stamp *)
let stamp id = id.stamp ;;



(* equal: t -> t -> bool   *)
(* Equality on identifiers *)
let equal id1 id2 = (id1.stamp = id2.stamp)  && (id1.name = id2.name) ;;



(* Generic environnement binding ident to generic data *)
type 'a tbl = (t * 'a) list ;;



(* emptytbl: 'a tbl  *)
(* Empty environment *)
let emptytbl = [] ;;



(* val add: t -> 'a -> 'a tbl -> 'a tbl                  *)
(* Add a binding beetween id and data in the environment *)
(* 'tbl' and return a new environment.                   *)
let add id data tbl = (id, data) :: tbl ;;


(* val append:  'a tbl -> 'a tbl -> 'a tbl *)
(* Appends 2 tables and return the new table *)
let append first second = first @ second ;;


(* find: t -> 'a tbl -> 'a                                    *)
(* Retrieve data associated to ident 'id1' or raise Not_found *)
(* if no such binding exists.                                 *)
let rec find id1 = function
  | [] -> raise Not_found
  | (id2, data) :: rem ->
      (* Debug stuff : track ids found during search *)
      (* Format.fprintf Format.std_formatter "... : %s/%d"
                        (name id2) (stamp id2) ;
      Format.print_flush () ; *)
      if equal id1 id2 then data
 else find id1 rem
;;


let int_ident = create "int" ;;
let char_ident = create "char" ;;
let string_ident = create "string" ;;
let float_ident = create "float" ;;
let unit_ident = create "unit" ;;
let bool_ident = create "bool" ;;
let exn_ident = create "exn" ;;
let list_ident = create "list" ;;
let array_ident = create "array" ;;
let format_ident = create "format" ;;

let nil_ident = create "[]" ;;
let cons_ident = create "::" ;;
let failure_ident = create "Failure" ;;
let invarg_ident = create "Invalid_argument" ;;
let void_ident = create "()" ;;
let eof_ident = create "End_of_file" ;;
let out_of_mem_ident = create "Out_of_memory" ;;
let stack_over_ident = create "Stack_overflow" ;;
let match_failure_ident = create "Match_failure" ;;

let true_ident = create "true" ;;
let false_ident = create "false" ;;
let not_found_ident = create "Not_found" ;;
let division_by_zero_ident = create "Division_by_zero" ;;
let sys_error_ident = create "Sys_error" ;;
