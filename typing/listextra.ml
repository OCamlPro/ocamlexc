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



let rec split3 = function
  | [] -> ([], [], [])
  | (x, y, z) :: rem ->
      let (rx, ry, rz) = split3 rem in (x :: rx, y :: ry, z :: rz)
;;


let rec sub e = function
  | [] -> []
  | h :: q -> if h = e then sub e q else h :: (sub e q)
;;


let rec map3 f l1 l2 l3 =
  match (l1, l2, l3) with
    ([], [], []) -> []
  | (a1::l1, a2::l2, a3::l3) ->
        let r = f a1 a2 a3 in r :: map3 f l1 l2 l3
  | (_, _, _) -> invalid_arg "map3"
;;


exception Filterskip ;;
let rec try_filter f = function
 | [] -> []
 | h :: q ->
     try let tmp = (f h) in tmp :: (try_filter f q) with
       Filterskip -> try_filter f q
;;
