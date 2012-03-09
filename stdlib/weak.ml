(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Damien Doligez, projet Para, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id: weak.ml,v 1.2 1998/10/13 17:11:10 pessaux Exp $ *)

(* Weak array operations *)

type 'a t;;

external create: int[`a] <[`b]-> 'a t[_] = "weak_create";;

let length x = Obj.size(Obj.repr x) - 1;;

(* SIMULATION WHILE WE DON'T HAVE A TYPE SYNTAX POWERFUL ENOUGHT *)
(* external set : 'a t[_] <[`a]-> int[`b] <[`c]-> 'a option <[`d]->
                            unit["()":Pre; `e] = "weak_set";; *)
let set v x y =
  if true then v else create x ;
  if true then None else y ;
  ()
;;

(* SIMULATION WHILE WE DON'T HAVE A TYPE SYNTAX POWERFUL ENOUGHT *)
(* external get: 'a t[_] <[`a]-> int[`b] <[`c]-> 'a option = "weak_get";; *)
let get v x =
 if true then v else create x ; None
;;

let fill ar ofs len x =
  if ofs < 0 || ofs + len > length ar
  then raise (Invalid_argument "Weak.fill")
  else begin
    for i = ofs to (ofs + len - 1) do
      set ar i x
    done
  end
;;

let blit ar1 of1 ar2 of2 len =
  if of1 < 0 || of1 + len > length ar1 || of2 < 0 || of2 + len > length ar2
  then raise (Invalid_argument "Weak.blit")
  else begin
    if of2 > of1 then begin
      for i = 0 to len - 1 do
        set ar2 (of2 + i) (get ar1 (of1 + i))
      done
    end else begin
      for i = len - 1 downto 0 do
        set ar2 (of2 + i) (get ar1 (of1 + i))
      done
    end
  end
;;

