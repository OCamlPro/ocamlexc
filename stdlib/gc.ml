(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Damien Doligez, projet Para, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id: gc.ml,v 1.1 1998/08/19 13:29:05 pessaux Exp $ *)

type stat = {
  minor_words : int;
  promoted_words : int;
  major_words : int;
  minor_collections : int;
  major_collections : int;
  heap_words : int;
  heap_chunks : int;
  live_words : int;
  live_blocks : int;
  free_words : int;
  free_blocks : int;
  largest_free : int;
  fragments : int;
  compactions : int
};;

type control = {
  mutable minor_heap_size : int;
  mutable major_heap_increment : int;
  mutable space_overhead : int;
  mutable verbose : bool;
  mutable max_overhead : int;
  mutable stack_limit : int
};;

external stat : unit[() :Pre;`a] <[`b]-> stat[_] = "gc_stat";;
external get : unit[() :Pre;`a] <[`b]-> control[_] = "gc_get";;
external set : control[`a] <[`b]-> unit[() :Pre;`c] = "gc_set";;
external minor : unit[() :Pre;`a] <[`b]-> unit[() :Pre;`c] = "gc_minor";;
external major : unit[() :Pre;`a] <[`b]-> unit[() :Pre;`c] = "gc_major";;
external full_major : unit[() :Pre;`a] <[`b]-> unit[() :Pre;`c]
                    = "gc_full_major";;
external compact : unit[() :Pre;`a] <[`b]-> unit[() :Pre;`c]
                    = "gc_compaction";;

let print_stat c =
  let st = stat () in
  Printf.fprintf c "minor_words: %d\n" st.minor_words;
  Printf.fprintf c "promoted_words: %d\n" st.promoted_words;
  Printf.fprintf c "major_words: %d\n" st.major_words;
  Printf.fprintf c "minor_collections: %d\n" st.minor_collections;
  Printf.fprintf c "major_collections: %d\n" st.major_collections;
  Printf.fprintf c "heap_words: %d\n" st.heap_words;
  Printf.fprintf c "heap_chunks: %d\n" st.heap_chunks;
  Printf.fprintf c "live_words: %d\n" st.live_words;
  Printf.fprintf c "live_blocks: %d\n" st.live_blocks;
  Printf.fprintf c "free_words: %d\n" st.free_words;
  Printf.fprintf c "free_blocks: %d\n" st.free_blocks;
  Printf.fprintf c "largest_free: %d\n" st.largest_free;
  Printf.fprintf c "fragments: %d\n" st.fragments;
  Printf.fprintf c "compactions: %d\n" st.compactions;
;;
