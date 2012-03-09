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



(* Display explicit error message for lexical errors *)
let handle_lexing_error err start stop =
  (match err with
   | Lexer.Illegal_character ->
       fprintf std_formatter "Illegal character"
   | Lexer.Unterminated_comment ->
       fprintf std_formatter "Unterminated comment"
   | Lexer.Unterminated_string ->
       fprintf std_formatter "Unterminated string") ;
  fprintf std_formatter " between characters %d and %d\n" start stop
;;



(* Display explicit error message for syntax errors *)
let handle_parsing_error = function
  | Syntaxerr.Unclosed (loc1, s1, loc2, s2) ->
      Location.print loc1 ;
      Location.print loc2 ;
      fprintf std_formatter
             "Syntax error. May be '%s' should match '%s'\n" s2 s1;
  | Syntaxerr.Other loc ->
      Location.print loc ;
      fprintf std_formatter "Syntax horror\n" ;
;;



(* Display explicit error message for unbound token errors when scoping *)
let handle_scoping_error err =
  fprintf std_formatter "Unbound (while scoping) " ;
  (match err with
   | Envscope.Unbound_value name -> fprintf std_formatter "value %s" name
   | Envscope.Unbound_type name -> fprintf std_formatter "type %s" name
   | Envscope.Unbound_module name -> fprintf std_formatter "module %s" name
   | Envscope.Unbound_label name -> fprintf std_formatter "label %s" name) ;
  fprintf std_formatter "\n"
;;



let handle_unification_error ty0 ty1 =
 (* Warning, we don't re-init the already seen types list by calling *)
 (* directly pp_ml_type.                                             *)
 fprintf std_formatter "Incompatibility between %a and %a\n"
                       Printtypes.pp_ml_type ty0
                       Printtypes.pp_ml_type ty1
;;



let handle_circularization_error ty0 ty1 =
 (* Warning, we don't re-init the already seen types list by calling *)
 (* directly pp_ml_type.                                             *)
 fprintf std_formatter "Incompatibility (due to non regular datatype) between %a and %a\n"
                       Printtypes.pp_ml_type ty0
                       Printtypes.pp_ml_type ty1
;;



(* Display explicit error message for unbound token errors when typing *)
let handle_typing_error err =
  fprintf std_formatter "Unbound (while typing) " ;
  (match err with
   | Envtype.Unbound_value path ->
       fprintf std_formatter "value %a" Path.pp_path_debug path
   | Envtype.Unbound_type path ->
       fprintf std_formatter "type %a" Path.pp_path_debug path
   | Envtype.Unbound_module path ->
       fprintf std_formatter "module %a" Path.pp_path_debug path
   | Envtype.Unbound_constructor path ->
       fprintf std_formatter "constructor %a" Path.pp_path_debug path
   | Envtype.Unbound_component name ->
       fprintf std_formatter "component %s" name
   | Envtype.Unbound_label path ->
       fprintf std_formatter "label %a" Path.pp_path_debug path) ;
  fprintf std_formatter "\n"
;;
