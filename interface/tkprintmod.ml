(***********************************************************************)
(*                                                                     *)
(*                           Ocamlexc                                  *)
(*                                                                     *)
(*        Francois Pessaux, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(* Pretty print of module types.                                       *)
(***********************************************************************)




open Format ;;
open Tk ;;



(* create_mark_radical: unit -> string *)
(* Creates a new mark core-name        *)
let create_mark_radical =
 let cpt = ref 0 in
 function () ->
  let name = "MARK" ^ (string_of_int !cpt) in
  incr cpt ;
  name
;;



(* pp_label_description: Format.formatter -> Typedtree.label_description -> *)
(*                       unit                                               *)
let pp_label_description pcontext ppf lbl_desc =
  fprintf ppf "%a:@ %a" Printbasic.pp_mutable_flag lbl_desc.Typedtree.fld_mut
          (Tkprinttypes.pp_ml_type_scheme pcontext)
          lbl_desc.Typedtree.fld_scheme
;;



(* pp_signature: Format.formatter -> Typedtree.signature_item list -> unit *)
(* Pretty print signatures *)
let rec pp_signature pcontext ppf signature = 
  let iter_pp_signature_item =
    Printbasic.iter_pp "" (pp_signature_item pcontext) in
  fprintf ppf "%a" iter_pp_signature_item signature



(* pp_signature_item: Format.formatter -> Typedtree.signature_item -> unit *)
(* Pretty print signature items *)
and pp_signature_item pcontext ppf = function
  | Typedtree.Tsig_value (ident, scheme) ->
      let mark_r = create_mark_radical () in
      let pcontext' = { pcontext with Printcontext.mark_radical = mark_r } in
      fprintf ppf "@<0>%sval@<0>%s@[ %s:@ @<0>%s%a@]@<0>%s"
              "\006VAL\007" "\006\007"
              (Ident.name ident) ("\006START"^mark_r^"\008")
              (Tkprinttypes.pp_ml_type_scheme pcontext') scheme
              ("\006STOP"^mark_r^"\008") ;
  | Typedtree.Tsig_type (ident, ty_declaration) ->
      fprintf ppf "typ@[e %a@]"
              (Tkprinttypes.pp_type_declaration pcontext)
              (ident, ty_declaration)
  | Typedtree.Tsig_module (ident, mod_ty) ->
      fprintf ppf "@<0>%smod@[ule@<0>%s %s :@ %a@]"
              "\006MODULE\007" "\006\007"
              (Ident.name ident) (pp_module_type pcontext) mod_ty
  | Typedtree.Tsig_constructor (id, cd) ->
      (* As specified in typedtree.mli, this the constructor comes *)
      (* from a type declaration, this kind of component is hidden *)
      (* to the user. It only serves to keep trace of constructors *)
      (* induced by type definitions in a module. So don't print ! *)
      (* Else if it comes from an exception, we must display it !  *)
      let mark_r = create_mark_radical () in
      let pcontext' = { pcontext with
			Printcontext.mark_radical = mark_r } in
      begin
      match cd.Typedtree.cstr_kind with
       | Typedtree.Exn file ->
	   fprintf ppf "@<0>%sexc@[eption@<0>%s %s__%s :@ @<0>%s%a@]@<0>%s"
                   "\006CONSTRUCTOR\007" "\006\007"
                   file (Ident.name id)
	           ("\006START"^mark_r^"\008")
                   (Tkprinttypes.pp_ml_type_scheme pcontext')
                   cd.Typedtree.cstr_scheme
	           ("\006STOP"^mark_r^"\008")
       | Typedtree.Sum ->
	   fprintf ppf "@<0>%scon@[structor@<0>%s %s :@ @<0>%s%a@]@<0>%s"
                   "\006CONSTRUCTOR\007" "\006\007"
	           (Ident.name id)
	           ("\006START"^mark_r^"\008")
                   (Tkprinttypes.pp_ml_type_scheme pcontext')
                   cd.Typedtree.cstr_scheme
	           ("\006STOP"^mark_r^"\008")
      end
  | Typedtree.Tsig_label (id, lbl_descr) ->
      let mark_r = create_mark_radical () in
      let pcontext' = { pcontext with
			Printcontext.mark_radical = mark_r } in
      fprintf ppf "@<0>%srec@[ord label@<0>%s %s @<0>%s%a@]@<0>%s"
              "\006LABEL\007" "\006\007"
              (Ident.name id)
              ("\006START"^mark_r^"\008")
              (pp_label_description pcontext') lbl_descr
              ("\006STOP"^mark_r^"\008")



and pp_module_type pcontext ppf = function
  | Typedtree.Tmty_signature signature ->
      fprintf ppf "@[<hv>sig@;<1 1>@[<hv>%a@]@ end@]"
              (pp_signature pcontext) signature
  | Typedtree.Tmty_functor (_, _) ->
      fprintf ppf "functor (body skiped : only applications are displayed)"
;;
