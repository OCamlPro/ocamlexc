(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(* Syntax tree once scoped.                                            *)
(* Maps Longident to Paths.                                            *)
(* Remove mod_type in functor expressions.                             *)
(* Forget module constraints : transform to a simple module expr       *)
(***********************************************************************)

(* $Id: scopedtree.mli,v 1.24 1999/11/09 17:48:22 pessaux Exp $ *)

(* Abstract syntax tree produced by parsing *)

open Asttypes

(* Type expressions for annoted types language *)

type annoted_core_type =
  { patyp_desc: annoted_core_type_desc;
    patyp_loc: Location.t }

and annoted_core_type_desc = 
    Patyp_var of string
  | Patyp_arrow of annoted_core_type * annotation_expr * annoted_core_type
  | Patyp_tuple of annoted_core_type list
  | Patyp_constr of Path.t ref * annoted_core_type list * annotation_expr

and annotation_expr =
  | Pann_bottom
  | Pann_explicit of annotation_elem_expr list * string

and annotation_elem_expr =
  | Pelem_constant of string * presence_expr
  | Pelem_param of (string * annoted_core_type)
  | Pelem_record of (string * annoted_core_type) list

and presence_expr =
  | Prexpr_pre
  | Prexpr_var of string
;;

(* Type expressions for the core language *)

type core_type =
  { ptyp_desc: core_type_desc;
    ptyp_loc: Location.t }

and core_type_desc = 
    Ptyp_any
  | Ptyp_var of string
  | Ptyp_arrow of core_type * core_type
  | Ptyp_tuple of core_type list
  | Ptyp_constr of Path.t ref * core_type list
  | Ptyp_alias of core_type * string

and core_field_type =
  { pfield_desc: core_field_desc;
    pfield_loc: Location.t }

and core_field_desc =
    Pfield of string * core_type
  | Pfield_var

(* XXX Type expressions for the class language *)

type 'a class_infos =
  { pci_virt: virtual_flag;
    pci_params: string list * Location.t;
    pci_name: Ident.t ;
    pci_expr: 'a;
    pci_loc: Location.t }

(* Value expressions for the core language *)

type pattern =
  { ppat_desc: pattern_desc;
    ppat_loc: Location.t }

and pattern_desc =
    Ppat_any
  | Ppat_var of Ident.t
  | Ppat_alias of pattern * Ident.t
  | Ppat_constant of constant
  | Ppat_tuple of pattern list
  | Ppat_construct of Path.t * pattern option
  | Ppat_record of (Path.t * pattern) list
  | Ppat_array of pattern list
  | Ppat_or of pattern * pattern
  | Ppat_constraint of pattern * core_type

type expression =
  { pexp_desc: expression_desc;
    pexp_loc: Location.t }

and expression_desc =
    Pexp_ident of Path.t
  | Pexp_constant of constant
  | Pexp_let of rec_flag * (pattern * expression) list * expression
  | Pexp_function of (pattern * expression) list
  | Pexp_apply of expression * expression list
  | Pexp_match of expression * (pattern * expression) list
  | Pexp_try of expression * (pattern * expression) list
  | Pexp_tuple of expression list
  | Pexp_construct of Path.t * expression option
  | Pexp_record of (Path.t * expression) list * expression option
  | Pexp_field of expression * Path.t
  | Pexp_setfield of expression * Path.t * expression
  | Pexp_array of expression list
  | Pexp_ifthenelse of expression * expression * expression option
  | Pexp_sequence of expression * expression
  | Pexp_while of expression * expression
  | Pexp_for of Ident.t * expression * expression * direction_flag * expression
  | Pexp_constraint of expression * core_type option * core_type option
  | Pexp_when of expression * expression
  | Pexp_letmodule of Ident.t * module_expr * expression

(* Value descriptions *)

and value_description =
  { pval_type: core_type;
    pval_prim: string list }

and annoted_value_description =
  { paval_type: annoted_core_type;
    paval_prim: string list ;
    paval_explaination: (string * annoted_core_type) list  (* Liste des *)
                 (* clauses with id = type annoté servant a décrire les *)
                 (* types annotés cycliques. Rajout pour l'analyseur !  *)
  }


(* Type declarations *)

and type_declaration =
  { ptype_params: string list;
    ptype_cstrs: (core_type * core_type * Location.t) list;
    ptype_kind: type_kind;
    ptype_manifest: core_type option;
    ptype_loc: Location.t ;
    ptype_path: Path.t ref (* Path used to create ALL type expression
                              of this type. Must be a reference on the
                              path created to identify this type
			      declaration in the englobing structure. *) }

and type_kind =
    Ptype_abstract
  | Ptype_variant of (Ident.t * core_type list) list
  | Ptype_record of (Ident.t * mutable_flag * core_type) list

and exception_declaration = core_type option

(* Type expressions for the module language *)

and module_type =
  { pmty_desc: module_type_desc;
    pmty_loc: Location.t }

and module_type_desc =
    Pmty_ident of Path.t
  | Pmty_signature of signature
  | Pmty_functor of string * module_type * module_type

and signature = signature_item list

and signature_item =
  { psig_desc: signature_item_desc;
    psig_loc: Location.t }

and signature_item_desc =
    Psig_value of string * value_description
  | Psig_type of (Ident.t * type_declaration) list    (* The Ident.t is only
                                 used to retrieve the type definition in the
				 structure when needed. If we want to create
                                 a type expression of this type, we must use
                                 the reference contained in this declaration
                                 of type. *)
  | Psig_exception of string * exception_declaration
  | Psig_module of string * module_type
  | Psig_modtype of string * modtype_declaration
  | Psig_open of Path.t
  | Psig_include of module_type

and modtype_declaration =
    Pmodtype_abstract
  | Pmodtype_manifest of module_type

and with_constraint =
    Pwith_type of type_declaration
  | Pwith_module of Path.t

(* Value expressions for the module language *)

and module_expr =
  { pmod_desc: module_expr_desc;
    pmod_loc: Location.t }

and module_expr_desc =
    Pmod_ident of Path.t
  | Pmod_structure of structure
  | Pmod_functor of Ident.t * module_expr
  | Pmod_apply of module_expr * module_expr

and structure = structure_item list

and structure_item =
  { pstr_desc: structure_item_desc;
    pstr_loc: Location.t }

and structure_item_desc =
    Pstr_eval of expression
  | Pstr_value of rec_flag * (pattern * expression) list
  | Pstr_primitive of Ident.t * annoted_value_description
  | Pstr_type of (Ident.t * type_declaration) list
  | Pstr_exception of Ident.t * exception_declaration
  | Pstr_module of Ident.t * module_expr
  | Pstr_modtype (* Let's forget module type be we don't need sig matching *)
  | Pstr_open of Path.t


(* Toplevel phrases *)

type toplevel_phrase =
    Ptop_def of structure
  | Ptop_dir of string * directive_argument

and directive_argument =
    Pdir_none
  | Pdir_string of string
  | Pdir_int of int
  | Pdir_ident of Path.t
