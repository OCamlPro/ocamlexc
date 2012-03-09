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


type label_description = {
    (* Mutability for this field *)
    fld_mut: Asttypes.mutable_flag ;
    (* Full type scheme for this field, i.e arg -> ty result *)
    fld_scheme: Typecore.ml_types_scheme ;
    (* List of all fields in the type his field belongs to *)
    fld_countall: int
  } 
;;


type type_declaration =
  { type_params: Typecore.ml_type_expr list; (* Arguments du type. *)
                (* Attention, ils sont généralisés en même temps que l'on *)
                (* créé le schéma se trouvant dans le champ type_manifest *)
    type_arity: int;
    type_kind: type_kind;
    type_manifest: Typecore.ml_types_scheme option ; (* Schéma résultat *)
                       (* de ce à quoi est égal une expression de ce type *)
                       (* N'inclue pas les paramètres. Autrement dit, *)
                       (* type 'a t = 'a * int mettra Some ('a * int) dans *)
                       (* ce champ. *)
    type_path: Path.t ref (* Path used to create ALL type expression
                              of this type. Must be a reference on the
                              path created to identify this type
			      declaration in the englobing structure. *)
  }

and type_kind =
    Type_abstract
  | Type_variant of (Ident.t * Typecore.ml_types_scheme) list
  | Type_record of
      (Ident.t * Asttypes.mutable_flag * Typecore.ml_types_scheme) list
;;



type pattern =
  { pat_desc: pattern_desc;
    pat_loc: Location.t }

and pattern_desc =
    Tpat_any
  | Tpat_var of Ident.t
  | Tpat_alias of pattern * Ident.t
  | Tpat_constant of Asttypes.constant
  | Tpat_tuple of pattern list
  | Tpat_construct of Path.t * pattern option
  | Tpat_record of (Path.t * pattern) list
  | Tpat_array of pattern list
  | Tpat_or of pattern * pattern
  | Tpat_constraint of pattern * Scopedtree.core_type
;;



type expression =
  { exp_desc: expression_desc;
    exp_loc: Location.t;
    exp_type: Typecore.ml_type_expr ;
    exp_exn: Typecore.phi_expr }

and expression_desc =
    Texp_ident of Path.t
  | Texp_constant of Asttypes.constant
  | Texp_let of Asttypes.rec_flag * (pattern * expression) list * expression
  | Texp_function of (pattern * expression) list
  | Texp_apply of expression * expression list
  | Texp_match of expression * (pattern * expression) list
  | Texp_try of expression * (pattern * expression) list
  | Texp_tuple of expression list
  | Texp_construct of Path.t * expression option
  | Texp_record of (Path.t * expression) list * expression option
  | Texp_field of expression * Path.t
  | Texp_setfield of expression * Path.t * expression
  | Texp_array of expression list
  | Texp_ifthenelse of expression * expression * expression option
  | Texp_sequence of expression * expression
  | Texp_while of expression * expression
  | Texp_for of
      Ident.t * expression * expression * Asttypes.direction_flag * expression
  | Texp_constraint of expression * Scopedtree.core_type option
                                  * Scopedtree.core_type option
  | Texp_when of expression * expression
(*
  | Texp_override of Path.t * (Path.t * expression) list
*)
  | Texp_letmodule of Ident.t * module_expr * expression


and module_expr =
  { mod_desc: module_expr_desc;
    mod_loc: Location.t;
    mod_type: module_type }

and module_expr_desc =
    Tmod_ident of Path.t
  | Tmod_structure of structure
  | Tmod_functor of Ident.t * Scopedtree.module_expr
  | Tmod_apply of module_expr * module_expr (* * module_coercion *)
(*  | Tmod_constraint of module_expr * module_type * module_coercion *)

and structure = structure_item list

and structure_item =
    Tstr_eval of expression
  | Tstr_value of Asttypes.rec_flag * (pattern * expression) list
  | Tstr_primitive of Ident.t * Scopedtree.annoted_value_description
  | Tstr_type of (Ident.t * type_declaration) list
  | Tstr_exception of Ident.t (* * exception_declaration *)
                              (* On se fout de la declaration car on range *)
                              (* le constructeur dans l'environnement et   *)
                              (* c'est par cet environnement qu'on va      *)
                              (* retrouver le type du constructeur.        *)
  | Tstr_module of Ident.t * module_expr

and module_type =
  (*  Tmty_ident of Path.t *)
  | Tmty_signature of signature
  | Tmty_functor of Ident.t * Scopedtree.module_expr

and signature = signature_item list


(* Constructor arity *)
and constructor_arity = Zeroary | Unary

and constructor_kind = Exn of string  (* Source filename of the exception *)
 | Sum

(* Information binded to constructor ident in typing environment *)
and constructor_description = {
    (* Tells wether this constructor comes from an exception or a sum type *)
    cstr_kind: constructor_kind ;
    (* Arity : 0 or 1 (many = 1 type tuple), (1 = type, not a 1 tuple) *)
    cstr_arity: constructor_arity ;
    (* Full type scheme for this constructor, i.e (args ->) ty result *)
    cstr_scheme: Typecore.ml_types_scheme ;
  }

and signature_item =
  (* Values also contains primitives *)
    Tsig_value of Ident.t * Typecore.ml_types_scheme
  | Tsig_type of Ident.t * type_declaration
  | Tsig_module of Ident.t * module_type
  (* This kind of component does not appear in the displayed type. It's only *)
  (* used to keep trace of constructors defined in a module via types        *)
  (* declarations. If we had signature matching, this kind of component      *)
  (* should also not be taken in account for this operation !                *)
  (* This serves to be able to enter them as values in the typing environt.  *)
  | Tsig_constructor of Ident.t * constructor_description
  | Tsig_label of (Ident.t * label_description)


(* Typing environment *)
and envt = {
  values : Typecore.value_binding Ident.tbl ;
  types : type_declaration Ident.tbl ;
  modules : module_type Ident.tbl ;
  constructors : constructor_description Ident.tbl ;
  labels : label_description Ident.tbl }
;;



type toplevel_phrase =
    Ptop_def of structure
  | Ptop_dir
;;
