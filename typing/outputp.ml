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




(* output: string -> Envscope.t -> Typedtree.signature -> unit *)
(* Dumps in a persistent file al the informations resulting *)
(* from the analysis of the current compilation unit.       *)
let output filename mod_scope_info struct_signature typed_structure =
 (* Build the output file name *)
 let module_source = filename in
 let persist_name = Files.cme_filename_from_ml_filename filename in
 let out_channel = open_out persist_name in
 (* We must create a list of non generalized row variables *)
 let nfree_vars = Freevars.nfrv_signature [] struct_signature in
 (* Output everything at the same time to preserve sharing. *)
 output_value
    out_channel
    ((module_source : string),
     (* Non free variables for this compilation unit *)
     (nfree_vars : Freevars.persistent_binding list),
     (* Scope info for this compilation unit *)
     (mod_scope_info : Envscope.t),
     (* Signature for this compilation unit *)
     (struct_signature : Typedtree.signature),
     (* Assoc list recording which other compilation units this one needed *)
     (* and which instanciation have been made on their non free variables *)
     (* during the analysis of the current compilation unit.               *)
     (!Inputpt.loaded_compunits_nf_variables :
                (Freevars.persistent_binding list) Inputpt.assoclist),
     (typed_structure : Typedtree.structure)
    ) ;
 close_out out_channel
;;
