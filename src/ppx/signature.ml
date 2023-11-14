open Ppxlib
open Parsetree
(* open Utils *)

let map_signature_item mapper ({ psig_desc } as signature_item) =
  match psig_desc with
  | Psig_type (_, _) -> [ mapper#signature_item signature_item ]
  | _ -> [ mapper#signature_item signature_item ]
