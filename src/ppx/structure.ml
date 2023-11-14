open Ppxlib
open Parsetree
(* open Ast_helper *)
(* open Utils *)

let map_structure_item mapper ({ pstr_desc } as structure_item) =
  match pstr_desc with
  | Pstr_type (_, _) -> [ mapper#structure_item structure_item ]
  | _ -> [ mapper#structure_item structure_item ]
