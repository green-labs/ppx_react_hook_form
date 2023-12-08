open Ppxlib
open Parsetree
open Ast_helper

let attribute_name = "rhf"
let fail loc message = Location.raise_errorf ~loc "%s" message
let mkloc txt loc = { Location.txt; loc }
let mknoloc txt = mkloc txt Location.none
let lid ?(loc = Location.none) s = mkloc (Longident.parse s) loc
let capitalize = String.capitalize_ascii

let make_const_decls labels loc =
  labels
  |> List.map (fun label -> (String.capitalize_ascii label, label))
  |> List.map (fun (upper, lower) ->
         Type.constructor ~loc (mkloc upper loc)
           ~attrs:
             [
               Attr.mk (mkloc "as" loc)
                 (PStr [ Str.eval @@ Exp.constant (Const.string lower) ]);
             ])
(* make constructor declaration with label *)

let attr_named_arg = Attr.mk (mknoloc "res.namedArgLoc") (PStr [])
let attr_template = Attr.mk (mknoloc "res.template") (PStr [])
let attr_uapp = Attr.mk (mknoloc "res.uapp") (PStr [])
let attr_brace = Attr.mk (mknoloc "res.braces") (PStr [])
let has_attribute { attr_name = { Location.txt } } = txt = attribute_name

let remove_optional_attribute (attrs : attributes) : attributes =
  List.filter
    (fun ({ attr_name = { Location.txt } } : attribute) ->
      txt <> "res.optional")
    attrs

let add_optional_attribute (attrs : attributes) : attributes =
  Attr.mk (mknoloc "res.optional") (PStr []) :: remove_optional_attribute attrs

let uncurried_core_type_arrow ?loc ?attrs ~arity core_types =
  Typ.constr ?loc ?attrs (lid "function$")
    (core_types
    @ [
        Typ.variant
          [ Rf.tag (mknoloc @@ "Has_arity" ^ Int.to_string arity) true [] ]
          Closed None;
      ])

let uncurried_expr_func ?(loc = Location.none) ~arity e =
  let attr_arity =
    Attr.mk { txt = "res.arity"; loc }
      (PStr [ Str.eval (Exp.constant (Const.int arity)) ])
  in
  Exp.construct ~attrs:[ attr_arity ] { txt = Lident "Function$"; loc } (Some e)
