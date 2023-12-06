open Ppxlib
open Ast_helper

class mapper =
  object (self)
    inherit Ast_traverse.map

    method! signature sign =
      sign |> List.map (Signature.map_signature_item self) |> List.concat

    method! structure strt =
      strt |> List.map (Structure.map_structure_item self) |> List.concat
  end

let signature_mapper = (new mapper)#signature
let structure_mapper = (new mapper)#structure

(* transform setValue argument to ReactHookForm.Value(expr)
    setValue(Name, rhfValue("woonki"))
    setValue(Age, rhfValue(0)) *)
let expand_rhf_value e =
  match e.pexp_desc with
  | Pexp_apply (_, [ (_, expr) ]) ->
      Some
        (Exp.construct
           (Utils.mkloc
              (Longident.Ldot (Lident "ReactHookForm", "Value"))
              e.pexp_loc)
           (Some expr))
  | _ -> Some e

let rule_rhf_value =
  Context_free.Rule.special_function "rhfValue" expand_rhf_value

(* transform the spread form
    <Form {...rhfSpread(form)} />*)
let expand_rhf_spread e =
  match e.pexp_desc with
  | Pexp_apply (_, [ (_, expr) ]) ->
      Some
        (Exp.apply
           ~attrs:[ Attr.mk (Utils.mknoloc "res.uapp") (PStr []) ]
           (Exp.ident
              (Utils.mkloc (Longident.Ldot (Lident "Obj", "magic")) e.pexp_loc))
           [ (Nolabel, expr) ])
  | _ -> Some e

let rule_rhf_spread =
  Context_free.Rule.special_function "rhfSpread" expand_rhf_spread

let _ =
  Ppxlib.Driver.register_transformation ~preprocess_impl:structure_mapper
    ~preprocess_intf:signature_mapper
    ~rules:[ rule_rhf_value; rule_rhf_spread ]
    "rhf"
