open Ppxlib
open Parsetree
open Ast_helper
open Utils

let map_type_decl
    {
      ptype_attributes;
      ptype_name = { txt };
      ptype_manifest;
      ptype_kind;
      ptype_loc;
    } =
  if List.exists has_attribute ptype_attributes then
    match (ptype_manifest, ptype_kind) with
    | None, Ptype_record lds ->
        let fields = lds |> List.map (fun { pld_name = { txt } } -> txt) in
        let type_decls =
          Str.type_ Recursive
            [
              (* type useFormReturnOfInputs<'setValueAs> = {
                   register: (variantOfInputs, ~options: registerOptionsOfInputs<'setValueAs>=?) => JsxDOM.domProps,
                   handleSubmit: (inputs => unit) => JsxEvent.Form.t => unit,
                   watch: variantOfInputs => watchReturnOfInputs,
                   formState: formStateOfInputs,
                 } *)
              Type.mk
                (mkloc
                   ("useFormReturnOf" ^ String.capitalize_ascii txt)
                   ptype_loc)
                ~params:[ (Typ.var "setValueAs", (NoVariance, NoInjectivity)) ]
                ~priv:Public
                ~kind:
                  (Ptype_record
                     [
                       (* register: (variantOfInputs, ~options: registerOptionsOfInputs=?) => JsxDOM.domProps, *)
                       Type.field ~mut:Immutable (mknoloc "register")
                         (uncurried_core_type_arrow ~arity:2
                            [
                              Typ.arrow Nolabel
                                (Typ.constr
                                   (lid @@ "variantOf"
                                   ^ String.capitalize_ascii txt)
                                   [])
                                (Typ.arrow (Optional "options")
                                   (Typ.constr
                                      ~attrs:
                                        [
                                          Attr.mk
                                            (mknoloc "res.namedArgLoc")
                                            (PStr []);
                                        ]
                                      (lid @@ "registerOptionsOf"
                                      ^ String.capitalize_ascii txt)
                                      [ Typ.var "setValueAs" ])
                                   (Typ.constr
                                      (mknoloc
                                         (Longident.Ldot
                                            (Lident "JsxDOM", "domProps")))
                                      []));
                            ]);
                       (* handleSubmit: (inputs => unit) => JsxEvent.Form.t => unit, *)
                       Type.field ~mut:Immutable (mknoloc "handleSubmit")
                         (uncurried_core_type_arrow ~arity:1
                            [
                              Typ.arrow Nolabel
                                (uncurried_core_type_arrow ~arity:1
                                   [
                                     Typ.arrow Nolabel
                                       (Typ.constr (lid txt) [])
                                       (Typ.constr (lid "unit") []);
                                   ])
                                (uncurried_core_type_arrow ~arity:1
                                   [
                                     Typ.arrow Nolabel
                                       (Typ.constr
                                          (mknoloc
                                             (Longident.Ldot
                                                ( Ldot
                                                    (Lident "JsxEvent", "Form"),
                                                  "t" )))
                                          [])
                                       (Typ.constr (lid "unit") []);
                                   ]);
                            ]);
                       (* watch: variantOfInputs => watchReturnOfInputs, *)
                       Type.field ~mut:Immutable (mknoloc "watch")
                         (uncurried_core_type_arrow ~arity:1
                            [
                              Typ.arrow Nolabel
                                (Typ.constr
                                   (lid @@ "variantOf"
                                   ^ String.capitalize_ascii txt)
                                   [])
                                (Typ.constr
                                   (lid @@ "watchReturnOf"
                                   ^ String.capitalize_ascii txt)
                                   []);
                            ]);
                       (* formState: formStateOfInputs, *)
                       Type.field ~mut:Immutable (mknoloc "formState")
                         (Typ.constr
                            (lid @@ "formStateOf" ^ String.capitalize_ascii txt)
                            []);
                     ]);
              (* type variantOfinputs = | @as("example") Example | @as("exampleRequired") ExampleRequired *)
              Type.mk
                (mkloc ("variantOf" ^ String.capitalize_ascii txt) ptype_loc)
                ~priv:Public
                ~kind:(Ptype_variant (make_const_decls fields ptype_loc));
              (* type registerOptionsOfInputs<'setValueAs> = {required?: bool, setValueAs: 'setValueAs} *)
              Type.mk
                (mkloc
                   ("registerOptionsOf" ^ String.capitalize_ascii txt)
                   ptype_loc)
                ~params:[ (Typ.var "setValueAs", (NoVariance, NoInjectivity)) ]
                ~priv:Public
                ~kind:
                  (Ptype_record
                     [
                       Type.field
                         ~attrs:[ Attr.mk (mknoloc "res.optional") (PStr []) ]
                         ~mut:Immutable (mknoloc "required")
                         (Typ.constr (lid "bool") []);
                       Type.field
                         ~attrs:[ Attr.mk (mknoloc "res.optional") (PStr []) ]
                         ~mut:Immutable (mknoloc "setValueAs")
                         (Typ.var "setValueAs");
                     ]);
              (* @unboxed type watchReturnOfInputs = String(string) | Number(float) *)
              Type.mk
                (mkloc
                   ("watchReturnOf" ^ String.capitalize_ascii txt)
                   ptype_loc)
                ~attrs:[ Attr.mk (mknoloc "unboxed") (PStr []) ]
                ~priv:Public
                ~kind:
                  (Ptype_variant
                     [
                       Type.constructor (mknoloc "String")
                         ~args:(Pcstr_tuple [ Typ.constr (lid "string") [] ]);
                       Type.constructor (mknoloc "Number")
                         ~args:(Pcstr_tuple [ Typ.constr (lid "float") [] ]);
                     ]);
              (* type formStateOfInputs = {errors: fieldErrorsOfInputs} *)
              Type.mk
                (mkloc ("formStateOf" ^ String.capitalize_ascii txt) ptype_loc)
                ~priv:Public
                ~kind:
                  (Ptype_record
                     [
                       Type.field ~mut:Immutable (mknoloc "errors")
                         (Typ.constr
                            (lid @@ "fieldErrorsOf"
                            ^ String.capitalize_ascii txt)
                            []);
                     ]);
              (* type fieldErrorsOfInputs = { example: fieldErrorOfInputs, exampleRequired: fieldErrorOfInputs } *)
              Type.mk
                (mkloc
                   ("fieldErrorsOf" ^ String.capitalize_ascii txt)
                   ptype_loc)
                ~priv:Public
                ~kind:
                  (Ptype_record
                     (lds
                     |> List.map (fun (ld : label_declaration) ->
                            {
                              ld with
                              pld_type =
                                Typ.constr
                                  (lid @@ "fieldErrorOf"
                                  ^ String.capitalize_ascii txt)
                                  [];
                              pld_attributes =
                                add_optional_attribute ld.pld_attributes;
                            })));
              (* type fieldErrorOfInputs = {message?: string} *)
              Type.mk
                (mkloc ("fieldErrorOf" ^ String.capitalize_ascii txt) ptype_loc)
                ~priv:Public
                ~kind:
                  (Ptype_record
                     [
                       Type.field
                         ~attrs:[ Attr.mk (mknoloc "res.optional") (PStr []) ]
                         ~mut:Immutable (mknoloc "message")
                         (Typ.constr (lid "string") []);
                     ]);
              (* type useFormParamsOfInputs<'resolver> = {
                   resolver?: 'resolver,
                   defaultValues?: inputs,
                 } *)
              Type.mk
                (mkloc
                   ("useFormParamsOf" ^ String.capitalize_ascii txt)
                   ptype_loc)
                ~params:[ (Typ.var "resolver", (NoVariance, NoInjectivity)) ]
                ~priv:Public
                ~kind:
                  (Ptype_record
                     [
                       Type.field
                         ~attrs:[ Attr.mk (mknoloc "res.optional") (PStr []) ]
                         ~mut:Immutable (mknoloc "resolver")
                         (Typ.var "resolver");
                       Type.field
                         ~attrs:[ Attr.mk (mknoloc "res.optional") (PStr []) ]
                         ~mut:Immutable (mknoloc "defaultValues")
                         (Typ.constr (lid txt) []);
                     ]);
            ]
        in
        (* @module("react-hook-form")
           external useFormOfInputs: (~options: useFormParamsOfInputs<'resolver>=?) => useFormReturnOfInputs = "useForm" *)
        let primitive_use_form =
          Str.primitive
            (Val.mk
               ~attrs:
                 [
                   Attr.mk (mknoloc "module")
                     (PStr
                        [
                          Str.eval
                          @@ Exp.constant (Const.string "react-hook-form");
                        ]);
                 ]
               ~prim:[ "useForm" ]
               (mknoloc @@ "useFormOf" ^ String.capitalize_ascii txt)
               (uncurried_core_type_arrow ~arity:1
                  [
                    Typ.arrow (Optional "options")
                      (Typ.constr
                         ~attrs:
                           [ Attr.mk (mknoloc "res.namedArgLoc") (PStr []) ]
                         (lid @@ "useFormParamsOf" ^ String.capitalize_ascii txt)
                         [ Typ.var "resolver" ])
                      (Typ.constr
                         (lid @@ "useFormReturnOf" ^ String.capitalize_ascii txt)
                         [ Typ.var "setValueAs" ]);
                  ]))
        in
        [ type_decls; primitive_use_form ]
    | _ -> fail ptype_loc "This type is not handled by @ppx_ts.keyOf"
  else []

let map_structure_item mapper ({ pstr_desc } as structure_item) =
  match pstr_desc with
  | Pstr_type (_, decls) ->
      let structure_items = decls |> List.map map_type_decl |> List.concat in
      mapper#structure_item structure_item :: structure_items
  | _ -> [ mapper#structure_item structure_item ]
