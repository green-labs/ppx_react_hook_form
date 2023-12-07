open Ppxlib
open Parsetree
open Ast_helper
open Utils

let map_type_decl
    {
      ptype_attributes;
      ptype_name = { txt = record_name };
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
              (* type fieldStateOfInputs = {invalid: bool, isDirty: bool, isTouched: bool, error: fieldErrorOfInputs} *)
              Type.mk
                (mkloc
                   ("fieldStateOf" ^ String.capitalize_ascii record_name)
                   ptype_loc)
                ~priv:Public
                ~kind:
                  (Ptype_record
                     [
                       Type.field ~mut:Immutable (mknoloc "invalid")
                         (Typ.constr (lid "bool") []);
                       Type.field ~mut:Immutable (mknoloc "isDirty")
                         (Typ.constr (lid "bool") []);
                       Type.field ~mut:Immutable (mknoloc "isTouched")
                         (Typ.constr (lid "bool") []);
                       Type.field ~mut:Immutable (mknoloc "error")
                         (Typ.constr
                            (lid @@ "fieldErrorOf"
                            ^ String.capitalize_ascii record_name)
                            []);
                     ]);
              (* type fieldErrorOfInputs = {message?: string} *)
              Type.mk
                (mkloc
                   ("fieldErrorOf" ^ String.capitalize_ascii record_name)
                   ptype_loc)
                ~priv:Public
                ~kind:
                  (Ptype_record
                     [
                       Type.field
                         ~attrs:[ Attr.mk (mknoloc "res.optional") (PStr []) ]
                         ~mut:Immutable (mknoloc "message")
                         (Typ.constr (lid "string") []);
                     ]);
              (* @unboxed type watchReturnOfInputs = String(string) | Number(float) *)
              Type.mk
                (mkloc
                   ("watchReturnOf" ^ String.capitalize_ascii record_name)
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
            ]
        in
        let type_decls1 =
          Str.type_ Recursive
            [
              (* type useFormReturnOfInputs<'setValueAs> = {
                  control: controlOfInputs,
                  register: (variantOfInputs, ~options: registerOptionsOfInputs<'setValueAs>=?) => JsxDOM.domProps,
                  handleSubmit: (inputs => unit) => JsxEvent.Form.t => unit,
                  watch: variantOfInputs => watchReturnOfInputs,
                  formState: formStateOfInputs,
                    } *)
              Type.mk
                (mkloc
                   ("useFormReturnOf" ^ String.capitalize_ascii record_name)
                   ptype_loc)
                ~params:[ (Typ.var "setValueAs", (NoVariance, NoInjectivity)) ]
                ~priv:Public
                ~kind:
                  (Ptype_record
                     [
                       (* control: controlOfInputs *)
                       Type.field ~mut:Immutable (mknoloc "control")
                         (Typ.constr
                            (lid @@ "controlOf"
                            ^ String.capitalize_ascii record_name)
                            []);
                       (* register: (variantOfInputs, ~options: registerOptionsOfInputs=?) => JsxDOM.domProps, *)
                       Type.field ~mut:Immutable (mknoloc "register")
                         (uncurried_core_type_arrow ~arity:2
                            [
                              Typ.arrow Nolabel
                                (Typ.constr
                                   (lid @@ "variantOf"
                                   ^ String.capitalize_ascii record_name)
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
                                      ^ String.capitalize_ascii record_name)
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
                                       (Typ.constr (lid record_name) [])
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
                                   ^ String.capitalize_ascii record_name)
                                   [])
                                (Typ.constr
                                   (lid @@ "watchReturnOf"
                                   ^ String.capitalize_ascii record_name)
                                   []);
                            ]);
                       (* formState: formStateOfInputs, *)
                       Type.field ~mut:Immutable (mknoloc "formState")
                         (Typ.constr
                            (lid @@ "formStateOf"
                            ^ String.capitalize_ascii record_name)
                            []);
                       (* getFieldState: (variantOfInputs, formStateOfInputs) => fieldStateOfInputs, *)
                       Type.field ~mut:Immutable (mknoloc "getFieldState")
                         (uncurried_core_type_arrow ~arity:2
                            [
                              Typ.arrow Nolabel
                                (Typ.constr
                                   (lid @@ "variantOf"
                                   ^ String.capitalize_ascii record_name)
                                   [])
                                (Typ.arrow Nolabel
                                   (Typ.constr
                                      (lid @@ "formStateOf"
                                      ^ String.capitalize_ascii record_name)
                                      [])
                                   (Typ.constr
                                      (lid @@ "fieldStateOf"
                                      ^ String.capitalize_ascii record_name)
                                      []));
                            ]);
                       (* setValue: (variantOfInputs, ReactHookForm.value) => unit, *)
                       Type.field ~mut:Immutable (mknoloc "setValue")
                         (uncurried_core_type_arrow ~arity:2
                            [
                              Typ.arrow Nolabel
                                (Typ.constr
                                   (lid @@ "variantOf"
                                   ^ String.capitalize_ascii record_name)
                                   [])
                                (Typ.arrow Nolabel
                                   (Typ.constr
                                      (mknoloc
                                         (Longident.Ldot
                                            (Lident "ReactHookForm", "value")))
                                      [])
                                   (Typ.constr (lid "unit") []));
                            ]);
                     ]);
              (* type controlOfInputs *)
              Type.mk
                (mkloc
                   ("controlOf" ^ String.capitalize_ascii record_name)
                   ptype_loc)
                ~priv:Public ~kind:Ptype_abstract;
              (* type variantOfinputs = | @as("example") Example | @as("exampleRequired") ExampleRequired *)
              Type.mk
                (mkloc
                   ("variantOf" ^ String.capitalize_ascii record_name)
                   ptype_loc)
                ~priv:Public
                ~kind:(Ptype_variant (make_const_decls fields ptype_loc));
              (* type registerOptionsOfInputs<'setValueAs> = {required?: bool, setValueAs?: 'setValueAs} *)
              Type.mk
                (mkloc
                   ("registerOptionsOf" ^ String.capitalize_ascii record_name)
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
              (* type formStateOfInputs = {isDirty: bool, isValid: bool, errors: fieldErrorsOfInputs} *)
              Type.mk
                (mkloc
                   ("formStateOf" ^ String.capitalize_ascii record_name)
                   ptype_loc)
                ~priv:Public
                ~kind:
                  (Ptype_record
                     [
                       Type.field ~mut:Immutable (mknoloc "isDirty")
                         (Typ.constr (lid "bool") []);
                       Type.field ~mut:Immutable (mknoloc "isValid")
                         (Typ.constr (lid "bool") []);
                       Type.field ~mut:Immutable (mknoloc "errors")
                         (Typ.constr
                            (lid @@ "fieldErrorsOf"
                            ^ String.capitalize_ascii record_name)
                            []);
                     ]);
              (* type fieldErrorsOfInputs = { example: fieldErrorOfInputs, exampleRequired: fieldErrorOfInputs } *)
              Type.mk
                (mkloc
                   ("fieldErrorsOf" ^ String.capitalize_ascii record_name)
                   ptype_loc)
                ~priv:Public
                ~kind:
                  (Ptype_record
                     (lds
                     |> List.map
                          (fun (({ pld_type } : label_declaration) as ld) ->
                            match pld_type with
                            (* type fieldErrorsOfInputs = {cart?: array<fieldErrorsOfItem>} *)
                            | {
                             ptyp_desc =
                               Ptyp_constr
                                 ( { txt = Lident "array" },
                                   [
                                     {
                                       ptyp_desc =
                                         Ptyp_constr ({ txt = Lident l }, []);
                                     };
                                   ] );
                            } ->
                                {
                                  ld with
                                  pld_type =
                                    Typ.constr (lid "array")
                                      [
                                        Typ.constr
                                          (lid @@ "fieldErrorsOf"
                                         ^ String.capitalize_ascii l)
                                          [];
                                      ];
                                  pld_attributes =
                                    add_optional_attribute ld.pld_attributes;
                                }
                            | _ ->
                                {
                                  ld with
                                  pld_type =
                                    Typ.constr
                                      (lid @@ "fieldErrorOf"
                                      ^ String.capitalize_ascii record_name)
                                      [];
                                  pld_attributes =
                                    add_optional_attribute ld.pld_attributes;
                                })));
              (* type useFormParamsOfInputs<'resolver> = {
                   resolver?: 'resolver,
                   defaultValues?: inputs,
                   mode?: [#onBlur | #onChange | #onSubmit | #onTouched | #all],
                 } *)
              Type.mk
                (mkloc
                   ("useFormParamsOf" ^ String.capitalize_ascii record_name)
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
                         (Typ.constr (lid record_name) []);
                       Type.field
                         ~attrs:[ Attr.mk (mknoloc "res.optional") (PStr []) ]
                         ~mut:Immutable (mknoloc "mode")
                         (Typ.variant
                            [
                              Rf.tag (mknoloc "onBlur") true [];
                              Rf.tag (mknoloc "onChange") true [];
                              Rf.tag (mknoloc "onSubmit") true [];
                              Rf.tag (mknoloc "onTouched") true [];
                              Rf.tag (mknoloc "all") true [];
                            ]
                            Closed None);
                     ]);
            ]
        in
        let type_decls2 =
          Str.type_ Nonrecursive
            [
              (* type inputsWithId = {id: string, ...} *)
              Type.mk
                (mkloc (record_name ^ "WithId") ptype_loc)
                ~priv:Public
                ~kind:
                  (Ptype_record
                     (Type.field ~mut:Immutable (mknoloc "id")
                        (Typ.constr (lid "string") [])
                     :: lds));
            ]
        in

        (* @module("react-hook-form")
           external useFormOfInputs: (~options: useFormParamsOfInputs<'resolver>=?) => useFormReturnOfInputs<'setValueAs> = "useForm" *)
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
               (mknoloc @@ "useFormOf" ^ String.capitalize_ascii record_name)
               (uncurried_core_type_arrow ~arity:1
                  [
                    Typ.arrow (Optional "options")
                      (Typ.constr
                         ~attrs:
                           [ Attr.mk (mknoloc "res.namedArgLoc") (PStr []) ]
                         (lid @@ "useFormParamsOf"
                         ^ String.capitalize_ascii record_name)
                         [ Typ.var "resolver" ])
                      (Typ.constr
                         (lid @@ "useFormReturnOf"
                         ^ String.capitalize_ascii record_name)
                         [ Typ.var "setValueAs" ]);
                  ]))
        in
        (* module ControllerOfInputs = {
             type controllerRulesOfInputs = {required?: bool}
             type controllerFieldsOfInputs = {field: JsxDOM.domProps}

             @module("react-hook-form") @react.component
             external make: (
               ~name: variantOfInputs=?,
               ~control: controlOfInputs=?,
               ~rules: controllerRulesOfInputs,
               ~render: controllerFieldsOfInputs => React.element=?,
             ) => React.element = "Controller"
           } *)
        let module_controller =
          Str.module_
            (Mb.mk
               (mknoloc
               @@ Some ("ControllerOf" ^ String.capitalize_ascii record_name))
               (Mod.structure
                  [
                    Str.type_ Recursive
                      [
                        (* type controllerRulesOfInputs = {required?: bool} *)
                        Type.mk
                          (mknoloc
                             ("controllerRulesOf"
                             ^ String.capitalize_ascii record_name))
                          ~priv:Public
                          ~kind:
                            (Ptype_record
                               [
                                 Type.field
                                   ~attrs:
                                     [
                                       Attr.mk (mknoloc "res.optional")
                                         (PStr []);
                                     ]
                                   (mknoloc "required")
                                   (Typ.constr (lid "bool") []);
                               ]);
                        (* type controllerFieldsOfInputs = {field: JsxDOM.domProps} *)
                        Type.mk
                          (mknoloc
                             ("controllerFieldsOf"
                             ^ String.capitalize_ascii record_name))
                          ~priv:Public
                          ~kind:
                            (Ptype_record
                               [
                                 Type.field (mknoloc "field")
                                   (Typ.constr
                                      (mknoloc
                                         (Longident.Ldot
                                            (Lident "JsxDOM", "domProps")))
                                      []);
                               ]);
                      ];
                    (* @module("react-hook-form") @react.component
                       external make: (
                         ~name: variantOfInputs=?,
                         ~control: controlOfInputs=?,
                         ~rules: controllerRulesOfInputs,
                         ~render: controllerFieldsOfInputs => React.element=?,
                       ) => React.element = "Controller" *)
                    Str.primitive
                      (Val.mk
                         ~attrs:
                           [
                             Attr.mk (mknoloc "module")
                               (PStr
                                  [
                                    Str.eval
                                    @@ Exp.constant
                                         (Const.string "react-hook-form");
                                  ]);
                             Attr.mk (mknoloc "react.component") (PStr []);
                           ]
                         ~prim:[ "Controller" ] (mknoloc "make")
                         (uncurried_core_type_arrow ~arity:4
                            [
                              Typ.arrow (Labelled "name")
                                (Typ.constr
                                   ~attrs:
                                     [
                                       Attr.mk
                                         (mknoloc "res.namedArgLoc")
                                         (PStr []);
                                     ]
                                   (lid @@ "variantOf"
                                   ^ String.capitalize_ascii record_name)
                                   [])
                                (Typ.arrow (Labelled "control")
                                   (Typ.constr
                                      ~attrs:
                                        [
                                          Attr.mk
                                            (mknoloc "res.namedArgLoc")
                                            (PStr []);
                                        ]
                                      (lid @@ "controlOf"
                                      ^ String.capitalize_ascii record_name)
                                      [])
                                   (Typ.arrow (Labelled "rules")
                                      (Typ.constr
                                         ~attrs:
                                           [
                                             Attr.mk
                                               (mknoloc "res.namedArgLoc")
                                               (PStr []);
                                           ]
                                         (lid @@ "controllerRulesOf"
                                         ^ String.capitalize_ascii record_name)
                                         [])
                                      (Typ.arrow (Labelled "render")
                                         (uncurried_core_type_arrow
                                            ~attrs:
                                              [
                                                Attr.mk
                                                  (mknoloc "res.namedArgLoc")
                                                  (PStr []);
                                              ]
                                            ~arity:1
                                            [
                                              Typ.arrow Nolabel
                                                (Typ.constr
                                                   (lid @@ "controllerFieldsOf"
                                                   ^ String.capitalize_ascii
                                                       record_name)
                                                   [])
                                                (Typ.constr
                                                   (mknoloc
                                                      (Longident.Ldot
                                                         ( Lident "React",
                                                           "element" )))
                                                   []);
                                            ])
                                         (Typ.constr
                                            (mknoloc
                                               (Longident.Ldot
                                                  (Lident "React", "element")))
                                            []))));
                            ]));
                  ]))
        in

        let type_decls3 =
          lds
          |> List.filter_map
               (fun
                 ({ pld_name = { txt = field_name }; pld_type } :
                   label_declaration)
               ->
                 match pld_type with
                 (* if the field has array type, e.g. array<item> *)
                 | {
                  ptyp_desc =
                    Ptyp_constr
                      ( { txt = Lident "array" },
                        [
                          {
                            ptyp_desc =
                              Ptyp_constr ({ txt = Lident item_name }, []);
                          };
                        ] );
                 } ->
                     Some
                       (Str.type_ Recursive
                          [
                            (* type useFieldArrayReturnOfInputsCart = {
                                 fields: array<itemWithId>,
                                 append: item => unit,
                                 remove: int => unit,
                               } *)
                            Type.mk
                              (mkloc
                                 ("useFieldArrayReturnOf"
                                 ^ String.capitalize_ascii record_name
                                 ^ String.capitalize_ascii field_name)
                                 ptype_loc)
                              ~priv:Public
                              ~kind:
                                (Ptype_record
                                   [
                                     Type.field ~mut:Immutable
                                       (mknoloc "fields")
                                       (Typ.constr (lid "array")
                                          [
                                            Typ.constr
                                              (lid @@ item_name ^ "WithId")
                                              [];
                                          ]);
                                     Type.field ~mut:Immutable
                                       (mknoloc "append")
                                       (uncurried_core_type_arrow ~arity:1
                                          [
                                            Typ.arrow Nolabel
                                              (Typ.constr (lid item_name) [])
                                              (Typ.constr (lid "unit") []);
                                          ]);
                                     Type.field ~mut:Immutable
                                       (mknoloc "remove")
                                       (uncurried_core_type_arrow ~arity:1
                                          [
                                            Typ.arrow Nolabel
                                              (Typ.constr (lid "int") [])
                                              (Typ.constr (lid "unit") []);
                                          ]);
                                   ]);
                            (* type useFieldArrayParamsOfInputsCart = {
                                 name: variantOfInputs,
                                 control: controlOfInputs,
                               } *)
                            Type.mk
                              (mkloc
                                 ("useFieldArrayParamsOf"
                                 ^ String.capitalize_ascii record_name
                                 ^ String.capitalize_ascii field_name)
                                 ptype_loc)
                              ~priv:Public
                              ~kind:
                                (Ptype_record
                                   [
                                     Type.field ~mut:Immutable (mknoloc "name")
                                       (Typ.constr
                                          (lid @@ "variantOf"
                                          ^ String.capitalize_ascii record_name
                                          )
                                          []);
                                     Type.field ~mut:Immutable
                                       (mknoloc "control")
                                       (Typ.constr
                                          (lid @@ "controlOf"
                                          ^ String.capitalize_ascii record_name
                                          )
                                          []);
                                   ]);
                          ])
                 | _ -> None)
        in

        (* @module("react-hook-form")
            external useFieldArrayOfInputsCart: useFieldArrayParamsOfInputsCart => useFieldArrayReturnOfInputsCart =
              "useFieldArray" *)
        let primitive_use_field_array =
          lds
          |> List.map
               (fun
                 ({ pld_name = { txt = field_name }; pld_type } :
                   label_declaration)
               ->
                 match pld_type with
                 (* if the field has array type, e.g. array<item> *)
                 | {
                  ptyp_desc =
                    Ptyp_constr
                      ( { txt = Lident "array" },
                        [ { ptyp_desc = Ptyp_constr (_, []) } ] );
                 } ->
                     [
                       Str.primitive
                         (Val.mk
                            ~attrs:
                              [
                                Attr.mk (mknoloc "module")
                                  (PStr
                                     [
                                       Str.eval
                                       @@ Exp.constant
                                            (Const.string "react-hook-form");
                                     ]);
                              ]
                            ~prim:[ "useFieldArray" ]
                            (mknoloc @@ "useFieldArrayOf"
                            ^ String.capitalize_ascii record_name
                            ^ String.capitalize_ascii field_name)
                            (uncurried_core_type_arrow ~arity:1
                               [
                                 Typ.arrow Nolabel
                                   (Typ.constr
                                      ~attrs:
                                        [
                                          Attr.mk
                                            (mknoloc "res.namedArgLoc")
                                            (PStr []);
                                        ]
                                      (lid @@ "useFieldArrayParamsOf"
                                      ^ String.capitalize_ascii record_name
                                      ^ String.capitalize_ascii field_name)
                                      [])
                                   (Typ.constr
                                      (lid @@ "useFieldArrayReturnOf"
                                      ^ String.capitalize_ascii record_name
                                      ^ String.capitalize_ascii field_name)
                                      []);
                               ]));
                     ]
                 | _ -> [])
          |> List.concat
        in
        (* let fieldArrayCart = ((
             variantOfInputs: variantOfInputs,
             index: int,
             variantOfItem: variantOfItem,
           )) => {
             `${(variantOfInputs :> string)}.${index->Belt.Int.toString}.${(variantOfItem :> string)}`->Obj.magic
           } *)
        let vb_field_array =
          lds
          |> List.filter_map
               (fun
                 ({ pld_name = { txt = field_name }; pld_type } :
                   label_declaration)
               ->
                 match pld_type with
                 (* if the field has array type, e.g. array<item> *)
                 | {
                  ptyp_desc =
                    Ptyp_constr
                      ( { txt = Lident "array" },
                        [
                          {
                            ptyp_desc =
                              Ptyp_constr ({ txt = Lident item_name }, []);
                          };
                        ] );
                 } ->
                     Some
                       (Str.value Nonrecursive
                          [
                            Vb.mk
                              (Pat.var
                                 (mknoloc
                                    ("fieldArrayOf"
                                    ^ String.capitalize_ascii field_name)))
                              (uncurried_expr_func ~arity:1
                                 (Exp.fun_ Nolabel None
                                    (Pat.tuple
                                       [
                                         Pat.constraint_
                                           (Pat.var
                                              (mknoloc
                                                 ("variantOf"
                                                 ^ String.capitalize_ascii
                                                     record_name)))
                                           (Typ.constr
                                              (lid
                                                 ("variantOf"
                                                 ^ String.capitalize_ascii
                                                     record_name))
                                              []);
                                         Pat.constraint_
                                           (Pat.var (mknoloc "index"))
                                           (Typ.constr (lid "int") []);
                                         Pat.constraint_
                                           (Pat.var
                                              (mknoloc
                                                 ("variantOf"
                                                 ^ String.capitalize_ascii
                                                     item_name)))
                                           (Typ.constr
                                              (lid
                                                 ("variantOf"
                                                 ^ String.capitalize_ascii
                                                     item_name))
                                              []);
                                       ])
                                    (Exp.apply
                                       ~attrs:
                                         [
                                           Attr.mk (mknoloc "res.uapp")
                                             (PStr []);
                                           Attr.mk (mknoloc "res.braces")
                                             (PStr []);
                                         ]
                                       (Exp.ident
                                          (mknoloc
                                             (Longident.Ldot
                                                (Lident "Obj", "magic"))))
                                       [
                                         ( Nolabel,
                                           Exp.apply
                                             ~attrs:
                                               [
                                                 Attr.mk
                                                   (mknoloc "res.template")
                                                   (PStr []);
                                               ]
                                             (Exp.ident (lid "^"))
                                             [
                                               ( Nolabel,
                                                 Exp.apply
                                                   ~attrs:
                                                     [
                                                       Attr.mk
                                                         (mknoloc "res.template")
                                                         (PStr []);
                                                     ]
                                                   (Exp.ident (lid "^"))
                                                   [
                                                     ( Nolabel,
                                                       Exp.apply
                                                         ~attrs:
                                                           [
                                                             Attr.mk
                                                               (mknoloc
                                                                  "res.template")
                                                               (PStr []);
                                                           ]
                                                         (Exp.ident (lid "^"))
                                                         [
                                                           ( Nolabel,
                                                             Exp.apply
                                                               ~attrs:
                                                                 [
                                                                   Attr.mk
                                                                     (mknoloc
                                                                        "res.template")
                                                                     (PStr []);
                                                                 ]
                                                               (Exp.ident
                                                                  (lid "^"))
                                                               [
                                                                 ( Nolabel,
                                                                   Exp.apply
                                                                     ~attrs:
                                                                       [
                                                                         Attr.mk
                                                                           (mknoloc
                                                                              "res.template")
                                                                           (PStr
                                                                              []);
                                                                       ]
                                                                     (Exp.ident
                                                                        (lid "^"))
                                                                     [
                                                                       ( Nolabel,
                                                                         Exp
                                                                         .apply
                                                                           ~attrs:
                                                                             [
                                                                               Attr
                                                                               .mk
                                                                                (
                                                                                mknoloc
                                                                                "res.template")
                                                                                (
                                                                                PStr
                                                                                []);
                                                                             ]
                                                                           (Exp
                                                                            .ident
                                                                              (lid
                                                                                "^"))
                                                                           [
                                                                             ( Nolabel,
                                                                               Exp
                                                                               .constant
                                                                                ~attrs:
                                                                                [
                                                                                Attr
                                                                                .mk
                                                                                (
                                                                                mknoloc
                                                                                "res.template")
                                                                                (
                                                                                PStr
                                                                                []);
                                                                                ]
                                                                                (
                                                                                Const
                                                                                .string
                                                                                ~quotation_delimiter:
                                                                                "*j"
                                                                              "")
                                                                             );
                                                                             ( Nolabel,
                                                                               Exp
                                                                               .coerce
                                                                                (
                                                                                Exp
                                                                                .ident
                                                                                (
                                                                                lid
                                                                                (
                                                                                "variantOf"
                                                                                ^ 
                                                                                String
                                                                                .capitalize_ascii
                                                                                record_name
                                                                                )))
                                                                                None
                                                                                (
                                                                                Typ
                                                                                .constr
                                                                                (
                                                                                lid
                                                                                "string")
                                                                                [])
                                                                             );
                                                                           ] );
                                                                       ( Nolabel,
                                                                         Exp
                                                                         .constant
                                                                           ~attrs:
                                                                             [
                                                                               Attr
                                                                               .mk
                                                                                (
                                                                                mknoloc
                                                                                "res.template")
                                                                                (
                                                                                PStr
                                                                                []);
                                                                             ]
                                                                           (Const
                                                                            .string
                                                                              ~quotation_delimiter:
                                                                                "*j"
                                                                              ".")
                                                                       );
                                                                     ] );
                                                                 ( Nolabel,
                                                                   Exp.apply
                                                                     ~attrs:
                                                                       [
                                                                         Attr.mk
                                                                           (mknoloc
                                                                              "res.uapp")
                                                                           (PStr
                                                                              []);
                                                                       ]
                                                                     (Exp.ident
                                                                        (mknoloc
                                                                           (Longident
                                                                            .Ldot
                                                                              ( Ldot
                                                                                ( 
                                                                                Lident
                                                                                "Belt",
                                                                                "Int"
                                                                                ),
                                                                                "toString"
                                                                              ))))
                                                                     [
                                                                       ( Nolabel,
                                                                         Exp
                                                                         .ident
                                                                           (lid
                                                                              "index")
                                                                       );
                                                                     ] );
                                                               ] );
                                                           ( Nolabel,
                                                             Exp.constant
                                                               ~attrs:
                                                                 [
                                                                   Attr.mk
                                                                     (mknoloc
                                                                        "res.template")
                                                                     (PStr []);
                                                                 ]
                                                               (Const.string
                                                                  ~quotation_delimiter:
                                                                    "*j" ".") );
                                                         ] );
                                                     ( Nolabel,
                                                       Exp.coerce
                                                         (Exp.ident
                                                            (lid
                                                               ("variantOf"
                                                               ^ String
                                                                 .capitalize_ascii
                                                                   item_name)))
                                                         None
                                                         (Typ.constr
                                                            (lid "string") [])
                                                     );
                                                   ] );
                                               ( Nolabel,
                                                 Exp.constant
                                                   ~attrs:
                                                     [
                                                       Attr.mk
                                                         (mknoloc "res.template")
                                                         (PStr []);
                                                     ]
                                                   (Const.string
                                                      ~quotation_delimiter:"*j"
                                                      "") );
                                             ] );
                                       ])));
                          ])
                 | _ -> None)
        in

        [
          type_decls;
          type_decls1;
          type_decls2;
          primitive_use_form;
          module_controller;
        ]
        @ type_decls3 @ primitive_use_field_array @ vb_field_array
    | _ -> fail ptype_loc "This type is not handled by @ppx_react_hook_form"
  else []

let map_structure_item mapper ({ pstr_desc } as structure_item) =
  match pstr_desc with
  | Pstr_type (_, decls) ->
      let structure_items = decls |> List.map map_type_decl |> List.concat in
      mapper#structure_item structure_item :: structure_items
  | _ -> [ mapper#structure_item structure_item ]
