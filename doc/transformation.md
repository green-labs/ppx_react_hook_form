## Transformation

```res
// source
@rhf
type item = {
  name: string,
  quantity: float,
  price: float,
}

@rhf
type inputs = {
  example?: string,
  exampleRequired: string,
  cart: array<item>,
}

// generated
type inputsWithId = {
  id: string,
  example?: string,
  exampleRequired?: string,
  cart?: array<item>
}
type defaultValuesOfInputs = {
  example?: string,
  exampleRequired?: string,
  cart?: array<defaultValuesOfItem>
}
type rec fieldStateOfInputs = {invalid: bool, isDirty: bool, isTouched: bool, error: fieldErrorOfInputs}
and fieldErrorOfInputs = {message?: string}
@unboxed
type rec valuesOfInputs =
  | @as(null) Null
  | Bool(bool)
  | Number(float)
  | String(string)
  | Object(Js.Dict.t<valuesOfInputs>)
  | Array(array<valuesOfInputs>)

type fieldDirtyOfInputs = {
  example?: bool,
  exampleRequired?: bool,
  cart?: array<fieldDirtyOfItem>
}

type rec useFormReturnOfInputs<'setValueAs> = {
  control: controlOfInputs,
  register: (variantOfInputs, ~options: registerOptionsOfInputs<'setValueAs>=?) => JsxDOM.domProps,
  handleSubmit: (inputs => unit) => JsxEvent.Form.t => unit,
  watch: variantOfInputs => option<valuesOfInputs>,
  getValues: variantOfInputs => option<valuesOfInputs>
  formState: formStateOfInputs,
  getFieldState: (variantOfInputs, formStateOfInputs) => fieldStateOfInputs,
  setValue: (variantOfInputs, ReactHookForm.value, ~options: setValueConfigOfInputs=?) => unit,
  reset: (~options:defaultValues=?) => unit
} 
and controlOfInputs
and variantOfInputs = | @as("example") Example | @as("exampleRequired") ExampleRequired | @as("cart") Cart
and registerOptionsOfInputs<'setValueAs> = {required?: bool, setValueAs?: 'setValueAs, valueAsNumber?: bool}
and formStateOfInputs = {
  isDirty: bool,
  isValid: bool,
  errors: fieldErrorsOfInputs,
  dirtyFields: fieldDirtyOfInputs
}
and fieldErrorsOfInputs = {
  example?: fieldErrorOfInputs,
  exampleRequired?: fieldErrorOfInputs,
  cart?: array<fieldErrorsOfItem>
}
and useFormParamsOfInputs<'resolver> = {
  resolver?: 'resolver,
  defaultValues?: defaultValuesOfInput,
  mode?: [#onBlur | #onChange | #onSubmit | #onTouched | #all],
}
and setValueConfigOfInputs = {
  shouldValidate: bool,
  shouldDirty: bool,
  shouldTouch: bool,
}

@module("react-hook-form")
external useFormOfInputs: (
  ~options: useFormParamsOfInputs<'resolver>=?
) => useFormReturnOfInputs<'setValueAs> = "useForm"

module ControllerOfInputs = {
  type controllerRulesOfInputs = {required?: bool}
  type controllerFieldsOfInputs = {field: JsxDOM.domProps}

  @module("react-hook-form") @react.component
  external make: (
    ~name: variantOfInputs,
    ~control: controlOfInputs=?,
    ~rules: controllerRulesOfInputs=?,
    ~render: controllerFieldsOfInputs => React.element=?,
  ) => React.element = "Controller"
}

//useWatch
type useWatchParamsOfInputs = {
  name: variantOfInputs,
  control: controlOfInputs=?,
  defaultValue: valueOfInputs=?,
  disabled: bool=?,
  exact: bool=?,
}
@module("react-hook-form")
external useWatchOfInputs: useWatchParamsOfInputs => option<valueOfInputs>

// useFieldArray
type rec useFieldArrayReturnOfInputsCart = {
  fields: array<itemWithId>,
  append: defaultValuesOfItem => unit,
  remove: int => unit,
}
and useFieldArrayParamsOfInputsCart = {
  name: variantOfInputs,
  control: controlOfInputs,
}

@module("react-hook-form")
external useFieldArrayOfInputsCart: useFieldArrayParamsOfInputsCart => useFieldArrayReturnOfInputsCart =
  "useFieldArray"
```
