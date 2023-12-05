@rhf
type item = {
  name: string,
  price: float,
  quantity: float,
}

@rhf
type inputs = {cart: array<item>}

// // FIXME
// type inputsWithId = {id: string, cart: array<item>}
// type variantOfInputs = | @as("cart") Cart | @as("sample") Sample
// type fieldErrorOfInputs = {message?: string}
// // FIXME
// type fieldErrorsOfInputs = {cart?: array<fieldErrorsOfItem>}

// @unboxed
// type watchReturnOfInputs = String(string) | Number(float)

// type formStateOfInputs = {errors: fieldErrorsOfInputs}
// // FIXME
// type registerOptionsOfInputs<'setValueAs> = {required?: bool, setValueAs?: 'setValueAs}
// type controlOfInputs

// type rec useFormReturnOfInputs<'setValueAs> = {
//   control: controlOfInputs,
//   register: (variantOfInputs, ~options: registerOptionsOfInputs<'setValueAs>=?) => JsxDOM.domProps,
//   handleSubmit: (inputs => unit) => JsxEvent.Form.t => unit,
//   watch: variantOfInputs => watchReturnOfInputs,
//   formState: formStateOfInputs,
// }

// type useFormParamsOfInputs<'resolver> = {
//   resolver?: 'resolver,
//   defaultValues?: inputs,
//   // FIXME
//   mode?: [#onBlur | #onChange | #onSubmit | #onTouched | #all],
// }
// @module("react-hook-form")
// external useFormOfInputs: (
//   ~options: useFormParamsOfInputs<'resolver>=?,
// ) => useFormReturnOfInputs<'setValueAs> = "useForm"

// module ControllerOfInputs = {
//   type controllerRulesOfInputs = {required?: bool}
//   type controllerFieldsOfInputs = {field: JsxDOM.domProps}

//   @module("react-hook-form") @react.component
//   external make: (
//     ~name: variantOfInputs=?,
//     ~control: controlOfInputs=?,
//     ~rules: controllerRulesOfInputs,
//     ~render: controllerFieldsOfInputs => React.element=?,
//   ) => React.element = "Controller"
// }

// // FIXME
// type rec useFieldArrayReturnOfInputsCart = {
//   fields: array<itemWithId>,
//   append: item => unit,
//   remove: int => unit,
// }
// and useFieldArrayParamsOfInputsCart = {
//   name: variantOfInputs,
//   control: controlOfInputs,
// }

// FIXME
// @module("react-hook-form")
// external useFieldArrayOfInputsCart: useFieldArrayParamsOfInputsCart => useFieldArrayReturnOfInputsCart =
//   "useFieldArray"

// FIXME
// let fieldArrayCart = ((
//   variantOfInputs: variantOfInputs,
//   index: int,
//   variantOfItem: variantOfItem,
// )) => {
//   `${(variantOfInputs :> string)}.${index->Belt.Int.toString}.${(variantOfItem :> string)}`->Obj.magic
// }

@react.component @genType
let default = () => {
  let {register, control, handleSubmit, formState: {errors}} = useFormOfInputs(
    ~options={
      defaultValues: {
        cart: [{name: "text", quantity: 1., price: 23.}],
      },
      mode: #onBlur,
    },
  )
  let {fields, append, remove} = useFieldArrayOfInputsCart({name: Cart, control})

  let onSubmit = (data: inputs) => Js.log(data)

  <form onSubmit={handleSubmit(onSubmit)}>
    {fields
    ->Belt.Array.mapWithIndex((index, field) => {
      <div key={field.id}>
        <section key={field.id}>
          <input
            {...register((Cart, index, Name)->fieldArrayOfCart, ~options={required: true})}
            placeholder="name"
          />
          {switch errors.cart->ReactHookForm.getFieldError(index) {
          | Some({name: ?Some(_)}) => "error"->React.string
          | _ => React.null
          }}
          <input {...register((Cart, index, Quantity)->fieldArrayOfCart)} placeholder="quantity" />
          {switch errors.cart->ReactHookForm.getFieldError(index) {
          | Some({quantity: ?Some({message})}) => message->React.string
          | _ => React.null
          }}
          <input {...register((Cart, index, Price)->fieldArrayOfCart)} placeholder="price" />
          {switch errors.cart->ReactHookForm.getFieldError(index) {
          | Some({price: ?Some({message})}) => message->React.string
          | _ => React.null
          }}
        </section>
        <button type_="button" onClick={_ => remove(index)}> {"DELETE"->React.string} </button>
      </div>
    })
    ->React.array}
    <button type_="button" onClick={_ => append({name: "ppx", quantity: 1., price: 1.})}>
      {"APPEND"->React.string}
    </button>
    <input type_="submit" />
  </form>
}
