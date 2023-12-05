@rhf
type inputs = {
  example?: string,
  exampleRequired: string,
}

// type variantOfInputs = | @as("example") Example | @as("exampleRequired") ExampleRequired
// type fieldErrorsOfInputs = {
//   example: bool,
//   exampleRequired: bool,
// }

// @unboxed
// type watchReturnOfInputs = String(string) | Number(float)

// type rec useFormReturnOfInputs = {
//   register: (variantOfInputs, ~options: registerOptionsOfInputs=?) => JsxDOM.domProps,
//   handleSubmit: (inputs => unit) => JsxEvent.Form.t => unit,
//   watch: variantOfInputs => watchReturnOfInputs,
//   formState: formStateOfInputs,
// }

// type formStateOfInputs = {errors: fieldErrorsOfInputs}
// type registerOptionsOfInputs = {required?: bool}

// @module("react-hook-form")
// external useFormOfInputs: unit => useFormReturnOfInputs = "useForm"

@react.component @genType
let default = () => {
  let {register, handleSubmit, watch, formState: {errors}} = useFormOfInputs()
  let onSubmit = (data: inputs) => Js.log(data)

  Js.log(watch(Example))

  <form onSubmit={handleSubmit(onSubmit)}>
    <input {...register(Example)} defaultValue="test" />
    <input {...register(ExampleRequired, ~options={required: true})} />
    {errors.exampleRequired->Belt.Option.isSome
      ? <span> {"This field is required"->React.string} </span>
      : React.null}
    <input type_="submit" />
  </form>
}
