@rhf
type inputs = {
  example?: string,
  exampleRequired: string,
}

@react.component
let default = () => {
  let {register, handleSubmit, watch, formState, getFieldState, setValue} = useFormOfInputs()
  let onSubmit = (data: inputs) => Js.log(data)

  switch watch(Example) {
  | Some(Null) => Js.log("null")
  | Some(Bool(v)) => Js.log(v)
  | Some(Number(v)) => Js.log(v)
  | Some(String(v)) => Js.log(v)
  | Some(Object(v)) => Js.log(v)
  | Some(Array(v)) => Js.log(v)
  | None => Js.log("undefined")
  }

  let exampleFieldState = getFieldState(Example, formState)
  Js.log(exampleFieldState)
  Js.log(setValue)

  <form onSubmit={handleSubmit(onSubmit)}>
    <input {...register(Example)} defaultValue="test" />
    <input {...register(ExampleRequired, ~options={required: true})} />
    {formState.errors.exampleRequired->Belt.Option.isSome
      ? <span> {"This field is required"->React.string} </span>
      : React.null}
    <input type_="submit" />
  </form>
}
