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
  | Null => Js.log("null")
  | Bool(v) => Js.log(v)
  | Number(v) => Js.log(v)
  | String(v) => Js.log(v)
  | Object(v) => Js.log(v)
  | Array(v) => Js.log(v)
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
