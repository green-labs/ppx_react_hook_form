@rhf
type inputs = {
  example?: string,
  exampleRequired: string,
}

@react.component
let default = () => {
  let {register, handleSubmit, watch, formState, getFieldState, setValue} = useFormOfInputs()
  let onSubmit = (data: inputs) => Js.log(data)

  Js.log(watch(Example))

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

let _ = rhfValue(0)
