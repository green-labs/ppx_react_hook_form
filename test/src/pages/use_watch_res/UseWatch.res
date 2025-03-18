@rhf
type inputs = {
  example?: string,
  exampleRequired: string,
}

@react.component @genType
let default = () => {
  let {register, control, handleSubmit, formState} = useFormOfInputs()
  let onSubmit = (data: inputs) => Js.log(data)

  let example = useWatchOfInputs({name: Example, control, defaultValue: String("test")})

  Js.log(example)

  <form onSubmit={handleSubmit(onSubmit)}>
    <input {...register(Example)} defaultValue="test" />
    <input {...register(ExampleRequired, ~options={required: true})} />
    {formState.errors.exampleRequired->Belt.Option.isSome
      ? <span> {"This field is required"->React.string} </span>
      : React.null}
    <input type_="submit" />
  </form>
}
