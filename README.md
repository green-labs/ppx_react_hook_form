# PPX react hook form

A ReScript PPX for the [React Hook Form](https://react-hook-form.com) bindings

## Example

```res
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

  Js.log(getValues(Example))

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
```

[More examples](doc/examples.md)

## Getting Started

### Install

**The ppx-rhf supports the uncurried mode only.**

```sh
pnpm add -D @greenlabs/ppx-rhf
```

```json
// rescript.json or bsconfig.json

"bs-dependencies": [
  "@greenlabs/ppx-rhf"
],
"ppx-flags": [
  ...,
  "@greenlabs/ppx-rhf/ppx"
],
```

## API

[Read more](/doc/api.md)
