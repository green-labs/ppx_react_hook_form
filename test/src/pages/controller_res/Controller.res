@rhf
type inputs = {myCheckBox: bool}

module Checkbox = {
  @module("@material-ui/core") @react.component(: JsxDOM.domProps)
  external make: unit => React.element = "Checkbox"
}

@react.component @genType
let default = () => {
  let {control, handleSubmit, watch} = useFormOfInputs()
  let onSubmit = (data: inputs) => Js.log(data)

  Js.log(watch(MyCheckBox))

  <form onSubmit={handleSubmit(onSubmit)}>
    <ControllerOfInputs
      name={MyCheckBox}
      control={control}
      rules={{required: true}}
      render={({field}) => <Checkbox {...field} />}
    />
    <input type_="submit" />
  </form>
}
