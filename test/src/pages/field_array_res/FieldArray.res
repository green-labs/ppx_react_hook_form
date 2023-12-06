@rhf
type item = {
  name: string,
  price: float,
  quantity: float,
}

@rhf
type inputs = {cart: array<item>}

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
