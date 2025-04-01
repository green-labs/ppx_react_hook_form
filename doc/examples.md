## Basic

```res
@rhf
type inputs = {
  name: string,
  age: float,
  email: string,
}

@react.component
let make = () => {
  let {register, handleSubmit, watch, formState: {errors}, getValues} = useFormOfInputs()
  let onSubmit = (data: inputs) => Js.log(data)

  Js.log(watch(Name))

  Js.log(getValues(Name))

  <form onSubmit={handleSubmit(onSubmit)}>
    <input {...register(Name, ~options={required: true})} placeholder="Name" />
    {switch errors.name {
    | Some(_) => <span> {"Name is required"->React.string} </span>
    | None => React.null
    }}
    <input
      {...register(Age, ~options={required: true, min: 18.})} type_="number" placeholder="Age"
    />
    {switch errors.age {
    | Some(_) => <span> {"Age must be at least 18"->React.string} </span>
    | None => React.null
    }}
    <input
      {...register(Email, ~options={required: true, pattern: /^\S+@\S+$/i})} placeholder="Email"
    />
    {switch errors.email {
    | Some(_) => <span> {"Please enter a valid email"->React.string} </span>
    | None => React.null
    }}
    <input type_="submit" />
  </form>
}
```

## Schema validation

* You can use it with any schema validation library binding you like.

```res
@rhf
type schema = {
  firstName: Zod.string_,
  age: Zod.optional<Zod.number>,
}

let schema = Zod.z->Zod.object(
  (
    {
      firstName: Zod.z->Zod.string({}),
      age: Zod.z->Zod.number({required_error: "Expected number, provided string"})->Zod.optional,
    }: schema
  ),
)

@react.component
let make = () => {
  let {register, handleSubmit, watch, formState: {errors}} = useFormOfSchema(
    ~options={
      resolver: Resolver.zodResolver(schema),
      defaultValues: {
        firstName: "ppx",
      },
    },
  )
  let onSubmit = (data: schema) => Js.log(data)

  Js.log(watch(FirstName))

  Js.log(getValues(FirstName))

  <form onSubmit={handleSubmit(onSubmit)}>
    <input {...register(FirstName)} />
    <p>
      {switch errors.firstName {
      | Some({message: ?Some(message)}) => message
      | _ => ""
      }->React.string}
    </p>
    <input
      {...register(
        Age,
        ~options={
          setValueAs: %raw(`v => v === "" ? undefined : parseInt(v, 10)`),
        },
      )}
    />
    <p>
      {switch errors.age {
      | Some({message: ?Some(message)}) => message
      | _ => ""
      }->React.string}
    </p>
    <input type_="submit" />
  </form>
}
```
#### Example binding of the schema validation library Zod

```res
// Zod.res
type t

@module("zod")
external z: t = "default"

type string_ = string
type number = float
type object<'form> = 'form
type optional<'value> = option<'value>
type params = {required_error?: string}

@send
external string: (t, params) => string_ = "string"

@send
external number: (t, params) => number = "number"

@send
external object: (t, 'schema) => object<'schema> = "object"

@send
external array: (t, 'schema) => array<'schema> = "array"

@send
external optional: 'z => optional<'z> = "optional"

// Resolver.res
type t

@module("@hookform/resolvers/zod")
external zodResolver: 'schema => t = "zodResolver"
```

## Controller

```res
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

  Js.log(getValues(MyCheckBox))

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
```

## useFieldArray

```res
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
```

### useFormContext

```res
@rhf
type inputs = {
  firstName: string,
  lastName: string,
  email: string,
  age: float,
}

module PersonalInfo = {
  @react.component
  let make = () => {
    let {register, formState: {errors}} = useFormContextOfInputs()

    <div>
      <h3> {"Personal Information"->React.string} </h3>
      <div>
        <label htmlFor="firstName"> {"First Name"->React.string} </label>
        <input
          {...register(
            FirstName,
            ~options={
              required: true,
            },
          )}
          id="firstName"
        />
        {switch errors.firstName {
        | Some(_) => <p> {"Please enter your first name"->React.string} </p>
        | _ => React.null
        }}
      </div>
      <div>
        <label htmlFor="lastName"> {"Last Name"->React.string} </label>
        <input
          {...register(
            LastName,
            ~options={
              required: true,
            },
          )}
          id="lastName"
        />
        {switch errors.lastName {
        | Some(_) => <p> {"Please enter your last name"->React.string} </p>
        | _ => React.null
        }}
      </div>
      <div>
        <label htmlFor="age"> {"Age"->React.string} </label>
        <input
          {...register(
            Age,
            ~options={
              required: true,
              min: 0.,
            },
          )}
          id="age"
          type_="number"
        />
        {switch errors.age {
        | Some(_) => <p> {"Please enter your age, it must be greater than 0"->React.string} </p>
        | _ => React.null
        }}
      </div>
    </div>
  }
}

module ContactInfo = {
  @react.component
  let make = () => {
    let {register, formState: {errors}} = useFormContextOfInputs()

    <div>
      <h3> {"Contact Information"->React.string} </h3>
      <div>
        <label htmlFor="email"> {"Email"->React.string} </label>
        <input
          {...register(
            Email,
            ~options={
              required: true,
              pattern: /^\S+@\S+$/i,
            },
          )}
          id="email"
        />
        {switch errors.email {
        | Some(_) => <p> {"Please enter your email"->React.string} </p>
        | _ => React.null
        }}
      </div>
    </div>
  }
}

@react.component
let make = () => {
  let methods = useFormOfInputs(
    ~options={
      defaultValues: {
        firstName: "",
        lastName: "",
        email: "",
        age: 0.,
      },
    },
  )

  let onSubmit = (data: inputs) => Js.log(data)

  <FormProviderOfInputs {...rhfSpread(methods)}>
    <form onSubmit={methods.handleSubmit(onSubmit)}>
      <h2> {"Profile Information"->React.string} </h2>
      <PersonalInfo />
      <ContactInfo />
      <button type_="submit"> {"Submit"->React.string} </button>
    </form>
  </FormProviderOfInputs>
}
```
