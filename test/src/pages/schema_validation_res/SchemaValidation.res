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

// type variantOfSchema = | @as("firstName") FirstName | @as("age") Age
// type fieldErrorOfSchema = {message?: string}
// type fieldErrorsOfSchema = {
//   firstName?: fieldErrorOfSchema,
//   age?: fieldErrorOfSchema,
// }

// @unboxed
// type watchReturnOfSchema = String(string) | Number(float)

// type formStateOfSchema = {errors: fieldErrorsOfSchema}

// type registerOptionsOfSchema<'setValueAs> = {
//   required?: bool,
//   setValueAs?: 'setValueAs,
// }

// type rec useFormReturnOfSchema<'setValueAs> = {
//   register: (variantOfSchema, ~options: registerOptionsOfSchema<'setValueAs>=?) => JsxDOM.domProps,
//   handleSubmit: (schema => unit) => JsxEvent.Form.t => unit,
//   watch: variantOfSchema => watchReturnOfSchema,
//   formState: formStateOfSchema,
// }

// type useFormParamsOfSchema<'resolver> = {
//   resolver?: 'resolver,
//   defaultValues?: schema,
// }
// @module("react-hook-form")
// external useFormOfSchema: (
//   ~options: useFormParamsOfSchema<'resolver>=?,
// ) => useFormReturnOfSchema<'setValueAs> = "useForm"

@react.component @genType
let default = () => {
  let {register, handleSubmit, watch, formState: {errors}} = useFormOfSchema(
    ~options={
      resolver: Resolver.zodResolver(schema),
      defaultValues: {
        firstName: "ppx",
        age: None,
      },
    },
  )
  let onSubmit = (data: schema) => Js.log(data)

  Js.log(watch(FirstName))

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
