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

@react.component @genType
let default = () => {
  let {register, handleSubmit, watch, getValues, formState: {errors}} = useFormOfSchema(
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
