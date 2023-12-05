import { zodResolver } from "@hookform/resolvers/zod"
import * as z from "zod"
import { useForm } from "react-hook-form"

const schema = z.object({
  firstName: z.string(),
  age: z.number({ required_error: "Expected number, provided string" }).optional()
})

export default function SchemaValidation() {
  const {
    register,
    handleSubmit,
    watch,
    formState: { errors },
  } = useForm<z.infer<typeof schema>>({
    resolver: zodResolver(schema),
    defaultValues: {
      firstName: "ppx",
    }
  })
  const onSubmit = (data: z.infer<typeof schema>) => console.log(data)

  console.log(watch("firstName")) // watch input value by passing the name of it

  return (
    /* "handleSubmit" will validate your inputs before invoking "onSubmit" */
    <form onSubmit={handleSubmit(onSubmit)}>
      <input {...register("firstName")} />
      <p>{errors.firstName?.message}</p>

      <input {...register("age", { setValueAs: v => v === "" ? undefined : parseInt(v, 10) })} />
      <p>{errors.age?.message}</p>

      <input type="submit" />
    </form>
  )
}