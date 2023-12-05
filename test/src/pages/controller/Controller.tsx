import { Checkbox } from "@material-ui/core"
import { useForm, Controller, SubmitHandler } from "react-hook-form"

type Inputs = {
  myCheckBox: boolean,
}

export default function Basic() {
  const {
    control,
    handleSubmit,
    watch,
    reset
  } = useForm<Inputs>({
    defaultValues: {
      myCheckBox: false
    }
  })
  const onSubmit: SubmitHandler<Inputs> = (data) => console.log(data)

  console.log(watch("myCheckBox"))

  return (
    <form onSubmit={handleSubmit(onSubmit)}>
      <Controller
        name="myCheckBox"
        control={control}
        rules={{ required: true }}
        render={({ field }) => <Checkbox {...field} />}
      />
      <input type="submit" />
    </form>
  )
}