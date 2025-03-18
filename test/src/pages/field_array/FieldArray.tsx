import { useForm, useFieldArray, useWatch, Control } from "react-hook-form";

type inputs = {
  cart: {
    name: string;
    price: number;
    quantity: number;
  }[];
};

const Total = ({ control }: { control: Control<inputs> }) => {
  const formValues = useWatch({
    name: "cart",
    control
  });
  const total = formValues.reduce(
    (acc, current) => acc + (current.price || 0) * (current.quantity || 0),
    0
  );
  return <p>Total Amount: {total}</p>;
};


export default function App() {
  const {
    register,
    control,
    handleSubmit,
    formState: { errors, dirtyFields }
  } = useForm<inputs>({
    defaultValues: {
      cart: [{ name: "test", quantity: 1, price: 23 }]
    },
    mode: "onBlur"
  });
  const { fields, append, remove } = useFieldArray({
    name: "cart",
    control
  });
  const onSubmit = (data: inputs) => console.log(data);

  console.log("!!", errors)

  const isCartPriceFieldDirty = (index: number) =>
    (dirtyFields.cart?.[index]?.price) ?
      dirtyFields.cart?.[index]?.price
      :
      false

  return (
    <div>
      <form onSubmit={handleSubmit(onSubmit)}>
        {fields.map((field, index) => {
          return (
            <div key={field.id}>
              <section className={"section"} key={field.id}>
                <input
                  placeholder="name"
                  {...register(`cart.${index}.name` as const, {
                    required: true
                  })}
                  className={errors?.cart?.[index]?.name ? "error" : ""}
                />
                <input
                  placeholder="quantity"
                  type="number"
                  {...register(`cart.${index}.quantity` as const, {
                    valueAsNumber: true,
                    required: true
                  })}
                  className={errors?.cart?.[index]?.quantity ? "error" : ""}
                />
                <input
                  placeholder="value"
                  type="number"
                  {...register(`cart.${index}.price` as const, {
                    valueAsNumber: true,
                    required: true
                  })}
                  className={errors?.cart?.[index]?.price ? "error" : ""}
                />
                <span>
                  {(isCartPriceFieldDirty(index) ? "dirty" : "clean")}
                </span>
              </section>
              <button type="button" onClick={() => remove(index)}>
                DELETE
              </button>
            </div>
          );
        })}


        <Total control={control} />


        <button
          type="button"
          onClick={() =>
            append({
              name: "",
              quantity: 0,
              price: 0
            })
          }
        >
          APPEND
        </button>
        <input type="submit" />
      </form>
    </div>
  );
}