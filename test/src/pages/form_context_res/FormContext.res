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

// 연락처 정보 입력 컴포넌트
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

@react.component @genType
let default = () => {
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
