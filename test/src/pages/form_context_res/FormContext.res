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
      <h3> {"개인 정보"->React.string} </h3>
      <div>
        <label htmlFor="firstName"> {"이름"->React.string} </label>
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
        | Some(_) => <p> {"이름을 입력해주세요"->React.string} </p>
        | _ => React.null
        }}
      </div>
      <div>
        <label htmlFor="lastName"> {"성"->React.string} </label>
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
        | Some(_) => <p> {"성을 입력해주세요"->React.string} </p>
        | _ => React.null
        }}
      </div>
      <div>
        <label htmlFor="age"> {"나이"->React.string} </label>
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
        | Some(_) =>
          <p>
            {"나이를 입력해주세요, 나이는 0보다 커야 합니다."->React.string}
          </p>
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
      <h3> {"연락처 정보"->React.string} </h3>
      <div>
        <label htmlFor="email"> {"이메일"->React.string} </label>
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
        | Some(_) => <p> {"이메일을 입력해주세요"->React.string} </p>
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
      <h2> {"프로필 정보"->React.string} </h2>
      <PersonalInfo />
      <ContactInfo />
      <button type_="submit"> {"제출하기"->React.string} </button>
    </form>
  </FormProviderOfInputs>
}
