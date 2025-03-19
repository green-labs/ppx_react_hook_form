import { useForm, FormProvider, useFormContext, SubmitHandler } from 'react-hook-form';

// 폼 데이터 타입 정의
interface FormInputs {
  firstName: string;
  lastName: string;
  email: string;
  age: number;
}

// 최상위 폼 컴포넌트
const FormContextExample = () => {
  const methods = useForm<FormInputs>({
    defaultValues: {
      firstName: '',
      lastName: '',
      email: '',
      age: 0
    }
  });

  const onSubmit: SubmitHandler<FormInputs> = (data) => {
    console.log(data);
    alert(JSON.stringify(data, null, 2));
  };

  return (
    <FormProvider {...methods}>
      <form onSubmit={methods.handleSubmit(onSubmit)}>
        <h2>프로필 정보</h2>
        <PersonalInfo />
        <ContactInfo />
        <button type="submit">제출하기</button>
      </form>
    </FormProvider>
  );
};

// 개인 정보 입력 컴포넌트
const PersonalInfo = () => {
  const { register, formState: { errors } } = useFormContext<FormInputs>();

  return (
    <div>
      <h3>개인 정보</h3>
      <div>
        <label htmlFor="firstName">이름: </label>
        <input
          id="firstName"
          {...register("firstName", { required: true })}
        />
        {errors.firstName && <p>이름을 입력해주세요</p>}
      </div>

      <div>
        <label htmlFor="lastName">성: </label>
        <input
          id="lastName"
          {...register("lastName", { required: true })}
        />
        {errors.lastName && <p>성을 입력해주세요</p>}
      </div>

      <div>
        <label htmlFor="age">나이: </label>
        <input
          id="age"
          type="number"
          {...register("age", {
            required: true,
            min: 0
          })}
        />
        {errors.age && <p>나이를 입력해주세요, 나이는 0보다 커야 합니다.</p>}
      </div>
    </div>
  );
};

// 연락처 정보 입력 컴포넌트
const ContactInfo = () => {
  const { register, formState: { errors } } = useFormContext<FormInputs>();

  return (
    <div>
      <h3>연락처 정보</h3>
      <div>
        <label htmlFor="email">이메일: </label>
        <input
          id="email"
          {...register("email", {
            required: true,
            pattern: /^\S+@\S+$/i
          })}
        />
        {errors.email && <p>이메일을 입력해주세요</p>}
      </div>
    </div>
  );
};

export default FormContextExample;
