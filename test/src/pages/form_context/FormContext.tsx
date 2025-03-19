import { useForm, FormProvider, useFormContext, SubmitHandler } from 'react-hook-form';

interface FormInputs {
  firstName: string;
  lastName: string;
  email: string;
  age: number;
}

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
        <h2>Profile Information</h2>
        <PersonalInfo />
        <ContactInfo />
        <button type="submit">Submit</button>
      </form>
    </FormProvider>
  );
};

const PersonalInfo = () => {
  const { register, formState: { errors } } = useFormContext<FormInputs>();

  return (
    <div>
      <h3>Personal Information</h3>
      <div>
        <label htmlFor="firstName">First Name: </label>
        <input
          id="firstName"
          {...register("firstName", { required: true })}
        />
        {errors.firstName && <p>Please enter your first name</p>}
      </div>

      <div>
        <label htmlFor="lastName">Last Name: </label>
        <input
          id="lastName"
          {...register("lastName", { required: true })}
        />
        {errors.lastName && <p>Please enter your last name</p>}
      </div>

      <div>
        <label htmlFor="age">Age: </label>
        <input
          id="age"
          type="number"
          {...register("age", {
            required: true,
            min: 0
          })}
        />
        {errors.age && <p>Please enter your age, it must be greater than 0</p>}
      </div>
    </div>
  );
};

const ContactInfo = () => {
  const { register, formState: { errors } } = useFormContext<FormInputs>();

  return (
    <div>
      <h3>Contact Information</h3>
      <div>
        <label htmlFor="email">Email: </label>
        <input
          id="email"
          {...register("email", {
            required: true,
            pattern: /^\S+@\S+$/i
          })}
        />
        {errors.email && <p>Please enter your email</p>}
      </div>
    </div>
  );
};

export default FormContextExample;
