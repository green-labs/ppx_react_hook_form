# v0.0.10(unreleased)

- Change control field from optional to required in useWatchParamsOfInputs type

# v0.0.9

- Add `dirtyFields` field to `formState`
- Add `valueAsNumber` field to the option argument of `register`
- Add new methods to useFormReturn: `clearErrors`, `resetField`, `setError`, `setFocus`, `trigger`, `unregister`
- Expand validation options in `register`: `maxLength`, `minLength`, `max`, `min`, `pattern`
- Add `useFormContext` hook and `FormProvider` module

# v0.0.8

- Add `useWatch` hook
- Updated Controller component type definition in react-hook-form: made name parameter required and rules parameter optional

# v0.0.7

- Add `getValues` to `useForm` hook
- Change type `*withId` with optional fields

# v0.0.6

- Remove `-bs-super-errors` flag for the compatibility with ReScript v12

# v0.0.5

- Support ReScript v12

# v0.0.4

- Change type signature of append in useFieldArray hook
  - Updated from `t => unit` to `defaultValuesOfT => unit`
  - This change provides better type flexibility when appending new items to the array

# v0.0.3

- Enhance defaultValuesOf type definition for array types
  - For array types (e.g., `array<t>`), the defaultValuesOf type now uses 
    `defaultValuesOfT` for the array items instead of `array<t>`
  - This allows for more precise default value handling in nested array structures

# v0.0.2

- Add setValueConfig
- Add more variants for watch return type and watch returns option type

# v0.0.1

- Support useForm, useFieldArray, Controller
