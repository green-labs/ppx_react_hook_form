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
