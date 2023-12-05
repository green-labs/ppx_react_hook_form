let getFieldError: (option<array<'a>>, int) => option<'a> = %raw(`(field, index) => field?.[index]`)
