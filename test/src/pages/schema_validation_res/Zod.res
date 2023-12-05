type t

@module("zod")
external z: t = "default"

type string_ = string
type number = float
type object<'form> = 'form
type optional<'value> = option<'value>
type params = {required_error?: string}

@send
external string: (t, params) => string_ = "string"

@send
external number: (t, params) => number = "number"

@send
external object: (t, 'schema) => object<'schema> = "object"

@send
external optional: 'z => optional<'z> = "optional"
