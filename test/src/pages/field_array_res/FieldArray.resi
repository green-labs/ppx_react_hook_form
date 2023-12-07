@rhf
type item = {name: string, price: float, quantity: float}

@rhf
type inputs = {cart: array<item>}

@react.component @genType
let default: unit => Jsx.element
