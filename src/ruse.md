# Ruse

## `#inert`, `inert?`

## `assume`

## equivalence under mutation: `eq?`

## equivalence: `equal?`

## `#false`, `#true`, `boolean?`, `if`

## `string?`, `string-ref`, `string-append`

## `pair?`, `cons`, `car`, `cdr`, `#null`, `null?`

## `number?`, `+`, `-`, `*`, `/`

## `apply`

## `define`

## `set!`

## `call-with-current-context`, `context?`, `guard`, `context-root`, `context-continuation`

## `make-encapsulation` -> `(list encapsulator predicate? decapsulator)`

## `make-keyed-dynamic-variable` -> `(b a)`

Returns a list of the form `(b a)`, where `b` and `a` are
applicatives, as follows. Each call to `make-keyed-dynamic-variable`
returns different `b` and `a`.

- `b` is an applicative that takes two arguments, the second of which
  must be a combiner. It calls its second argument with no operands
  (`nil` operand tree) in a fresh empty environment, and returns the
  result.

- `a` is an applicative that takes zero arguments. If the call to `a`
  occurs within the dynamic extent of a call to `b`, then `a`
  returns the value of the first argument passed to `b` in the
  smallest enclosing dynamic extent of a call to `b`. If the call to
  `a` is not within the dynamic extent of any call to `b`, an error is
  signaled.

## `make-box`, `box-set!, `unbox`

## hash-table

## pattern matching
