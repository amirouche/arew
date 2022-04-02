# Ruse

## booleans, representation `#false`, `#true`, predicate `boolean?`

## integers, predicate: `integer?`

## inert: representation `#inert`, predicate: `inert?`

## null: representation `#null`, predicate: `null?`

## equivalence under mutation: `eq?`

## equivalence: `equal?`

## branch control: `if`

## pair: `cons`, `car`, and `cdr`

## `eval`

## `define`

## `set!`

## `call-with-current-context`, `context?`, `guard`, `root`, `context-continuation`

## `make-encapsulation-type` -> `(e p? d)`

- `encapsulator`
- `predicate?`
- `decapsulator`

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

## `make-keyed-static-variable` -> `(b a)`

Returns a list of the form `(b a)`, where `b` and `a` are
applicatives, as follows. Each call to `make-keyed-static-variable`
returns different `b` and `a`.

- `b` is an applicative that takes two arguments, the second of which
  must be an environment. It constructs and returns a
  child-environment of its second argument, with initially no local
  bindings.

- `a` is an applicative that takes zero arguments. If the dynamic
  environment `e` of the call to a has an improper ancestor that was
  constructed by a call to `b`, then a returns the value of the first
  argument passed to `b` in the first such environment encountered by
  a depth-first traversal of the improper ancestors of `e`. If `e` has
  no improper ancestors constructed via `b`, an error is signaled.

## `match`
