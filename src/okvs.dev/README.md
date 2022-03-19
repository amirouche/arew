# `(import (okvs.dev))`

![](okvs.dev-cover.jpg)

## Status

**Rework draft.**

## Issues

## Abstract

## Rationale

`okvs.dev` can be the primitive datastructure for building many
datastructures, called *extensions*. Low level extensions include
counter, bag, set, and multi-mapping. Higher level extensions include
[Entity-Attribute-Value](https://en.wikipedia.org/wiki/Entity%E2%80%93attribute%E2%80%93value_model)
possibly supported by datalog, Generic Tuple Store (nstore) inspired
from [Resource Description
Framework](https://en.wikipedia.org/wiki/Resource_Description_Framework)
that can match the query capabilities of
[SPARQL](https://www.w3.org/TR/rdf-sparql-query/), and
[RDF-star](https://w3c.github.io/rdf-star/cg-spec/), or the Versioned
Generic Tuple Store (vnstore), that ease the implementation of
bitemporal databases. Also, it is possible to implement a property
graph database, ranked set, leaderboard, priority queue. It is
possible to implement efficient geometric queries with the help of
xz-ordered curves.

There is several existing databases that expose an interface similar
to `okvs.dev`, and even more that use an Ordered Key-Value Store
(OKVS) as their backing storage.

While `okvs.dev` interface is lower-level than the mainstream SQL, it
has less moving pieces, and stems from the well-known datastructure
part of every software engineering curriculum, namely binary trees, it
makes `okvs.dev` a better teaching material with immediate useful
applications, including building your own SQL database. Last but not
least, `okvs.dev` reflects the current practices where databases, that
are possibly distributed, are based on a similar interface.

`okvs.dev` extends the OKVS interface inherited from BerkeleyDB to
make the implementation of efficient extensions easier thanks to the
ability to estimate the count of keys, and the count of bytes.

## Reference

### `(make-okvs.dev)`

Returns a handle for the database.

### `(okvs.dev? obj)`

Returns `#t` if `OBJ` is an instance of `<okvs.dev>`. Otherwise,
returns `#f`.

### `(okvs.dev-close! okvs.dev)`

Close `okvs.dev`.

### `(okvs.dev-transaction? obj)`

Returns `#t` if `OBJ` is an `<okvs.dev-transaction>` instance. Otherwise,
returns `#f`.

### `(okvs.dev-cursor? obj)`

Returns `#t` if `OBJ` is an `<okvs.dev-cursor>` instance. Otherwise,
returns `#f`.

### `(okvs.dev-handle? obj)`

Returns `#t` if `OBJ` satisfy either `okvs.dev?`, `okvs.dev-transaction?`, or
`okvs.dev-cursor?`. Otherwise, returns `#f`.

### `(okvs.dev-key-max-size handle)`

Returns the maximum size of a key for the database associated with
`HANDLE`.

### `(okvs.dev-value-max-size handle)`

Returns the maximum size of a value of the database associated with
`HANDLE`.

### `(make-okvs.dev-transaction-parameter init)`

Returns a procedure that may take one or two arguments:

- One argument: the procedure takes a transaction as first argument
and returns the current value for the given transaction. `INIT` is the
initial value.

- Two arguments: the procedure takes a transaction as first argument,
and a new value for the associated transaction. It returns no values.

In the following example, `okvs.dev-in-transaction` will return `#f`:

```scheme
(define read-only? (make-okvs.dev-transaction-parameter #t))

(define (proc tx)
  (display (read-only? tx)) ;; => #t
  (okvs.dev-set! tx #u(42) #u8(13 37))
  (read-only? tx #f)
  ...
  (read-only? tx))

(okvs.dev-in-transaction okvs.dev proc) ;; => #f
```

### `(okvs.dev-transaction-parametrize ((parameter value) ...) expr ...)`

Similar to `parametrize`.

### `(okvs.dev-begin-hook handle)`

Returns SRFI-173 hook associated with the beginning of a transaction.
This hook gives a chance to extension libraries to initialize their
internal states.

### `(okvs.dev-pre-commit-hook handle)`

Returns SRFI-173 hook associated with the end of a transaction.  This
hook gives a chance to extension libraries to execute triggers.

### `(okvs.dev-post-commit-hook handle)`

Returns SRFI-173 hook associated with the success of a transaction.
This hook may be used to implement features such as notify or watches.

### `(okvs.dev-rollback-hook handle)`

Returns SRFI-173 hook associated with the rollback of a transaction.

### `(okvs.dev-in-transaction okvs.dev proc [failure [success]])`

Begin a transaction against the database, and execute `PROC`. `PROC`
is called with first and only argument an object that satisfy
`okvs.dev-transaction?`. In case of error, rollback the transaction and
execute `FAILURE` with the error object as argument. The default value
of `FAILURE` re-raise the error with `raise`. Otherwise, executes
`SUCCESS` with the returned values of `PROC`.  The default value of
`SUCCESS` is the procedure `values`.

When the transaction begin, `okvs.dev-in-transaction` must call the
procedures associated with `okvs.dev-begin-hook`.

Just before the transaction commit, `okvs.dev-in-transaction` must
call the procedures associated with `okvs.dev-pre-commit-hook`.

Just after the transaction commit is a success,
`okvs.dev-in-transaction` must call the procedures associated with
`okvs.dev-post-commit-hook`.

Just before calling `FAILURE`, `okvs.dev-in-transaction` must call
the procedures associated with `okvs.dev-rollback-hook`.

`okvs-in-transaction` describes the extent of the atomic property, the
A in [ACID](https://en.wikipedia.org/wiki/ACID), of changes against
the underlying database. A transaction will apply all database
operations in `PROC` or none: all or nothing. When
`okvs.dev-in-transaction` returns successfully, the changes will be
visible for future transactions, and implement durability, D in
ACID. In case of error, changes will not be visible to other
transactions in all cases. Regarding isolation, and consistency,
respectively the I and C in ACID, TODO...

### `(okvs.dev-call-with-cursor handle proc)`

Open a cursor against `HANDLE` and call `PROC` with it. When `PROC`
returns, the cursor is closed.

If `HANDLE` satisfy `okvs.dev?`, `okvs.dev-call-with-cursor` must wrap the
call to `PROC` with `okvs-in-transaction`.

If `HANDLE` satisfy `okvs.dev-cursor?`, `okvs.dev-call-with-cursor` must pass
a cursor positioned at the same position as `HANDLE`.

The cursor is stable: the cursor will see a snapshot of keys of the
database taken when `okvs.dev-call-with-cursor` is called. During the
extent of `PROC`, `okvs.dev-set!` and `okvs.dev-remove!`  will not
change the position of the cursor, and the cursor will not see removed
keys, and not see added keys. Keys which value was changed during
cursor navigation, that exist when `okvs.dev-call-with-cursor` is
called, can also be seen.

### `(okvs.dev-estimate-key-count handle [key other [offset [limit]]])`

Returns an estimate count of keys between `KEY` and `OTHER`. If `KEY`
and `OTHER` are omitted return the approximate count of keys in the
whole database.

If `OFFSET` integer is provided, `okvs.dev-estimate-key-count` will
skip the first `OFFSET` keys from the count.

If `LIMIT` integer is provided, `okvs.dev-estimate-key-count` will
consider `LIMIT` keys from the count.

Rationale: It is helpful to know how big is a range to be able to tell
which index to use as seed. Imagine a query against two attributes,
each attribute with their own index and no compound index: being able
to tell which subspace contains less keys, can speed up significantly
query time.

### `(okvs.dev-estimate-bytes-count handle [key [other [offset [limit]]]])`

Returns the estimated size in bytes of key-value pairs in the subspace
described by `KEY` and `OTHER`. If `OTHER` is omitted, return the
approximate size of the key-value pair associated with `KEY`. When
both `KEY` and `OTHER` are omitted return the estimated size of the
whole database associated with `HANDLE`.

If `OFFSET` integer is provided, `okvs.dev-estimate-bytes-count` will
skip the first `OFFSET` keys from the count.

If `LIMIT` integer is provided, `okvs.dev-estimate-bytes-count` will
consider `LIMIT` keys from the count.

Rationale: That is useful in cases where the size of a transaction is
limited.

### `(okvs.dev-set! handle key value)`

Associate the bytevector `KEY`, with the bytevector `VALUE`.

### `(okvs.dev-remove! handle key) okvs.dev-handle? bytevector?`

Removes the bytevector `KEY`, and its associated value.

### `(okvs.dev-cursor-search cursor key)`

Position `CURSOR` on `KEY` or an adjacent key. Returns a symbol
describing the position of the cursor:

- Return `'cursor-exact-key`, when `KEY` is found;

- Returns `'cursor-before-key`, when `KEY` is not found, and the cursor is
  positioned on a key, before `KEY`;

- If there is no key before `KEY`, but there is a key after, returns
  `'cursor-after-key`;

- If the the database is empty, returns `'cursor-empty`;

### `(okvs.dev-cursor-next? cursor) okvs.dev-cursor? → boolean?`

Move the `CURSOR` to the next key if any, and returns `#t`. Otherwise,
if there is no next key returns `#f`. `#f` means the cursor reached
the end of the key space.

### `(okvs.dev-cursor-previous? cursor) okvs.dev-cursor? → boolean?`

Move the `CURSOR` to the previous key if any, and returns
`#t`. Otherwise, if there is no previous key returns `#f`. `#f` means
the cursor reached the begining of the key space.

### `(okvs.dev-cursor-key cursor)` okvs.dev-cursor? → bytevector?`

Returns the key bytevector on the position of the `CURSOR`.

It is an error to call `okvs.dev-cursor-key`, when `CURSOR` reached
the begining or end of the key space.

### `(okvs.dev-cursor-value cursor)` okvs.dev-cursor? → bytevector?`

Returns the value bytevector on the position of the `CURSOR`.

It is an error to call `okvs.dev-cursor-key`, when `CURSOR` reached
the begining or end of the key space.

### `(okvs.dev-query handle key [other [offset [limit]]])`

`OKVS.DEV-QUERY` will query the associated database.

If only `KEY` is provided it will return the associated value
bytevector; or `#f` if `KEY` is not present.

If `OTHER` is provided there is two cases:

- `KEY < OTHER` then `okvs.dev-query` returns a list with all the
  key-value pairs present in the database between `KEY` and `OTHER`
  excluded ie. without the key-value pair associated with `OTHER` if
  any;

- `OTHER < KEY` then `okvs.dev-query` returns a list with all the key-value
  pairs present in the database between `OTHER` and `KEY` starting at `OTHER`
  in reverse lexicographic order, any key-value pair associated with `KEY`
  is excluded;

If `OFFSET` integer is provided, `okvs.dev-query` will skip as much
key-value pairs from the start of the described subspace.

If `LIMIT` integer is provided the `okvs.dev-query` will produce a
list with at most `LIMIT` key-value pairs.

### `(okvs.dev-bytevector-next-prefix bytevector) bytevector? → bytevector?`

Returns the first bytevector that follows `BYTEVECTOR` according to
lexicographic order that is not prefix of `BYTEVECTOR` such as the
following code iterates over all keys that have `key` as prefix:

```scheme
(okvs.dev-query handle key (okvs.dev-bytevector-next-prefix key))
```
