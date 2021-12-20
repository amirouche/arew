
## `(arew entangle)`

This library expose Linux `epoll` facility with separate continuations
callbacks for read and write readyness.

It expose a new type `<entangle>`, the constructor is `make-entangle`
and the predicate `entangle?`.

- related: https://github.com/libuv/libuv

### `(entangle-register-read! entangle fd thunk)`

Register the file descriptor `FD` with `ENTANGLE`. When `FD` is ready
to be *read*, and when `entangle-continue` is called, `THUNK` will be
called.

> Warning: You can not register the same file descriptor twice with
> the same read or write event type.

### `(entangle-register-write! entangle fd thunk)`

Register the file descriptor `FD` with `ENTANGLE`. When `FD` is ready
to be *written*, when `entangle-continue` is called `THUNK` will be
called.

> Warning: You can not register the same file descriptor twice with
> the same read or write event type.

### `(entangle-unregister-read! entangle fd)`

Unregister the file descriptor `FD` for *read* events with `ENTANGLE`.

### `(entangle-unregister-write! entangle fd)`

Unregister the file descriptor `FD` for *write* events with `ENTANGLE`.

### `(entangle-continue entangle milliseconds)`

Wait for events registred with `ENTANGLE` for
`MILLISECONDS`. `entangle-continue` will call the continuations
registred with `entangle-register-read!` and
`entangle-register-write!` according to every file descriptor type of
readyness.

`MILLISECONDS` is a timeout:

- If there is no file descriptor ready
after `MILLISECONDS`, `entangle-continue` will return.

- When `MILLISECONDS` is `-1`, `entangle-continue` will passively
block indefinitly.

- When `MILLISECONDS` is zero, `entangle-continue` will not wait and
will exec the continuations for the file descriptor that are already
ready at the moment of the call.

A `timeout` bigger than zero, does not mean that `entangle-continue`
will necessarly return after `timeout`, it may return
sooner.

`entangle-continue` will block passively until either:

- one or more file descriptors are ready;
- the call is interrupted by a signal handler; or
- the timeout expires.

> **Note:** Usually you should set the timeout so that,
> `entangle-continue` returns so that the event-loop may have a chance
> to execute some code that is scheduled at a fixed time. Otherwise,
> when the event-loop has nothing else to do but network input/output,
> timeout should be `-1`.

### `(entangle? obj)`

Return `#t` if `OBJ` is an `<entangle>` instance. Otherwise, it
returns `#f`.

### `(entangle-epoll entangle)`

Return the file descriptor used by `ENTANGLE` as a fixnum.

### `(entangle-close entangle)`

Close `ENTANGLE`. It closes the file descriptor returned by
`entangle-epoll`.

### `(make-entangle)`

Return an initialized `<entangle>` instance.
