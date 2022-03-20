(library (lib)

  (export foobar qux)

  (import (chezscheme))

  (define foobar 1337)

  (define qux (lambda () 1337)))
