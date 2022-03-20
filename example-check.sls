(library (example-check)

  (export check-0 check-1)
  (import (chezscheme))


  (define check-0
    (lambda ()
      (display "Running check 0\n")))

  (define check-1
    (lambda ()
      (display "Running check 1\n"))))
