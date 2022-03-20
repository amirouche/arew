(define (translate-status status)
  (case status
    ((#t) 0)
    ((#f) 1)
    (else status)))

(define exit
  (case-lambda
   (()
    (chez-exit))
   ((status)
    (chez-exit (translate-status status)))))

(define native-emergency-exit
  (let ((c-exit (chez-foreign-procedure "(cs)c_exit" (integer-32) void)))
    (case-lambda
     (()
      (c-exit 0))
     ((status)
      (c-exit status)))))

(define emergency-exit
  (case-lambda
   (()
    (native-emergency-exit))
   ((status)
    (native-emergency-exit (translate-status status)))))

(alias command-line chez-command-line)
(alias get-environment-variable chez-getenv)
