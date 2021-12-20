(alias display chez-display)

(alias write chez-write)

(define write-shared
  (case-lambda
   ((obj)
    (write-shared obj (current-output-port)))
   ((obj port)
    (chez-parameterize ([chez-print-graph #t])
      (chez-write obj port)))))

(define write-simple
  (case-lambda
   ((obj)
    (write-shared obj (current-output-port)))
   ((obj port)
    (chez-parameterize ([chez-print-graph #f])
      (write obj port)))))
