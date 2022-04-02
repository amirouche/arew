



(define foo
  (vau (a b c . rest) env
    (frob a b (eval c))))

(define bar
  (vau (a b c . rest) #ignore
    (frob a b)))

(define qux
  (lambda (a b c . rest)
    (frob a b c)))

(spawn (lambda ()
         (+ 40 2)))

(call-with-current-context
 (lambda (ctx)
   (spawn (lambda () (+ 40 2) +infinity ctx))))

(call-with-current-context
 (lambda (ctx)
   (define (context-environment ctx) foobar 42)
    ;; display: 42
   (spawn (lambda () (display (eval foobar (context-environment (context-current)))))
          +infinity
          ctx)))

 (call-with-current-context
  (lambda (ctx)
    (define (context-environment ctx) foobar 42)
    ;; it's an error, because foobar is not set in the context's environment
    (spawn (lambda () (display (eval foobar (context-environment (context-current)))))
           +infinity)))
