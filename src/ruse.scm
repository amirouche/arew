(import (chezscheme)
        (ruse matchable))

(define pk
  ;; Write ARGS, and return last value of ARGS
  (lambda args
    (write args)
    (newline)
    (car (reverse args))))

(define pk*
  ;; Write ARGS, and return last value of ARGS
  (lambda args
    (pretty-print args)
    (newline)
    (car (reverse args))))

(define (any predicate? objects)
  (let loop ((objects objects))
    (if (null? objects)
        #f
        (if (predicate? (car objects))
            #t
            (loop (cdr objects))))))

(define ref
  ;; Return the value associated with OBJECT in ALIST; otherwise
  ;; return DEFAULT. Comparison is done with PREDICATE?
  (case-lambda
   ((object alist)
    (ref object alist #f eq?))
   ((object alist default)
    (ref object object default eq?))
   ((object alist default predicate?)
    (let loop ((alist alist))
      (if (null? alist)
          default
          (if (predicate? object (caar alist))
              ;; Use cdar instead of cadar because that is a list of
              ;; cons unlike `let` bindings.
              (cdar alist)
              (loop (cdr alist))))))))

;; unify: try to find the value associated with SYMBOL inside a list
;; of cons called ENVIRONMENT. ENVIRONMENT looks like is an
;; association list where severals cons can have the same car.

(define unify-failure '(unify-failure))

(define unify-failed?
  (lambda (object)
    (eq? object unify-failure)))

(define unify
  (lambda (symbol environment)
    (let loop ((environment environment))
      (if (null? environment)
          unify-failure
          (if (eq? symbol (caar environment))
              (cdar environment)
              (loop (cdr environment)))))))

;; Helpers for set of symbols stored as lists

(define set-cons
  ;; add an object to a set
  (lambda (x set)
    (if (memq x set)
        set
        (cons x set))))

(define intersect
  (lambda set*
    (if (null? set*)
        '()
        (fold-left (lambda (seta setb)
                     (let loop ([seta seta] [fset '()])
                       (if (null? seta)
                           fset
                           (let ([a (car seta)])
                             (if (memq a setb)
                                 (loop (cdr seta) (cons a fset))
                                 (loop (cdr seta) fset))))))
                   (car set*) (cdr set*)))))

(define difference
  (lambda set*
    (if (null? set*)
        '()
        (fold-right (lambda (setb seta)
                      (let loop ([seta seta] [final '()])
                        (if (null? seta)
                            final
                            (let ([a (car seta)])
                              (if (memq a setb)
                                  (loop (cdr seta) final)
                                  (loop (cdr seta) (cons a final)))))))
                    (car set*) (cdr set*)))))

(define union
  (lambda set*
    (if (null? set*)
        '()
        (fold-left (lambda (seta setb)
                     (let loop ([setb setb] [seta seta])
                       (if (null? setb)
                           seta
                           (loop (cdr setb) (set-cons (car setb) seta)))))
                   (car set*) (cdr set*)))))

;; Other helpers

(define unique-var
  ;; Create a unique variable starting with NAME.
  (let ((count 0))
    (lambda (name)
      (let ([c count])
        (set! count (+ count 1))
        (string->symbol
         (string-append (symbol->string name) "." (number->string c)))))))

;; helper predicates

(define (primitive? symbol)
  (memq symbol '(javascript->procedure call/cc)))

(define (constant? object)
  (or (boolean? object)
      (number? object)
      (string? object)
      (vector? object)
      (bytevector? object)))

;; steps

  ;;; step-check

(define step-check
  (lambda (program)

    (define errors '())

    (define (error! error . irritants)
      (set! errors (cons (cons error irritants) errors)))

    (define check-body!
      (lambda (body environment)
        ;; XXX: Unlike the other check procedures, BODY is list of
        ;; expressions.

        ;; Allow definitions to be interleaved with expressions,
        ;; later it is translated to letrec*

        (let loop ((exprs body)
                   (environment environment))
          (if (null? (cdr exprs))
              ;; check that the last form is an expression, and not a define
              (match (car exprs)
                     ;; XXX: the following quoted define in the match
                     ;; clause, makes it impossible to override define.
                     (('define x ...) (error! "A body can not end with a define" (car exprs)))
                     (else (do! (car exprs) environment)))
              (match (car exprs)
                     (('define identifier expr ...)
                      (if (symbol? identifier)
                          ;; IDENTIFIER is visible in EXPR
                          (let ((environment (cons (cons identifier '()) environment)))
                            (do! expr environment)
                            (loop (cdr exprs) environment))
                          (error! "Define only support binding an identifier to an expression" (car exprs))))
                     (else (do! (car exprs) environment)
                           (loop (cdr exprs) environment)))))))

    (define check-let!
      (lambda (expr environment)
        ;; destructure let form
        (match expr
               (('let name ((i* e*) ...) body ...)
                (for-each (lambda (e) (do! e environment)) e*)
                (check-body! body (append (map (lambda (x) (cons x '())) i*) environment)))
               (('let ((i* e*) ...) body ...)
                (let loop ((bindings (map cons i* e*))
                           (identifiers '())
                           (environment-extra '()))
                  (if (null? bindings)
                      (check-body! body (append environment-extra environment))
                      (let ((i (caar bindings))
                            (e (cdar bindings)))
                        (when (any (lambda (x) (eq? x i)) identifiers)
                          (error! "The same identifier can not appear twice in a `let` bindings" i))
                        (do! e environment)
                        (loop (cdr bindings)
                              (cons i identifiers)
                              (cons (cons i '()) environment-extra)))))))))

    (define check-set!
      (lambda (expr environment)
        (match expr
               (('set! identifier expr) (do! expr environment))
               (else (error! "Invalid set!" expr)))))

    (define check-quote!)

    (define check-quasiquote!)

    (define check-lambda!
      (lambda (expr environment)
        (match expr
               (('lambda args body ...)
                (check-body! body (append (map (lambda (x) (cons x '())) args) environment))))))

    (define check-if!
      (lambda (expr environment)
        (match expr
               (('if a b c)
                (do! a environment)
                (do! b environment)
                (do! c environment))
               (else (error! "The form is is buggy" expr)))))

    (define (check-exit! expr environment)
      (if (fx=? (length expr) 2)
          (do! (cadr expr) environment)
          (error! "Wrong number of arguments in procedure exit" expr)))

    (define (check-javascript->procedure! expr environment)
      (if (fx=? (length expr) 3)
          (unless (string=? (cadr expr))
            (error! "Wrong type of argument in procedure javascript->procedure, expected a string, and arity number" expr))
          (error! "Wrong number of arguments in procedure javascript->procedure, expected a string, and arity number" expr)))

    (define check-call/cc!
      (lambda (expr environment)
        ;; TODO: stricter checks
        (unless (fx=? (length expr) 2)
          (error! "Wrong number of arguments in procedure call/cc, excepted one argument" expr))))

    (define environment-zero
      (list (cons 'let check-let!)
            (cons 'set! check-set!)
            (cons 'exit check-exit!)
            (cons 'javascript->procedure check-javascript->procedure!)
            (cons 'call/cc check-call/cc!)
            (cons 'lambda check-lambda!)
            (cons 'if check-if!)))

    (define application?
      (lambda (expr)
        (symbol? (car expr))))

    (define do!
      (lambda (expr environment)
        (match expr
               ((? null? expr) (values))
               ;; constants
               ((? constant? expr) (values))
               ;; a mere symbol should be bound according to unify. XXX:
               ;; it will not detect syntax errors. A mere `let` is ok
               ;; according to this clause.
               ((? symbol? expr) (when (unify-failed? (unify expr environment))
                                   (error! "Unbound identifier" expr)))
               ;; That is a list, do the check based on what the first
               ;; object resolve to...
               ((? application? expr)
                (let* ((identifier (car expr))
                       (checker! (unify identifier environment)))
                  (cond
                   ;; Here one might proceed to check whether the rest of
                   ;; the expression may make sense. Without knowledge
                   ;; about the first form of the expression, it can yield
                   ;; useless errors e.g. (letloop ((a b)) body ...) will
                   ;; report that A is unbound, that is the case because
                   ;; the named-let has a typo. To avoid useless error
                   ;; reporting, report the first error, and do not check
                   ;; the rest of the expression.
                   ((unify-failed? checker!) (error! "Unbound identifier" identifier))
                   ;; checkers is null when it was bound with a define, or
                   ;; a let-like form. Let's *consider* IDENTIFIER is
                   ;; bound to a procedure, and check that the arguments
                   ;; make a Scheme sense. TODO: check that IDENTIFIER is
                   ;; bound to a procedure.
                   ((null? checker!) (for-each (lambda (e) (do! e environment)) (cdr expr)))
                   ;; That is a known form, execute the ad-hoc check strategy.
                   ((procedure? checker!) (checker! expr environment))
                   (else (error 'ruse "compiler error 1" expr)))))
               ((e* ...) (for-each (lambda (e) (do! e environment)) e*))
               (else (error 'ruse "compiler error 2" expr)))))

    (do! program environment-zero)

    (if (null? errors)
        program
        (begin
          ;; TODO: add line / col / offset information, needs chez
          ;; annotations...
          (for-each (lambda (error)
                      (display (car error)) (display " ~ ") (write (cdr error)) (newline))
                    errors)
          (error 'ruse "programming errors")))))

  ;;; step-named-let->letrec

(define step-named-let->letrec
  (lambda (expr)
    (match expr
           ((? constant? c) c)
           ((? symbol? s) s)

           (('let name ((i* e*) ...) body ...)
            `(letrec (,name (lambda ,@i*) ,@body)
               (,name ,@e*)))

           ((e* ...) (map step-named-let->letrec e*)))))

  ;;; step-body->letrec*

(define step-body->letrec*
  (lambda (expr)

    (define body->letrec*
      (lambda (body)
        (let loop ((body body)
                   (out '()))
          (if (null? (cdr body))
              (if (null? out)
                  (do (car body))
                  `(letrec* ,(reverse out) ,(do (car body))))
              (let ((expr (car body)))
                (match expr
                       (('define identifier expr*)
                        (loop (cdr body) (list identifier (do expr*))))
                       (else (loop (cdr body) (cons (list (unique-var 'i) (do expr)) out)))))))))

    (define do
      (lambda (expr)
        (match expr
               ((? constant? c) c)
               ((? symbol? s) s)

               (('javascript->procedure string arity) `(javascript->procedure ,string ,arity))

               (('let ((i* e*) ...) body ...)
                `(let ,(map (lambda (i e) (list i (do e))) i* e*) ,(body->letrec* body)))

               (('lambda args body ...)
                `(lambda ,args ,(body->letrec* body)))

               ((e ...) (map do e)))))

    (do expr)))

  ;;; step-letrec*->let+box

(define step-letrec*->nested-let+set!
  (lambda (expr)

    (define (do expr)
      (match expr
             (('letrec* ((i* e*) ...) e)
              `(let ,(map (lambda (i) (list i '(void))) i*)
                 ,(let f ((bindings (map cons i* e*)))
                    (if (null? bindings)
                        (do e)
                        (let ((i (caar bindings))
                              (e (cdar bindings)))
                          `(let ((,(unique-var 'i) (set! ,i ,(do e))))
                             ,(f (cdr bindings))))))))

             ((? constant? c) c)
             ((? symbol? s) s)

             (('javascript->procedure string arity) `(javascript->procedure ,string ,arity))

             (('let ((i* e*) ...) body)
              `(let ,(map (lambda (i e) (list i (do e))) i* e*) ,(do body)))

             (('lambda args body)
              `(lambda ,args ,(do body)))

             ((e ...) (map do e))))

    (do expr)))

  ;;; step-let-as-lambda

(define step-let-as-lambda
  (lambda (expr)
    (match expr
           ((? constant? c) c)
           ((? symbol? s) s)

           (('javascript->procedure string arity) `(javascript->procedure ,string ,arity))

           (('let ((i* e*) ...) body ...)
            `((lambda ,i* ,@(map step-let-as-lambda body)) ,@(map step-let-as-lambda e*)))

           ((e ...) (map step-let-as-lambda e)))))

  ;;; step-continuation-passing-style

(define step-continuation-passing-style ;; cps
  (lambda (program)

    (define do
      (lambda (expr)
        (match expr
               ;; mere symbols are returned as-is
               ((? symbol? s) s)

               ;; A constant is returned as cps continuation
               ((? constant? c)
                (let ((r (unique-var 'r)))
                  `(lambda (,r) (,r ,c))))

               (('void) '(lambda (r) (r (void))))

               (('set! identifier expr)
                `(lambda (k)
                   (set! ,identifier (,(do expr) (lambda (o) (lambda (r) (r o)))))
                   (k (void))))

               (('if a b c)
                `(lambda (k)
                   (,(do a)
                    (lambda (kif)
                      (if kif
                          (,(do b) k)
                          (,(do c) k))))))

               ;; cps soup
               (('call/cc proc)
                (let ((k (unique-var 'k)))
                  `(lambda (,k)
                     (,(do proc)
                      (lambda (proc*)
                        (proc* ,k (lambda (r) (r (lambda (_ k) (k ,k))))))))))

               ;; A javascript->procedure is only wrapped with a cps
               ;; lambda but otherwise pass throught this step as-is
               (('javascript->procedure string arity)
                (let ((r (unique-var 'r)))
                  `(lambda (,r)
                     (,r
                      ,(let ((arguments (map (lambda (x) (unique-var 'a)) (iota arity)))
                             (q (unique-var 'q)))
                         `(lambda ,(cons q arguments)
                            (,q ((javascript->procedure ,string)
                                 ,@(map (lambda (a) (let ((r (unique-var 'r)))
                                                      `(,a (lambda (,r) ,r))))
                                        arguments)))))))))

               ;; A lambda is wrapped with cps lambda, and returned with
               ;; a cps continuation, and the lambda takes an extra cps
               ;; continuation argument
               (('lambda args body)
                (let ((k (unique-var 'k))
                      (r (unique-var 'r)))
                  `(lambda (,r)
                     (,r (lambda ,(cons k args) (,(do body) ,k))))))

               ;; A procedure call will be wrapped with cps lambda and
               ;; "eval" its arguments, the actual call will take the cps
               ;; continuation procedure of the cps (outter) lambda

               ;; TODO: XXX: there might be too much lambda nesting!  the
               ;; lambda that takes `r` as argument as especially
               ;; dubuious.
               ((e e* ... en)
                (let ((p (unique-var 'p))
                      (k (unique-var 'k)))
                  `(lambda (,k)
                     ;; here E is special cased, to be able to create a
                     ;; temporary variables P... for some unknown reason.
                     ;; the pattern could be e* ... en. That lead the
                     ;; following match clause.
                     (,(do e)
                      (lambda (,p)
                        ,(let f ((e* e*)
                                 (t* '()))
                           (if (null? e*)
                               (let ((t (unique-var 't))
                                     (r (unique-var 'r)))
                                 `(,(do en)
                                   (lambda (,t)
                                     (,p ,k
                                         ,@(map (lambda (t)
                                                  (define r (unique-var 'r))
                                                  `(lambda (,r) (,r ,t))) (reverse t*))
                                         (lambda (,r) (,r ,t))
                                         ))))
                               (let ((t (unique-var 't)))
                                 `(,(do (car e*))
                                   (lambda (,t)
                                     ,(f (cdr e*) (cons t t*))))))))))))
               ((e)
                (let ((p (unique-var 'p))
                      (k (unique-var 'k)))
                  `(lambda (,k)
                     (,(do e)
                      (lambda (,p)
                        (,p ,k))))))

               )))

    ;; the code is wrapped with do, to make it easier to the name of
    ;; the procedure, while keeping the lines short.
    (do program)))

;; main

(define wrap
  (lambda (program)
    `(begin

       (import (chezscheme))
       (import (ruse matchable))

       (define (pk . x)
         (write x) (newline)
         (car (reverse x)))

       (define javascript->procedure
         (lambda (x)
           (match x
                  ;; testing
                  ("process.exit" (lambda (x) (pk 'exit-code x) (exit x)))
                  ;; runtime
                  ("ruse.void" void)
                  ;; stdlib...
                  ("ruse.box" box)
                  ("ruse.setbox" set-box!)
                  ("ruse.unbox" unbox)
                  ("ruse.pk" pk)
                  ("ruse.add" +)
                  ("ruse.minus" -))))

       (,program (lambda x (apply pk 'out x))))))

(define eval*
  (lambda (program)
    (define program* (wrap program))
    (pretty-print program*)
    (eval program*)))

(define main
  (lambda ()

    (define steps
      (list step-check
            step-named-let->letrec
            step-body->letrec*
            step-letrec*->nested-let+set!
            step-let-as-lambda
            step-continuation-passing-style))

    (define example
      '(let ((+ (javascript->procedure "ruse.add" 2))
             (- (javascript->procedure "ruse.minus" 2))
             (pk (javascript->procedure "ruse.pk" 1))
             #;(box (javascript->procedure "ruse.box" 1))
             #;(set-box! (javascript->procedure "ruse.setbox" 2))
             #;(unbox (javascript->procedure "ruse.unbox" 1))
             (exit (javascript->procedure "process.exit" 1)))

         (let ((make-coroutine-generator
                (lambda (proc)
                  (let ((return #f)
                        (resume #f))
                    (let ((yield (lambda (v)
                                   (call/cc (lambda (r)
                                              (set! resume r)
                                              (return v))))))

                      (lambda () (call/cc
                                  (lambda (cc)
                                    (set! return cc)
                                    (if resume
                                        (resume #f #;(void))
                                        (let ()
                                          (proc yield)
                                          (set! resume
                                                (lambda (v) (return
                                                             #f
                                                             #;(eof-object))))
                                          (return #f #;(eof-object))))))))))))
           (let ((generator (make-coroutine-generator
                             (lambda (yield)
                               (yield 40)
                               (yield 1)
                               (yield 1)))))
             (exit (+ (+ (generator) (generator)) (generator)))))))


    (define example*
      '(let ((exit (javascript->procedure "process.exit" 1))
             (pk (javascript->procedure "ruse.pk" 1))
             (xxx 0))
         (set! xxx 42)
         (exit xxx)))

    (define example**
      `((lambda () 101)))

    (pretty-print example (current-error-port))

    (let loop ((steps steps)
               (program example))
      (if (null? steps)
          (begin
            (display "* fin:\n" (current-error-port))
            (pretty-print program (current-error-port))
            (newline)
            (display "* eval:\n" (current-error-port))
            (eval* program))
          (begin
            (format (current-error-port) "* ~s\n" (car steps))
            (let ((program ((car steps) program)))
              (pretty-print program (current-error-port))
              (loop (cdr steps) program)))))))

(main)
