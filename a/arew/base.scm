(alias * chez-*)

(alias for-each chez-for-each)

(alias map chez-map)

(alias + chez-+)

(alias - chez--)

(alias ... chez-...)

(alias / chez-/)

(alias < chez-<)

(alias <= chez-<=)

(alias = chez-=)

(alias => chez-=>)

(alias > chez->)

(alias >= chez->=)

(alias _ chez-_)

(alias abs chez-abs)

(alias and chez-and)

(alias append chez-append)

(alias apply chez-apply)

(alias assq chez-assq)

(alias assv chez-assv)

(alias begin chez-begin)

(alias binary-port? chez-binary-port?)

(alias boolean=? chez-boolean=?)

(alias boolean? chez-boolean?)

(alias bytevector chez-bytevector)

(alias bytevector-copy chez-bytevector-copy)

(alias bytevector-copy! chez-bytevector-copy!)

(alias bytevector-length chez-bytevector-length)

(alias bytevector-u8-ref chez-bytevector-u8-ref)

(alias bytevector-u8-set! chez-bytevector-u8-set!)

(alias bytevector? chez-bytevector?)

(alias caar chez-caar)

(alias cadr chez-cadr)

(alias call-with-current-continuation chez-call-with-current-continuation)

(alias call-with-port chez-call-with-port)

(alias call-with-values chez-call-with-values)

(alias call/cc chez-call/cc)

(alias car chez-car)

(alias case chez-case)

(alias cdar chez-cdar)

(alias cddr chez-cddr)

(alias cdr chez-cdr)

(alias ceiling chez-ceiling)

(alias char->integer chez-char->integer)

(alias char-ready? chez-char-ready?)

(alias char<=? chez-char<=?)

(alias char<? chez-char<?)

(alias char=? chez-char=?)

(alias char>=? chez-char>=?)

(alias char>? chez-char>?)

(alias char? chez-char?)

(alias close-input-port chez-close-input-port)

(alias close-output-port chez-close-output-port)

(alias close-port chez-close-port)

(alias complex? chez-complex?)

(alias cond chez-cond)

(alias cons chez-cons)

(alias current-error-port chez-current-error-port)

(alias current-input-port chez-current-input-port)

(alias current-output-port chez-current-output-port)

(alias define-syntax chez-define-syntax)

(alias define-values chez-define-values)

(alias denominator chez-denominator)

(alias do chez-do)

(alias dynamic-wind chez-dynamic-wind)

(alias else chez-else)

(alias eof-object chez-eof-object)

(alias eof-object? chez-eof-object?)

(alias eq? chez-eq?)

(alias equal? chez-equal?)

(alias eqv? chez-eqv?)

(alias error chez-error)

(alias error-object-irritants chez-condition-irritants)

(alias error-object-message chez-condition-message)

(alias error-object? chez-condition?)

(alias even? chez-even?)

(alias exact chez-exact)

(alias exact-integer-sqrt chez-exact-integer-sqrt)

(define (exact-integer? x)
  (and (exact? x) (integer? x)))

(alias exact? chez-exact?)

(alias expt chez-expt)

(alias file-error? chez-i/o-error?)

(alias floor chez-floor)

(alias flush-output-port chez-flush-output-port)

(alias gcd chez-gcd)

(alias get-output-string chez-get-output-string)

(alias guard chez-guard)

(alias if chez-if)

(alias inexact chez-inexact)

(alias inexact? chez-inexact?)

(define input-port-open?
  (lambda (port)
    (and (not (chez-port-closed? port)) (chez-input-port? port))))

(alias input-port? chez-input-port?)

(alias integer->char chez-integer->char)

(alias integer? chez-integer?)

(alias lcm chez-lcm)

(alias length chez-length)

(alias let chez-let)

(alias let* chez-let*)

(alias let*-values chez-let*-values)

(alias let-syntax chez-let-syntax)

(alias let-values chez-let-values)

(alias letrec chez-letrec)

(alias letrec* chez-letrec*)

(alias letrec-syntax chez-letrec-syntax)

(alias list chez-list)

(alias list->string chez-list->string)

(alias list->vector chez-list->vector)

(alias list-ref chez-list-ref)

(define (list-set! l k obj)
  (define (itr cur count)
    (if (= count k)
        (set-car! cur obj)
        (itr (cdr cur) (+ count 1))))
  (itr l 0))

(alias list-tail chez-list-tail)

(alias list? chez-list?)

(alias make-bytevector chez-make-bytevector)

(alias make-parameter chez-make-parameter)

(alias make-string chez-make-string)

(alias make-vector chez-make-vector)

(alias max chez-max)

(alias memq chez-memq)

(alias memv chez-memv)

(alias min chez-min)

(alias modulo chez-modulo)

(alias negative? chez-negative?)

(alias newline chez-newline)

(alias not chez-not)

(alias null? chez-null?)

(alias number->string chez-number->string)

(alias number? chez-number?)

(alias numerator chez-numerator)

(alias odd? chez-odd?)

(alias open-input-string chez-open-input-string)

(alias open-output-string chez-open-output-string)

(alias or chez-or)

(define (output-port-open? port)
  (and (not (chez-port-closed? port)) (chez-output-port? port)))

(alias output-port? chez-output-port?)

(alias pair? chez-pair?)

(alias parameterize chez-parameterize)

(alias peek-char chez-peek-char)

(alias peek-u8 chez-lookahead-u8)

(alias port? chez-port?)

(alias positive? chez-positive?)

(alias procedure? chez-procedure?)

(alias quasiquote chez-quasiquote)

(alias quote chez-quote)

(alias quotient chez-quotient)

(alias raise chez-raise)

(alias raise-continuable chez-raise-continuable)

(alias rational? chez-rational?)

(alias rationalize chez-rationalize)

(alias read-bytevector chez-get-bytevector-n)

(alias read-char chez-read-char)

(alias read-error? chez-lexical-violation?)

(alias read-line chez-get-line)

(alias read-string chez-get-string-n)

(alias read-u8 chez-get-u8)

(alias real? chez-real?)

(alias remainder chez-remainder)

(alias reverse chez-reverse)

(alias round chez-round)

(alias set! chez-set!)

(alias set-car! chez-set-car!)

(alias set-cdr! chez-set-cdr!)

(define (square x) (* x x))

(alias string chez-string)

(alias string->list chez-string->list)

(alias string-for-each chez-string-for-each)

(alias string->number chez-string->number)

(alias string->symbol chez-string->symbol)

(alias string->utf8 chez-string->utf8)

(define (string->vector s)
  (list->vector (string->list s)))

(alias string-append chez-string-append)

(alias string-copy chez-string-copy)

(alias string-copy! chez-string-copy!)

(alias string-fill! chez-string-fill!)

(alias string-length chez-string-length)

(define (string-map proc . strs)
  (list->string (apply map proc (map string->list strs))))

(alias string-ref chez-string-ref)

(alias string-set! chez-string-set!)

(alias string<=? chez-string<=?)

(alias string<? chez-string<?)

(alias string=? chez-string=?)

(alias string>=? chez-string>=?)


(alias string>? chez-string>?)

(alias string? chez-string?)

(alias substring chez-substring)

(alias symbol->string chez-symbol->string)

(alias symbol=? chez-symbol=?)

(alias symbol? chez-symbol?)

(define-syntax syntax-error
  (lambda (x)
    (chez-syntax-case x ()
                      ((_ message args ...)
                       (chez-syntax-violation 'syntax-error #'message '#'(args ...))))))

(alias syntax-rules chez-syntax-rules)

(alias textual-port? chez-textual-port?)

(alias truncate chez-truncate)

(alias truncate-quotient chez-quotient)

(alias truncate-remainder chez-remainder)

(define (truncate/ x y)
  (values (truncate-quotient x y)
          (truncate-remainder x y)))

(alias u8-ready? chez-input-port-ready?)

(alias unless chez-unless)

(alias unquote chez-unquote)

(alias unquote-splicing chez-unquote-splicing)

(alias utf8->string chez-utf8->string)

(alias values chez-values)

(alias vector chez-vector)

(alias vector->list chez-vector->list)

(define vector->string
  (lambda (v) (list->string (vector->list v))))

(alias vector-copy chez-vector-copy)

(define (vector-copy! source source-start dest dest-start count)
  (let loop ((i 0))
    (if (fx<? i count)
	(begin
	  (vector-set! dest (fx+ dest-start i)
		       (vector-ref source (fx+ source-start i)))
	  (loop (fx+ 1 i))))))


(alias vector-fill! chez-vector-fill!)

(alias vector-for-each chez-vector-for-each)

(alias vector-length chez-vector-length)

(alias vector-map chez-vector-map)

(alias vector-ref chez-vector-ref)

(alias vector-set! chez-vector-set!)

(alias vector? chez-vector?)

(alias when chez-when)

(alias with-exception-handler chez-with-exception-handler)

(alias write-bytevector chez-put-bytevector)

(alias write-char chez-write-char)

(alias write-string chez-put-string)

(alias write-u8 chez-put-u8)

(alias zero? chez-zero?)
