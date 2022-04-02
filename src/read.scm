(import (arew))


(define-record-type <source>
  (make-source filepath line column offset)
  source?
  (filepath source-filepath)
  (line source-line)
  (column source-column)
  (offset source-offset))

(define %source-null (make-source #f -1 -1 -1))

(define-record-type <object>
  (make-object source type-name representation object)
  object?
  (source object-source)
  (type-name object-type-name)
  (representation object-representation)
  (object object-object))

(define object-write
  (lambda (object)
    (write (object-representation object))))

;; xchar is pair of a char associated with an instance of <source>;
;; char and xchar are not available in the object language.
(define xchar->char car)
(define xchar-source cdr)

(define make-xchar
  (lambda (source char)
    (make-object source 'string (list->string (list char)) (list->string (list char)))))

;; an object-string is a string associated with an instance of <source>
(define make-object-string
  (lambda (xchars)
    (let ((source (xchar-source (car xchars)))
          (string (list->string (map xchar->char xchars))))
      (make-object (xchar-source (car xchars))
                   'string
                   string
                   string))))

(define (generator-char->xchar filepath generator)

  (define line 0)
  (define column -1)
  (define offset -1)

  (unless (and filepath (char=? (string-ref filepath 0) #//))
    (error 'object "filepath should be an absolute path to a file or #f"))

  (lambda ()
    (let ((char (generator)))
      (if (eof-object? char)
          (eof-object)
          (let ((xchar (cons char (make-source filepath line column offset))))
            (when (char=? char #\newline)
              (set! line (fx+ line 1))
              (set! column 0))
            (set! offset (fx+ offset 1))
            (set! column (fx+ column 1))
            xchar)))))

(define list->generator
  (lambda (objects)
    (if (null? objects)
        (eof-object)
        (let ((object (car objects)))
          (set! objects (cdr objects))
          object))))

(define read-object-string
  (lambda (xchars)
    (let loop () ;; grow the stack recursive loop
      (let ((xchar (xchars)))

        (when (eof-object? (xchar->char xchar))
          (error 'object-read-string "unexpected end of input"))

        (case (xchar->char xchar)
          (#\\ (let ((xchar (xchars)))
                 (case xchar
                   ;; escaped double quote, escaped slash, newline
                   ((#\" #\\ #\n #\b #\t) (cons xchar (loop)))
                   (else (error 'object-read-string "unsupported escape character" xchar)))))
          (#\" '())
          (else (cons xchar (loop))))))))

(define make-token-generator
  (lambda (xchars)

    (define whitespace?
      (lambda (xchar)
        (memv (xchar->char xchar) (list #\space #\newline #\backspace))))

    (define maybe-ignore-whitespace
      (lambda (xchars)
        (let ((xchar (xchars)))
          (if (eof-object? xchar)
              (eof-object)
              (if (whitespace? xchar)
                  (maybe-ignore-whitespace)
                  xchar)))))

    (define read-chunk
      (lambda (xchars next-xchar out)
        ;; A chunk may be a string, a number, otherwise it is a
        ;; symbol.  It ends on control characters.

        (define chunk-control-character?
          (lambda (xchar)
            (memv (xchar->char xchar) (list #\newline
                                            #\tab
                                            #\space
                                            #\(
                                            #\)))))

        (if (chunk-control-character? (xchar->char next-xchar))
            (values next-xchar (reverse (map xchar->char out)))
            (read-chunk xchars (xchars) (cons xchar out)))))

    (define read-token
      (lambda (xchars next-xchar)
        (lambda ()
          (if (eof-object? next-xchar)
              (error 'object-read-string "Unexpected end of xchars")
              (let ((xchar (maybe-ignore-whitespace next-xchar)))
                (case xchar
                  (#\( (set! continue (read-token xchars (xchars))) 'list-open)
                  (#\) (set! continue (read-token xchars (xchars))) 'list-end)
                  (else
                   (call-with-values (lambda () (read-chunk xchars (xchars) '()))
                     (lambda (chars next-xchar)
                       (let* ((string (make-object-string (cons xchar xchars)))
                              (maybe-number (string->number string)))
                         (if maybe-number
                             maybe-number
                             (string->symbol string)))))))))))))

    (define continue (read-token xchars (make-xchar %source-null #\space)))

    (lambda ()
      (continue))))

(define object-read-string
  (lambda (string)

    (define read
      (lambda (tokens)
        (let loop ((out '()))
          (let ((token (tokens)))
            (if (eof-object? token)
                out
                (case token
                  (list-open (read tokens))
                  (list-close (reverse out))
                  (else (loop (cons token out)))))))))

    (read (make-token-generator (generator-char->xchar #f (list->generator (string->list string)))))))
