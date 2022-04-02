(library (ruse read)
  (export ruse-read)
  (import (arew))

  (define-record-type <source>
    (make-source filepath line column offset)
    source?
    (filepath source-filepath)
    (line source-line)
    (column source-column)
    (offset source-offset))

  (define-record-type <object>
    (make-object source data)
    object?
    (source object-source)
    (data object-data))

  ;; xchar is pair of a char associated with an instance of <source>
  (define xchar->char car)
  (define xchar-source cdr)

  ;; xstring is pair of a string associated with an instance of <source>
  (define list->xstring
    (lambda (xchars)
      (let ((source (xchar-source (car xchars))))
        (cons (list->string (map xchar->char xchars) source)))))

  (define (generator-char->xchar filepath generator)

    (define line 0)
    (define column 0)
    (define offset 0)++

    (lambda ()
      (let ((char (generator)))
        (if (eof-object? char)
            (eof-object)
            (let ((xchar (cons char (make-source line column offset))))
              (when (char=? char #\newline)
                (set! line (fx+ line 1))
                (set! column 0))
              (set! offset (fx+ offset 1))
              xchar)))))

  (define ruse-tokens
    (lambda (generator)

      (define char-whitespace?
        (lambda (xchar)
          ;; XXX: char-set:whitespace does not include invisible chars
          (char-set-contains? char-set:whitespace (xchar->char char))))

      (define maybe-ignore-whitespace
        (lambda ()
          (let ((xchar (generator)))
            (if (eof-object? (xchar->char char))
                (eof-object)
                (if (char-whitespace? xchar)
                    (maybe-ignore-whitespace)
                    xchar)))))

      (define read-string%
        (lambda ()
          (let ruse ((xchar (generator)))
            (when (eof-object? (xchar->char char))
              (error 'ruse-read "unexpected end of input"))

            (case (xchar->char xchar)
              (#\\ (let ((xchar (generator)))
                     (case xchar
                       ;; escaped double quote, escaped slash, newline
                       ((#\" #\\ #\n #\b #\t) (cons xchar (ruse (generator))))
                       (else (error 'ruse-read "unsupported escape character" xchar)))))
              (#\" '())
              (else (cons xchar (ruse (generator))))))))

      (define read-string
        (lambda ()
          (list->xstring (read-string%))))

      (define read-chunk
        (lambda ()
          ;; A chunk may be a number, if not it is a symbol.  It ends
          ;; on control characters.

          ;; The procedure called `ruse` should recursively return the
          ;; last char, but since it is built to avoid a call to
          ;; reverse, without tail calls, and because I am too lazy,
          ;; let's use the following variable to store the last seen
          ;; char.
          (define xchar* #f)

          (define char-set-chunk-control-character
            (list->char-set (list #\newline
                                  #\tab
                                  #\space
                                  #\(
                                  #\))))

          (define chunk-control-character?
            (lambda (char)
              (char-set-contains? char-set-chunk-control-character char)))

          (define ruse
            (lambda (char)
              (set! char* char)
              (if (chunk-control-character? char)
                  '()
                  (cons char (ruse (generator))))))

          (values (ruse (generator)) char*)))

      ;; The algorithm requires to know what is the next char, hence
      ;; store the last read char here. I do not know how to do it
      ;; otherwise. NB: `gcons` slow things down [citation neeed].
      (define xchar* (maybe-ignore-whitespace))

      (define continue
        (lambda ()
          (if (eof-object? char*)
              (eof-object)
              (case char*
                (#\( (set! char* (maybe-ignore-whitespace)) #\()
                (#\) (set! char* (maybe-ignore-whitespace)) #\))
                (#\" (let ((out (read-string)))
                       (set! char* (maybe-ignore-whitespace))
                       out))
                (else
                 (call-with-values (lambda () (read-chunk))
                   (lambda (chars next)
                     (let ((first char*))
                       (if (char-whitespace? next)
                           (set! char* (maybe-ignore-whitespace))
                           (set! char* next))
                       (let* ((string (list->string (cons first chars)))
                              (maybe-number (string->number string)))
                         (if maybe-number
                             maybe-number
                             (string->symbol string)))))))))))


      continue))

  (define ruse-read
    (lambda (generator)
      (define do
        (lambda (tokens)
          (let ((token (tokens)))
            (if (char=? token)
                ;; a list delimiter
                (let loop ((out '()))
                  (let ((obj (do tokens)))
                    (if (and (char? obj) (char=? obj #\)))
                        (reverse out)
                        (loop (cons obj out)))))
                ;; a symbol, a string, or a number.
                token))))

      (do (ruse-tokens generator)))))
