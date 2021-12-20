#!chezscheme
(import (chezscheme))


(define AREW_DEBUG (getenv "AREW_DEBUG"))

(meta define read-string
      (lambda (p)
        (let loop ([x (read-char p)]
                   [out '()])
          (if (eof-object? x)
              (begin (close-input-port p)
                     (list->string (reverse out)))
              (loop (read-char p)
                    (cons x out))))))

(meta define (run/output command)
      (call-with-values (lambda () (open-process-ports command 'line (current-transcoder)))
        (lambda (stdin stdout stderr pid)
          (read-string stdout))))

(define-syntax include-filename-as-string
  (lambda (x)
    (syntax-case x ()
      [(k filename)
       (let ([fn (datum filename)])
         (with-syntax ([exp (read-string (open-input-file fn))])
                      #'exp))])))

(define (pk . args)
  (when AREW_DEBUG
    (display ";;; " (current-error-port))
    (write args (current-error-port))
    (newline (current-error-port)))
  (car (reverse args)))

(define (string-join strings)
  (let loop ((strings strings)
             (out '()))
    (if (null? strings)
        (apply string-append strings)
        (loop (cdr strings) (cons* " " string out)))))

(define arew.md (include-filename-as-string "arew/arew.md"))

(define arew.nfo (include-filename-as-string "arew/arew.nfo"))

;; Include git commit

(define-syntax include-git-describe
  (lambda (x)
    (syntax-case x ()
      [(k)
       (let ([fn (datum filename)])
         (with-syntax ([exp (run/output "git describe --tags --dirty")])
                      #'exp))])))

;; Include some files

(define compile.md (include-filename-as-string "arew/compile.md"))
(define scheme.h (include-filename-as-string* "../boot/~a/scheme.h"))
(define main.c (include-filename-as-string "arew/main.c"))
(define kernel.o (include-filename-as-bytevector* "../boot/~a/kernel.o"))
(define arew.boot (include-filename-as-bytevector "arew.boot"))

(define git-describe (let ((out (include-git-describe)))
                       (substring out 0 (fx- (string-length out) 1))))

(define (display-meta)
  (write `(tag ,git-describe))
  (newline)
  (write `(homepage "http://hyper.dev/"))
  (newline))

(define (display-usage usage)
  (display arew.nfo)
  (newline)
  (display usage)
  (display-meta))

(define arew-usage
  (lambda ()
    (display-usage arew.md)))

(define (string-prefix? string prefix)
  (if (= (string-length string) (string-length prefix))
      #f
      (let loop ((index 0))
        (if (fx=? index (string-length prefix))
            #t
            (if (not (char=? (string-ref prefix index)
                             (string-ref string index)))
                #f
                (loop (fx+ index 1)))))))

(define (string-find string char)
  (let loop ((index 0))
    (if (fx=? (string-length string) index)
        #f
        (let ((other (string-ref string index)))
          (if (char=? char other)
              index
              (loop (fx+ index 1)))))))

(define (command-line-parse arguments)
  (let loop ((arguments arguments)
             (keywords '())
             (standalone '())
             (extra '()))
    (if (null? arguments)
        (list (cons 'keywords keywords)
              (cons 'standalone standalone)
              (cons 'extra extra))
        (let ((head (car arguments)))
          (cond
           ((string=? head "--")
            (loop '()
                  keywords
                  standalone
                  (cdr arguments)))
           ((string-prefix? head "--")
            (loop (cdr arguments)
                  (cons (let ((index (string-find head #\=)))
                          (if index
                              (cons (string->symbol (substring head 2 index))
                                    (substring head
                                               (fx+ index 1)
                                               (string-length head)))
                              (cons (string->symbol
                                     (substring head 2 (string-length head)))
                                    #t)))
                        keywords)
                  standalone
                  extra))
           (else (loop (cdr arguments) keywords (cons head standalone) extra)))))))

(define (make-filepath filepath)
  (cond
   ((string=? filepath ".") (current-directory))
   ((char=? (string-ref filepath 0) #\/)
    filepath)
   (else (string-append (current-directory) "/" filepath))))

(define (ref alist key default)
  (if (null? alist)
      default
      (if (eq? (caar alist) key)
          (cdar alist)
          (ref (cdr alist) key default))))

(define stdlib (load-shared-object #f))

(define mkdtemp
  (foreign-procedure "mkdtemp" (string) string))

(define (make-temporary-directory prefix)
  (let ((input (string-append prefix "-XXXXXX")))
    (mkdtemp input)))

;; Macros to include files

(meta define (machine-type*)
      (symbol->string (machine-type)))

(define-syntax include-filename-as-string*
  (lambda (x)
    (syntax-case x ()
      [(k filename)
       (let ([fn (format #f (datum filename) (machine-type*))])
         (with-syntax ([exp (read-string (open-input-file fn))])
                      #'exp))])))

(define-syntax include-filename-as-bytevector
  (lambda (x)
    (syntax-case x ()
      [(k filename)
       (let* ([fn (datum filename)])
         (with-syntax ([exp (get-bytevector-all (open-file-input-port fn))])
                      #'exp))])))

(define-syntax include-filename-as-bytevector*
  (lambda (x)
    (syntax-case x ()
      [(k filename)
       (let* ([fn (format #f (datum filename) (machine-type*))])
         (with-syntax ([exp (get-bytevector-all (open-file-input-port fn))])
                      #'exp))])))

(define (system* command)
  (unless (fxzero? (system command))
    (error 'arew "Command failed" command)))

(define (parse-and-validate arguments program.scm? a.out?)
  (define errors '())
  (define dev? #f)
  (define optimize-level* 0)
  (define extensions '())
  (define directories '())
  (define program.scm #f)
  (define a.out #f)
  (define extra (ref arguments 'extra '()))

  (define (parse-and-validate-keywords! keywords)
    (let loop ((keywords keywords))
      (unless (null? keywords)
        (case (caar keywords)
          ((dev) (set! dev? #t))
          ((optimize-level)
           (let ((maybe-number (string->number (cdar keywords))))
             (if (and maybe-number (<= 0 maybe-number 3))
                 (set! optimize-level* maybe-number)
                 (set! errors (cons (format #f "Optimization level must be between 0 and 3, not ~a"
                                            (cdar keywords))
                                    errors)))))
          (else (set! errors (cons (format #f "Unknown keyword: ~a" (caar keywords)) errors))))
        (loop (cdr keywords)))))

  (define (parse-and-validate-extensions-or-directories! strings)
    (let loop ((strings strings))
      (unless (null? strings)
        (let ((head (car strings)))
          (cond
           ((file-directory? head)
            (set! directories (cons head directories)))
           ((char=? (string-ref head 0) #\.)
            (set! extensions (cons head extensions)))
           (else (set! errors (cons (format #f "Does not look like a directory or extension: ~a"
                                            head)
                                    errors))))
          (loop (cdr strings))))))

  (define (parse-and-validate-standalone! standalone)
    (define a.out* #f)
    (define program.scm* #f)

    (when program.scm?
      (if a.out?
          (begin
            (set! a.out* (and (not (null? standalone)) (car standalone)))
            (set! program.scm* (and (not (null? standalone))
                                    (not (null? (cdr standalone)))
                                    (cadr standalone)))
            (set! standalone (cddr standalone)))
          (begin
            (set! program.scm* (and (not (null? standalone)) (car standalone)))
            (set! standalone (cdr standalone))))

      (when a.out?
        (if a.out*
            (if (file-exists? a.out*)
                (set! errors (cons (format #f "File does already exist: ~a" a.out*)
                                   errors))
                (set! a.out a.out*))
            (set! errors (cons "No a.out file specified" errors))))

      (if program.scm*
          (if (file-exists? program.scm*)
              (set! program.scm program.scm*)
              (set! errors (cons (format #f "File does not exist: ~a" program.scm*)
                                 errors)))
          (set! errors (cons "No program.scm specified" errors))))

    (unless (or (null? standalone)
                (null? (cdr standalone))
                (null? (cddr standalone)))
      (parse-and-validate-extensions-or-directories! standalone)))

  (parse-and-validate-keywords! (ref arguments 'keywords '()))
  (parse-and-validate-standalone! (ref arguments 'standalone '()))

  (if (null? errors)
      (if a.out?
          (values dev? optimize-level* extensions directories program.scm a.out extra)
          (values dev? optimize-level* extensions directories program.scm extra))
      (begin
        (display "* Ooops :|")
        (newline)
        (for-each (lambda (x) (display "** ") (display x) (newline)) errors)
        (exit 1))))

(define arew-compile
  (lambda (arguments)

    (define arguments* (command-line-parse arguments))

    (define program.boot.scm
      '(let ([program-name
              (foreign-procedure "program_name" () string)])
         (scheme-program
          (lambda (fn . fns)
            (command-line (cons (program-name) fns))
            (command-line-arguments fns)
            (load-program fn)))))

    (define ignore (when (null? arguments)
                     (display compile.md)
                     (exit 1)))

    (define-values (dev? optimize-level* extensions directories program.scm a.out extra)
      (parse-and-validate (command-line-parse arguments) #t #t))

    (define temporary-directory (pk 'temporary-directory
                                    (make-temporary-directory "/tmp/binjar-exe")))

    (unless (null? directories)
      (library-directories directories)
      (source-directories directories))

    (when optimize-level*
      (optimize-level optimize-level*)

      (unless (null? extensions)
        (library-extensions extensions))

      (when dev?
        (generate-allocation-counts #t)
        (generate-instruction-counts #t))

      (compile-imported-libraries #t)
      (generate-wpo-files #t)

      ;; create program.boot from program.boot.scm

      (call-with-port (open-file-output-port (string-append temporary-directory "/arew.boot"))
        (lambda (port)
          (put-bytevector port arew.boot)))

      (call-with-output-file (string-append temporary-directory "/program.boot.scm")
        (lambda (port)
          (write program.boot.scm port)))

      (make-boot-file (string-append temporary-directory "/program.boot")
                      '()
                      (string-append temporary-directory "/arew.boot")
                      (string-append temporary-directory "/program.boot.scm"))

      ;; create main.c

      (system* (format #f "cp ~a ~a/program.scm" program.scm temporary-directory))

      (compile-program (string-append temporary-directory "/program.scm"))

      (compile-whole-program (string-append temporary-directory "/program.wpo")
                             (string-append temporary-directory "/program.chez")
                             #t)

      (call-with-output-file (string-append temporary-directory "/main.c")
        (lambda (port)
          (let ((boot (bytevector->u8-list
                       (get-bytevector-all
                        (open-file-input-port
                         (string-append temporary-directory "/program.boot")))))
                (program (bytevector->u8-list
                          (get-bytevector-all
                           (open-file-input-port
                            (string-append temporary-directory "/program.chez"))))))
            (display (format #f main.c boot program) port))))

      ;; create the output executable

      (call-with-output-file (string-append temporary-directory "/scheme.h")
        (lambda (port)
          (display scheme.h port)))

      (call-with-port (open-file-output-port (string-append temporary-directory "/kernel.o"))
        (lambda (port)
          (put-bytevector port kernel.o)))

      (system* (format #f "cc -m64 -ldl -lz -llz4 -lm -luuid -lpthread ~a ~a/main.c ~a/kernel.o -o ~a"
                       (string-join extra)
                       temporary-directory
                       temporary-directory
                       a.out)))))

(define (arew-exec arguments)

  (define exec.md (include-filename-as-string "arew/exec.md"))

  (define-values (dev? optimize-level* extensions directories program.scm extra)
    (parse-and-validate (command-line-parse arguments) #t #f))

  (unless (null? directories)
    (library-directories directories)
    (source-directories directories))

  (when optimize-level*
    (optimize-level optimize-level*))

  (unless (null? extensions)
    (library-extensions extensions))

  (when dev?
    (generate-allocation-counts #t)
    (generate-instruction-counts #t))

  (compile-imported-libraries #t)
  (generate-wpo-files #t)

  (command-line (cons program.scm extra))

  (load-program program.scm))

(define (arew-repl arguments)

  (define-values (dev? optimize-level* extensions directories program.scm extra)
    (parse-and-validate (command-line-parse arguments) #f #f))

  (unless (null? directories)
    (library-directories directories)
    (source-directories directories))

  (when optimize-level*
    (optimize-level optimize-level*))

  (unless (null? extensions)
    (library-extensions extensions))

  (when dev?
    (generate-allocation-counts #t)
    (generate-instruction-counts #t))

  (let loop ()
    (display "\033[32m#;arew #;\033[m ")
    (let ((expr (read)))
      (unless (eof-object? expr)
        (call-with-values
            (lambda ()
              (guard (ex
                      ((condition? ex)
                       (display "\033[31m;; raised condition:\033[m ")
                       (display-condition ex)
                       (newline))
                      (else
                       (display "\033[31m;; raised:\033[m ")
                       (write ex)
                       (newline)))
                (eval expr)))
          (lambda args
            (unless (null? args)
              (for-each (lambda (x)
                          (unless (eq? x (void))
                            (display "\033[34m#;\033[m ")
                            (write x)
                            (newline)))
                        args))
            (loop)))))))

(self-evaluating-vectors #t)

(when (null? (cdr (command-line)))
  (arew-usage)
  (exit #f))

(case (string->symbol (cadr (command-line)))
  ((compile) (arew-compile (cddr (command-line))))
  ((exec) (arew-exec (cddr (command-line))))
  ((repl) (arew-repl (cddr (command-line))))
  (else (arew-usage) (exit #f)))
