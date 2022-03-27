#!chezscheme
(import (chezscheme))
(import (only (arew) list-index char-set-contains? char-set:letter+digit))


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

;; Macros to include files

(meta define (machine-type*)
      (symbol->string (machine-type)))

(define-syntax include-filename-as-string*
  (lambda (x)
    (syntax-case x ()
      [(k filename)
       (let ([fn (format #f (datum filename) (machine-type*) (machine-type*))])
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
       (let* ([fn (format #f (datum filename) (machine-type*) (machine-type*))])
         (with-syntax ([exp (get-bytevector-all (open-file-input-port fn))])
                      #'exp))])))

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
    (newline (current-error-port))
    (flush-output-port (current-error-port)))
  (car (reverse args)))

(define (string-join strings)
  (let loop ((strings strings)
             (out '()))
    (if (null? strings)
        (apply string-append strings)
        (loop (cdr strings) (cons* " " string out)))))

(define arew.md (include-filename-as-string "arew.md"))

(define arew.nfo (include-filename-as-string "arew.nfo"))

;; Include git commit

(define-syntax include-git-describe
  (lambda (x)
    (syntax-case x ()
      [(k)
       (let ([fn (datum filename)])
         (with-syntax ([exp (run/output "git describe --tags --dirty")])
                      #'exp))])))

;; Include some files

(define scheme.h (include-filename-as-string* "../racket-chez/~a/boot/~a/scheme.h"))
(define main.c (include-filename-as-string "main.c"))
(define kernel.o (include-filename-as-bytevector* "../racket-chez/~a/boot/~a/kernel.o"))
(define arew.boot (include-filename-as-bytevector "arew.boot"))
(define arew.concatenated.so (include-filename-as-bytevector "arew.concatenated.so"))

(define git-describe (let ((out (include-git-describe)))
                       (if (= (string-length out) 0)
                           "dev"
                           (substring out 0 (fx- (string-length out) 1)))))

(define make-accumulator
  (lambda ()
    (let ((out '()))
      (lambda (object)
        (if (eof-object? object)
            out
            (set! out (cons object out)))))))

(define (display-meta)
  (write `(tag ,git-describe))
  (newline)
  (write `(homepage "http://hyper.dev/"))
  (newline))

(define (display-usage usage)
  (display arew.nfo)
  (newline)
  (display usage)
  (newline)
  (display-meta))

(define arew-usage
  (lambda ()
    (display-usage arew.md)))

(define (command-line-parse arguments)

  ;; Given the following ARGUMENTS:
  ;;
  ;;   '("--foo=bar" "--qux" "-vvv" "name" "another" "--" "olive" "extra")
  ;;
  ;; command-line-parse returns the following values:
  ;;
  ;;   (values '((--foo . "bar") (--qux . #t) (-vvv . #t)) '("name" "other") '("olive" "extra"))
  ;;
  ;; Standalone arguments e.g. "name" and "other" and extra arguments
  ;; e.g. "olive" and "extra" are returned in the same order as
  ;; found in ARGUMENTS.

  (define keyword/value
    (lambda (string)
      (define index (list-index (lambda (x) (char=? x #\=)) (string->list string)))

      (if (not index)
          (values (string->symbol string) #t)
          (values (string->symbol (substring string 0 index)) (substring string (fx+ index 1) (string-length string))))))

  (let loop ((arguments arguments)
             (keywords '())
             (standalone '()))
    (if (null? arguments)
        (values keywords (reverse standalone) '())
        (let ((head (car arguments)))
          (cond
           ((string=? head "--")
            (values keywords (reverse standalone) (cdr arguments)))
           ((char=? (string-ref head 0) #\-)
            (call-with-values (lambda () (keyword/value head))
              (lambda (key value)
                (loop (cdr arguments) (cons (cons key value) keywords) standalone))))
           (else (loop (cdr arguments) keywords (cons head standalone))))))))

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
      (display-errors-then-exit errors)))

(define (display-errors-then-exit errors)
  (display "* Ooops :|")
  (newline)
  (for-each (lambda (x) (display "** ") (display x) (newline)) errors)
  (exit 1))

(define (guess string)
  (cond
   ((file-directory? string) (values 'directory (make-filepath string)))
   ((and (char=? (string-ref string 0) #\.)
         (not (file-exists? string))
         (char-set-contains? char-set:letter+digit (string-ref string 1)))
    ;; the first char is a dot, the file does not exists, and the
    ;; second char is a letter or a number; hence it is prolly an
    ;; extension... might break in some case.
    (values 'extension string))
   ((file-exists? string)
    (values 'file (make-filepath string)))
   (else (values 'unknown (make-filepath string)))))

(define arew-compile
  (lambda (arguments)

    (define program.boot.scm
      '(let ([program-name
              (foreign-procedure "program_name" () string)])
         (scheme-program
          (lambda (fn . fns)
            (command-line (cons (program-name) fns))
            (command-line-arguments fns)
            (load-program fn)))))

    ;; parse ARGUMENTS, and set the following variables:

    (define extensions '())
    (define directories '())
    (define program.scm #f)
    (define a.out #f)
    (define dev? #f)
    (define optimize-level* 0)
    (define extra '())

    (define errors (make-accumulator))

    (define massage-standalone!
      (lambda (standalone)
        (unless (null? standalone)
          (call-with-values (lambda () (guess (car standalone)))
            (lambda (type string*)
              (case type
                (directory (set! directories (cons string* directories)))
                (extension (set! extensions (cons string* extensions)))
                (file (if program.scm
                          (errors (format #f "You can compile only one file at a time, maybe remove: ~a" (car standalone)))
                          (set! program.scm string*)))
                (unknown (if a.out
                             (errors (format #f "You provided more than one file that does not exists, maybe remove: ~a" (car standalone)))
                             (set! a.out string*))))))
          (massage-standalone! (cdr standalone)))))

    (define massage-keywords!
      (lambda (keywords)
        (unless (null? keywords)
          (let ((keyword (car keywords)))
            (cond
             ((and (eq? (car keyword) '--dev) (not (string? (cdr keyword))))
              (set! dev? #t))
             ((and (eq? (car keyword) '--optimize-level)
                   (string->number (cdr keyword))
                   (<= 0 (string->number (cdr keyword) 3)))
              (set! optimize-level* (string->number (cdr keyword))))
             (else (errors (format #f "Dubious keyword: ~a" (car keyword))))))
          (massage-keywords! (cdr keywords)))))

    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords standalone extra*)
        (massage-standalone! standalone)
        (massage-keywords! keywords)
        (set! extra extra*)))

    (unless program.scm
      (errors "You need to provide one and only program file to compile."))

    (unless a.out
      (errors "You need to provide one and only one target file that will be created, and stuffed with bits."))

    (let ((errors (errors (eof-object))))
      (unless (null? errors)
        (display-errors-then-exit errors)))

    ;; All is well, proceed with compilation.
    (let ((temporary-directory (make-temporary-directory "/tmp/arew-compile")))

      (unless (null? directories)
        (library-directories directories)
        (source-directories directories))

      (optimize-level optimize-level*)

      (unless (null? extensions)
        (library-extensions extensions))

      (when dev?
        ;; TODO: Link to documentation to help me remember what the
        ;; following does.
        (generate-allocation-counts #t)
        (generate-instruction-counts #t))

      (compile-imported-libraries #t)
      (generate-wpo-files #t)

      ;; create program.boot

      (call-with-port (open-file-output-port (string-append temporary-directory "/arew.boot"))
        (lambda (port)
          (put-bytevector port arew.boot)))

      (call-with-port (open-file-output-port (string-append temporary-directory "/arew.so"))
        (lambda (port)
          (put-bytevector port arew.concatenated.so)))

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

      (system* (format #f "cc -m64 ~a ~a/main.c ~a/kernel.o -o ~a -ldl -lz -llz4 -lm -luuid -lpthread"
                       (string-join extra)
                       temporary-directory
                       temporary-directory
                       a.out)))))

(define (arew-exec arguments)

  ;; parse ARGUMENTS, and set the following variables:

  (define extensions '())
  (define directories '())
  (define dev? #f)
  (define optimize-level* 0)
  (define extra '())
  (define program.scm #f)

  (define errors (make-accumulator))

  (define massage-standalone!
    (lambda (standalone)
      (unless (null? standalone)
        (call-with-values (lambda () (guess (car standalone)))
          (lambda (type string*)
            (case type
              (directory (set! directories (cons string* directories)))
              (extension (set! extensions (cons string* extensions)))
              (file (if program.scm
                        (errors (format #f "Already registred a file to execute, maybe remove: ~a" (car standalone)))
                        (set! program.scm string*)))
              (unknown (errors (format #f "Directory does not exists: ~a" (car standalone)))))))
        (massage-standalone! (cdr standalone)))))

  (define massage-keywords!
    (lambda (keywords)
      (unless (null? keywords)
        (let ((keyword (car keywords)))
          (cond
           ((and (eq? (car keyword) '--dev) (not (string? (cdr keyword))))
            (set! dev? #t))
           ((and (eq? (car keyword) '--optimize-level)
                 (string->number (cdr keyword))
                 (<= 0 (string->number (cdr keyword) 3)))
            (set! optimize-level* (string->number (cdr keyword))))
           (else (errors (format #f "Dubious keyword: ~a" (car keyword))))))
        (massage-keywords! (cdr keywords)))))

  (call-with-values (lambda () (command-line-parse arguments))
    (lambda (keywords standalone extra*)
      (massage-standalone! standalone)
      (massage-keywords! keywords)
      (set! extra extra*)))

  (let ((errors (errors (eof-object))))
    (unless (null? errors)
      (display-errors-then-exit errors)))

  (unless (null? directories)
    (library-directories directories)
    (source-directories directories))

  (when optimize-level*
    (optimize-level optimize-level*))

  (unless (null? extensions)
    (library-extensions extensions))

  (when dev?
    (compile-profile 'source)
    (generate-allocation-counts #t)
    (generate-instruction-counts #t))

  (command-line (cons program.scm extra))

  (load-program program.scm)

  (when dev?
    (profile-dump-html)))

(define (arew-repl arguments)

  ;; parse ARGUMENTS, and set the following variables:

  (define extensions '())
  (define directories '())
  (define dev? #f)
  (define optimize-level* 0)
  (define extra '())

  (define errors (make-accumulator))

  (define massage-standalone!
    (lambda (standalone)
      (unless (null? standalone)
        (call-with-values (lambda () (guess (car standalone)))
          (lambda (type string*)
            (case type
              (directory (set! directories (cons string* directories)))
              (extension (set! extensions (cons string* extensions)))
              (file (errors (format #f "Does not support files: ~a" (car standalone))))
              (unknown (errors (format #f "Directory does not exists: ~a" (car standalone)))))))
        (massage-standalone! (cdr standalone)))))

  (define massage-keywords!
    (lambda (keywords)
      (unless (null? keywords)
        (let ((keyword (car keywords)))
          (cond
           ((and (eq? (car keyword) '--dev) (not (string? (cdr keyword))))
            (set! dev? #t))
           ((and (eq? (car keyword) '--optimize-level)
                 (string->number (cdr keyword))
                 (<= 0 (string->number (cdr keyword) 3)))
            (set! optimize-level* (string->number (cdr keyword))))
           (else (errors (format #f "Dubious keyword: ~a" (car keyword))))))
        (massage-keywords! (cdr keywords)))))

  (call-with-values (lambda () (command-line-parse arguments))
    (lambda (keywords standalone extra*)
      (massage-standalone! standalone)
      (massage-keywords! keywords)
      (set! extra extra*)))

  (unless (null? extra)
    (errors (format #f "No support for extra arguments: ~a" extra)))

  (let ((errors (errors (eof-object))))
    (unless (null? errors)
      (display-errors-then-exit errors)))

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

(define file->library
  (lambda (filename)
    (define sexp (call-with-input-file filename read))
    (define library (cadr sexp))

    (cons library (eval `(library-exports ',library) (environment '(chezscheme) library)))))

(define append-map
  (lambda (proc objs)
    (apply append (map proc objs))))

(define build-check-program
  (lambda (files)
    (define library (file->library (car files)))

    `((import (chezscheme) ,(car library))

      (let loopy ((thunks (list ,@(cdr library))))
        (unless (null? thunks)
          (format #t "* Checking `~a`:\n\n" (car thunks))
          ((car thunks))
          (newline)
          (loopy (cdr thunks)))))))

(define arew-check
  (lambda (arguments)
    (define errors (make-accumulator))

    (define fail-fast? #f)
    (define extensions '())
    (define directories '())
    (define files '())

    (define massage-keywords!
      (lambda (keywords)
        (unless (null? keywords)
          (case (caar keywords)
            (--fail-fast (set! fail-fast? #t))
            (else (errors (format #f "Unkown keywords: ~a" (caar keywords))))))))

    (define massage-standalone!
      (lambda (standalone)
        (unless (null? standalone)
          (call-with-values (lambda () (guess (car standalone)))
            (lambda (type string*)
              (case type
                (directory (set! directories (cons string* directories)))
                (extension (set! extensions (cons string* extensions)))
                (file (set! files (cons string* files)))
                (unknown (errors (format #f "Unknown file: ~a" (car standalone)))))))
          (massage-standalone! (cdr standalone)))))

    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords standalone extra)
        (massage-keywords! keywords)
        (massage-standalone! standalone)))

    (compile-profile 'source)

    ;; TODO: support discovery
    (when (null? files)
      (errors "Discovery is not supported yet, you need to provide one file with tests"))

    (unless (and (not (null? files)) (null? (cdr files)))
      (errors "At this time, only one file can be checked at once"))

    (let ((errors (errors (eof-object))))
      (unless (null? errors)
        (display-errors-then-exit errors)))

    (unless (null? directories)
      (library-directories directories)
      (source-directories directories))

    (unless (null? extensions)
      (library-extensions extensions))

    (let* ((temporary-directory (make-temporary-directory "/tmp/arew-check"))
           (check (string-append temporary-directory "/check.scm"))
           (program (build-check-program files)))
      (call-with-output-file check
        (lambda (port)
          (let loop ((program program))
            (unless (null? program)
              (pretty-print (car program) port)
              (loop (cdr program))))))
      (current-directory temporary-directory)
      (arew-exec (list "--dev" "." check))
      (format (current-output-port) "* profile can be found at: ~a/profile.html\n" temporary-directory))))

(self-evaluating-vectors #t)

(when (null? (cdr (command-line)))
  (arew-usage)
  (exit #f))

(case (string->symbol (cadr (command-line)))
  (check (arew-check (cddr (command-line))))
  (compile (arew-compile (cddr (command-line))))
  (exec (arew-exec (cddr (command-line))))
  (repl (arew-repl (cddr (command-line))))
  (else (arew-usage) (exit #f)))
