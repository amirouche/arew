(library (arew)

  (export
   match
   ;; cond-expand ;; TODO
   ;; delay-force ;; TODO
   ;; features ;; TODO
   ;; floor-quotient ;; TODO
   ;; floor-remainder ;; TODO
   ;; floor/ ;; TODO
   ;; get-environment-variables ;; TODO
   ;; get-output-bytevector ;; TODO
   ;; include-ci ;; TODO
   ;; make-promise ;; TODO
   ;; open-input-bytevector ;; TODO
   ;; open-output-bytevector ;; TODO
   ;; promise? ;; TODO
   ;; read-bytevector! ;; TODO
   ;; vector-append ;; TODO
   <
   <=
   =
   =>
   >
   >=
   _
   abs
   acos
   and
   angle
   append
   apply
   asin
   assoc
   assq
   assv
   atan
   begin
   binary-port?
   boolean=?
   boolean?
   bytevector
   bytevector-copy
   bytevector-copy!
   bytevector-length
   bytevector-u8-ref
   bytevector-u8-set!
   bytevector?
   caaaar
   caaadr
   caaar
   caadar
   caaddr
   caadr
   caar
   cadaar
   cadadr
   cadar
   caddar
   cadddr
   caddr
   cadr
   call-with-current-continuation
   call-with-input-file
   call-with-output-file
   call-with-port
   call-with-values
   call/cc
   car
   case
   cdaaar
   cdaadr
   cdaar
   cdadar
   cdaddr
   cdadr
   cdar
   cddaar
   cddadr
   cddar
   cdddar
   cddddr
   cdddr
   cddr
   cdr
   ceiling
   char->integer
   char-alphabetic?
   char-ci<=?
   char-ci<?
   char-ci=?
   char-ci>=?
   char-ci>?
   char-downcase
   char-foldcase
   char-lower-case?
   char-numeric?
   char-ready?
   char-upcase
   char-upper-case?
   char-whitespace?
   char<=?
   char<?
   char=?
   char>=?
   char>?
   char?
   close-input-port
   close-output-port
   close-port
   command-line
   complex?
   cond
   cons
   cos
   current-error-port
   current-input-port
   current-output-port
   define
   define-record-type
   define-syntax
   define-values
   delay
   delete-file
   denominator
   digit-value
   display
   do
   dynamic-wind
   else
   emergency-exit
   environment
   eof-object
   eof-object?
   eq?
   equal?
   eqv?
   error
   error-object-irritants
   error-object-message
   error-object?
   eval
   even?
   exact
   exact-integer-sqrt
   exact-integer?
   exact?
   exit
   exp
   expt
   file-error?
   file-exists?
   finite?
   floor
   flush-output-port
   for-each
   force
   gcd
   get-environment-variable
   get-output-string
   guard
   if
   imag-part
   include
   inexact
   inexact?
   infinite?
   input-port-open?
   input-port?
   integer->char
   integer?
   lambda
   lcm
   length
   let
   let*
   let*-values
   let-syntax
   let-values
   letrec
   letrec*
   letrec-syntax
   list
   list->string
   list->vector
   list-copy
   list-ref
   list-set!
   list-tail
   list?
   log
   magnitude
   make-bytevector
   make-list
   make-parameter
   make-polar
   make-rectangular
   make-string
   make-vector
   map
   max
   member
   memq
   memv
   min
   modulo
   nan?
   negative?
   newline
   not
   null?
   number->string
   number?
   numerator
   odd?
   open-binary-input-file open-binary-output-file
   open-input-file open-output-file with-input-from-file
   open-input-string
   open-output-string
   or
   output-port-open?
   output-port?
   pair?
   parameterize
   peek-char
   peek-u8
   port?
   positive?
   procedure?
   quasiquote
   quote
   quotient
   raise
   raise-continuable
   rational?
   rationalize
   read-bytevector
   read-char
   read-error?
   read-line
   read-string
   read-u8
   real-part
   real?
   remainder
   reverse
   round
   set!
   set-car!
   set-cdr!
   sin
   sqrt
   square
   string
   string->list
   string->number
   string->symbol
   string->utf8
   string->vector
   string-append
   string-ci<=?
   string-ci<?
   string-ci=?
   string-ci>=?
   string-ci>?
   string-copy
   string-copy!
   string-downcase
   string-fill!
   string-foldcase
   string-for-each
   string-length
   string-map
   string-ref
   string-set!
   string-upcase
   string<=?
   string<?
   string=?
   string>=?
   string>?
   string?
   substring
   symbol->string
   symbol=?
   symbol?
   syntax-error
   syntax-rules
   tan
   textual-port?
   truncate
   truncate-quotient
   truncate-remainder
   truncate/
   u8-ready?
   unless
   unquote
   unquote-splicing
   utf8->string
   values
   vector
   vector->list
   vector->string
   vector-copy
   vector-copy!
   vector-fill!
   vector-for-each
   vector-length
   vector-map
   vector-ref
   vector-set!
   vector?
   when
   with-exception-handler
   with-output-to-file
   write
   write-bytevector
   write-char
   write-shared
   write-simple
   write-string
   write-u8
   zero?
   *
   +
   -
   ->char-set
   ...
   /
   ;;
   ;;
   ;; bytevector-append ;; TODO
   <=?
   <?
   =?
   >=?
   >?
   alist->hash-table
   alist-cons
   alist-copy
   alist-delete
   alist-delete!
   any
   append
   append!
   append-map
   append-map!
   append-reverse
   append-reverse!
   assoc
   assq
   assv
   boolean-hash
   break
   break!
   bytevector=?
   car+cdr
   char-ci-hash
   char-hash
   char-set
   char-set->list
   char-set->string
   char-set-adjoin
   char-set-adjoin!
   char-set-any
   char-set-complement
   char-set-complement!
   char-set-contains?
   char-set-copy
   char-set-count
   char-set-cursor
   char-set-cursor-next
   char-set-delete
   char-set-delete!
   char-set-diff+intersection
   char-set-diff+intersection!
   char-set-difference
   char-set-difference!
   char-set-every
   char-set-filter
   char-set-filter!
   char-set-fold
   char-set-for-each
   char-set-hash
   char-set-intersection
   char-set-intersection!
   char-set-map
   char-set-ref
   char-set-size
   char-set-unfold
   char-set-unfold!
   char-set-union
   char-set-union!
   char-set-xor
   char-set-xor!
   char-set:ascii
   char-set:blank
   char-set:digit
   char-set:empty
   char-set:full
   char-set:graphic
   char-set:hex-digit
   char-set:iso-control
   char-set:letter
   char-set:letter+digit
   char-set:lower-case
   char-set:printing
   char-set:punctuation
   char-set:symbol
   char-set:title-case
   char-set:upper-case
   char-set:whitespace
   char-set<=
   char-set=
   char-set?
   circular-list
   circular-list?
   comparator-check-type
   comparator-equality-predicate
   comparator-hash
   comparator-hash-function
   comparator-hashable?
   comparator-if<=>
   comparator-ordered?
   comparator-ordering-predicate
   comparator-register-default!
   comparator-test-type
   comparator-type-test-predicate
   comparator?
   concatenate
   concatenate!
   cons
   cons*
   count
   default-hash
   delete
   delete!
   delete-duplicates
   delete-duplicates!
   dotted-list?
   drop
   drop-right
   drop-right!
   drop-while
   eighth
   end-of-char-set?
   every
   fifth
   filter
   filter!
   filter-map
   find
   find-tail
   first
   fixnum?
   fold
   fold-right
   for-each
   fourth
   fx*
   fx*/carry
   fx+
   fx+/carry
   fx-
   fx-/carry
   fx-greatest
   fx-least
   fx-width
   fx<=?
   fx<?
   fx=?
   fx>=?
   fx>?
   fxabs
   fxand
   fxarithmetic-shift
   fxarithmetic-shift-left
   fxarithmetic-shift-right
   fxbit-count
   fxbit-field
   fxbit-field-reverse
   fxbit-field-rotate
   fxbit-set?
   fxcopy-bit
   fxeven?
   fxfirst-set-bit
   fxif
   fxior
   fxlength
   fxmax
   fxmin
   fxneg
   fxnegative?
   fxnot
   fxodd?
   fxpositive?
   fxquotient
   fxremainder
   fxsqrt
   fxsquare
   fxxor
   fxzero?
   hash-bound
   hash-salt
   hash-table
   hash-table->alist
   hash-table-clear!
   hash-table-contains?
   hash-table-copy
   hash-table-count
   hash-table-delete!
   hash-table-difference!
   hash-table-empty-copy
   hash-table-empty?
   hash-table-entries
   hash-table-find
   hash-table-fold
   hash-table-for-each
   hash-table-intern!
   hash-table-intersection!
   hash-table-keys
   hash-table-map
   hash-table-map!
   hash-table-map->list
   hash-table-mutable?
   hash-table-pop!
   hash-table-prune!
   hash-table-ref
   hash-table-ref/default
   hash-table-set!
   hash-table-size
   hash-table-unfold
   hash-table-union!
   hash-table-update!
   hash-table-update!/default
   hash-table-values
   hash-table-xor!
   hash-table=?
   hash-table?
   import
   iota
   last
   last-pair
   length
   length+
   list
   list->char-set
   list->char-set!
   list-copy
   list-index
   list-ref
   list-tabulate
   list=
   lset-adjoin
   lset-diff+intersection
   lset-diff+intersection!
   lset-difference
   lset-difference!
   lset-intersection
   lset-intersection!
   lset-union
   lset-union!
   lset-xor
   lset-xor!
   lset<=
   lset=
   make-comparator
   make-default-comparator
   make-eq-comparator
   make-equal-comparator
   make-eqv-comparator
   make-hash-table
   make-list
   make-list-comparator
   make-pair-comparator
   make-vector-comparator
   map!
   map-in-order
   member
   memq
   memv
   ninth
   not-pair?
   null-list?
   null?
   number-hash
   pair-fold
   pair-fold-right
   pair-for-each
   pair?
   partition
   partition!
   pk
   proper-list?
   reduce
   reduce-right
   remove
   remove!
   reverse
   reverse!
   second
   set-car!
   set-cdr!
   seventh
   sixth
   span
   span!
   split-at
   split-at!
   string->char-set
   string->char-set!
   string-ci-hash
   string-hash
   symbol-hash
   take
   take!
   take-right
   take-while
   take-while!
   tenth
   third
   #;tree-copy
   ucs-range->char-set
   ucs-range->char-set!
   unfold
   unfold-right
   unzip1
   unzip2
   unzip3
   unzip4
   unzip5
   xcons
   zip
   char-set-contains?
   char-set:letter+digit
   list->char-set
   )

  (import (prefix (chezscheme) chez-)
          (prefix (only (rnrs)
                        assoc
                        for-each
                        member
                        open-file-input-port
                        open-file-output-port)
                  r6rs-)
          (arew matchable)
          (arew record))

  (chez-alias alias chez-alias)

  (alias import chez-import)

  (alias syntax chez-syntax)

  (alias define chez-define)

  (define pk
    (lambda args
      (display ";; " (current-error-port))
      (write args (current-error-port))
      (newline (current-error-port))
      (chez-flush-output-port (current-error-port))
      (car (reverse args))))

  (alias lambda chez-lambda)

  (alias include chez-include)

  (include "arew/base.scm")

  (alias bytevector=? chez-bytevector=?)

  (alias case-lambda chez-case-lambda)

  (include "arew/char.scm")

  (include "arew/complex.scm")

  (include "arew/cxr.scm")

  (include "arew/eval.scm")

  (include "arew/file.scm")

  (include "arew/inexact.scm")

  (include "arew/lazy.scm")

  (alias load chez-load)

  (include "arew/process-context.scm")

  (alias read chez-read)

  (alias interaction-environment chez-interaction-environment)

  (include "arew/write.scm")

  (include "arew/charset.scm")

  (include "arew/fixnum.scm")

  (include "arew/comparator.scm")

  (include "arew/hash-table.scm")

  (include "arew/list.scm"))
