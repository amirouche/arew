;; Part of Scheme 48 1.9.  See file COPYING for notices and license.

;; Authors: Mike Sperber

;; This constructs the SRFI 14 char sets from thin air and what's defined in
;; srfi-14-base-char-sets.scm.

;; Defined there:
;; lower-case, upper-case, title-case, letter, digit, punctuation, symbol

(define char-set:empty (char-set))

(define char-set:full (char-set-complement char-set:empty))

(define char-set:letter+digit
  (char-set-union char-set:letter char-set:digit))

(define char-set:graphic
  (char-set-union char-set:mark
		  char-set:letter
		  char-set:digit
		  char-set:symbol
		  char-set:punctuation))

(define char-set:whitespace
  (char-set-union char-set:separator
		  (list->char-set (map integer->char
                           '(9 ;; tab
                             10 ;; newline
                             11 ;; vtab
                             12 ;; page
                             13 ;; return
                             )))))


(define char-set:printing
  (char-set-union char-set:whitespace char-set:graphic))

(define char-set:iso-control
  (char-set-union (ucs-range->char-set 0 #x20)
		  (ucs-range->char-set #x7f #xa0)))

(define char-set:blank
  (char-set-union char-set:space-separator
		  (char-set (integer->char 9)))) ;; tab

(define char-set:ascii (ucs-range->char-set 0 128))

(define char-set:hex-digit (string->char-set "0123456789abcdefABCDEF"))
