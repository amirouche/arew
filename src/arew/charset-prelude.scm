;; Part of Scheme 48 1.9.  See file COPYING for notices and license.
;;
;; Authors: Mike Sperber, Robert Tansom
;;
;; Copyright (c) 2005-2006 by Basis Technology Corporation.
;;
;; This is basically a complete re-implementation, suitable for Unicode.
;;
;; Some bits and pieces from Olin's reference implementation remain,
;; but none from the MIT Scheme code.  For whatever remains, the
;; following copyright holds:
;;
;; Copyright (c) 1994-2003 by Olin Shivers
;;
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the authors may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(define (list-partition pred objects)
  ;; taken and adapted from chez-srfi
  ;; TODO: rewrite use tail call
  (let recur ((objects objects))
    (if (null? objects) (values objects objects)
        (let ((elt (car objects))
              (tail (cdr objects)))
          (call-with-values (lambda () (recur tail))
            (lambda (in out)
              (if (pred elt)
                  (values (if (pair? out) (cons elt in) objects) out)
                  (values in (if (pair? in) (cons elt out) objects)))))))))


(define-record-type <char-set>
  (make-char-set simple i-list)
  char-set?
  ;; byte vector for the Latin-1 part
  (simple char-set-simple set-char-set-simple!)
  ;; inversion list for the rest
  (i-list char-set-i-list char-set-i-list!))

;;; "Simple Csets"---we use mutable byte vectors for the Latin-1 part

(define *simple-cset-boundary* 256)

(define (simple-char? c)
  (fx<? (char->integer c) *simple-cset-boundary*))

(define (make-empty-simple-cset)
  (make-bytevector *simple-cset-boundary* 0))

(define (make-full-simple-cset)
  (make-bytevector *simple-cset-boundary* 1))

(define (copy-simple-cset s)
  (bytevector-copy s))

;; don't mistake these for abstractions
(define (simple-cset-code-not-member? s i) (fxzero? (bytevector-u8-ref s i)))
(define (simple-cset-code-member? s i) (not (simple-cset-code-not-member? s i)))
(define (simple-cset-ref s i) (bytevector-u8-ref s i))
(define (simple-cset-set! s i v) (bytevector-u8-set! s i v))
(define (simple-cset-remove-code! s i) (bytevector-u8-set! s i 0))
(define (simple-cset-adjoin-code! s i) (bytevector-u8-set! s i 1))

(define (simple-cset-contains? s char)
  (simple-cset-code-member? s (char->integer char)))

(define (simple-cset=? s1 s2)
  (bytevector=? s1 s2))

(define (simple-cset<=? s1 s2)
  (or (eq? s1 s2)
      (let loop ((i 0))
        (if (fx>=? i *simple-cset-boundary*)
            #t
            (and (fx<=? (simple-cset-ref s1 i) (simple-cset-ref s2 i))
                 (loop (fx+ 1 i)))))))

(define (simple-cset-size s)
  (let loop ((i 0) (size 0))
    (if (fx>=? i *simple-cset-boundary*)
        size
        (loop (fx+ 1 i) (fx+ size (simple-cset-ref s i))))))

(define (simple-cset-count pred s)
  (let loop ((i 0) (count 0))
    (if (fx>=? i *simple-cset-boundary*)
        count
        (loop (fx+ 1 i)
              (if (and (simple-cset-code-member? s i) (pred (integer->char i)))
                  (fx+ count 1)
                  count)))))

(define (simple-cset-modify! modify! s chars)
  (for-each (lambda (c) (modify! s (char->integer c))) chars) s)

(define (simple-cset-modify modify! s chars)
  (simple-cset-modify! modify! (copy-simple-cset s) chars))

(define (simple-cset-adjoin s . chars)
  (simple-cset-modify simple-cset-adjoin-code! s chars))
(define (simple-cset-adjoin! s . chars)
  (simple-cset-modify! simple-cset-adjoin-code! s chars))
(define (simple-cset-delete s . chars)
  (simple-cset-modify simple-cset-remove-code! s chars))
(define (simple-cset-delete! s . chars)
  (simple-cset-modify! simple-cset-remove-code! s chars))

;;; If we represented char sets as a bit set, we could do the following
;;; trick to pick the lowest bit out of the set:
;;;   (count-bits (xor (- cset 1) cset))
;;; (But first mask out the bits already scanned by the cursor first.)

(define (simple-cset-cursor-next s cursor)
  (let loop ((cur cursor))
    (let ((cur (fx- cur 1)))
      (if (or (fx<? cur 0) (simple-cset-code-member? s cur))
          cur
          (loop cur)))))

(define (end-of-simple-cset? cursor)
  (fxnegative? cursor))

(define (simple-cset-cursor-ref cursor)
  (integer->char cursor))

(define (simple-cset-for-each proc s)
  (let loop ((i 0))
    (when (fx<? i *simple-cset-boundary*)
      (when (simple-cset-code-member? s i)
        (proc (integer->char i)))
      (loop (fx+ 1 i)))))

(define (simple-cset-fold kons knil s)
  (let loop ((i 0) (ans knil))
    (if (fx>=? i *simple-cset-boundary*)
        ans
        (loop (fx+ 1 i)
              (if (simple-cset-code-not-member? s i)
                  ans
                  (kons (integer->char i) ans))))))

(define (simple-cset-every? pred s)
  (let loop ((i 0))
    (cond
     ((fx>=? i *simple-cset-boundary*)
      #t)
     ((or (simple-cset-code-not-member? s i)
          (pred (integer->char i)))
      (loop (fx+ 1 i)))
     (else
      #f))))

(define (simple-cset-any pred s)
  (let loop ((i 0))
    (cond
     ((fx>=? i *simple-cset-boundary*) #f)
     ((and (simple-cset-code-member? s i)
           (pred (integer->char i))))
     (else
      (loop (fx+ 1 i))))))

(define (ucs-range->simple-cset lower upper)
  (let ((s (make-empty-simple-cset)))
    (let loop ((i lower))
      (if (fx<? i upper)
          (begin
            (simple-cset-adjoin-code! s i)
            (loop (fx+ 1 i)))))
    s))

;; Algebra

;; These do various "s[i] := s[i] op val" operations

(define (simple-cset-invert-code! s i v)
  (simple-cset-set! s i (fx- 1 v)))

(define (simple-cset-and-code! s i v)
  (when (fxzero? v)
    (simple-cset-remove-code! s i)))

(define (simple-cset-or-code! s i v)
  (unless (fxzero? v)
    (simple-cset-adjoin-code! s i)))

(define (simple-cset-minus-code! s i v)
  (unless (fxzero? v)
    (simple-cset-remove-code! s i)))

(define (simple-cset-xor-code! s i v)
  (unless (fxzero? v)
    (simple-cset-set! s i (fx- 1 (simple-cset-ref s i)))))

(define (simple-cset-complement s)
  (simple-cset-complement! (copy-simple-cset s)))

(define (simple-cset-complement! s)
  (bytevector-for-each (lambda (i v) (simple-cset-invert-code! s i v)) s) s)

(define (simple-cset-op! s simple-csets code-op!)
  (for-each (lambda (s2)
              (let loop ((i 0))
                (when (fx<? i *simple-cset-boundary*)
                  (code-op! s i (simple-cset-ref s2 i))
                  (loop (fx+ 1 i)))))
            simple-csets)
  s)

(define (simple-cset-union! s1 . ss)
  (simple-cset-op! s1 ss simple-cset-or-code!))

(define (simple-cset-union . ss)
  (if (pair? ss)
      (apply simple-cset-union!
             (bytevector-copy (car ss))
             (cdr ss))
      (make-empty-simple-cset)))

(define (simple-cset-intersection! s1 . ss)
  (simple-cset-op! s1 ss simple-cset-and-code!))

(define (simple-cset-intersection . ss)
  (if (pair? ss)
      (apply simple-cset-intersection!
             (bytevector-copy (car ss))
             (cdr ss))
      (make-full-simple-cset)))

(define (simple-cset-difference! s1 . ss)
  (simple-cset-op! s1 ss simple-cset-minus-code!))

(define (simple-cset-difference s1 . ss)
  (if (pair? ss)
      (apply simple-cset-difference! (copy-simple-cset s1) ss)
      (copy-simple-cset s1)))

(define (simple-cset-xor! s1 . ss)
  (simple-cset-op! s1 ss simple-cset-xor-code!))

(define (simple-cset-xor . ss)
  (if (pair? ss)
      (apply simple-cset-xor!
             (bytevector-copy (car ss))
             (cdr ss))
      (make-empty-simple-cset)))

(define (simple-cset-diff+intersection! s1 s2 . ss)
  (bytevector-for-each (lambda (i v)
                         (cond
                          ((fxzero? v)
                           (simple-cset-remove-code! s2 i))
                          ((simple-cset-code-member? s2 i)
                           (simple-cset-remove-code! s1 i))))
                       s1)

  (for-each (lambda (s)
              (bytevector-for-each (lambda (i v)
                                     (when (and (not (fxzero? v))
                                                (simple-cset-code-member? s1 i))
                                       (simple-cset-remove-code! s1 i)
                                       (simple-cset-adjoin-code! s2 i)))
                                   s))
            ss)

  (values s1 s2))

;; Compute (c + 37 c + 37^2 c + ...) modulo BOUND, with sleaze thrown
;; in to keep the intermediate values small. (We do the calculation
;; with just enough bits to represent BOUND, masking off high bits at
;; each step in calculation. If this screws up any important properties
;; of the hash function I'd like to hear about it. -Olin)

(define (simple-cset-hash s bound)
  ;; The mask that will cover BOUND-1:
  (let ((mask (let loop ((i #x10000)) ;; Let's skip first 16 iterations, eh?
                (if (fx>=? i bound) (fx- i 1) (loop (fx+ i i))))))
    (let loop ((i (fx- *simple-cset-boundary* 1)) (ans 0))
      (if (fx<? i 0)
          (modulo ans bound)
          (loop (fx- i 1)
                (if (simple-cset-code-not-member? s i)
                    ans
                    (fxand mask (fx+ (* 37 ans) i))))))))

;;; Now for the real character sets

(define (make-empty-char-set)
  (make-char-set (make-empty-simple-cset)
                 (make-empty-inversion-list *simple-cset-boundary* (fx+ 1 #x10ffff))))

(define (make-full-char-set)
  (make-char-set (make-full-simple-cset)
                 (range->inversion-list *simple-cset-boundary* (fx+ 1 #x10ffff)
                                        *simple-cset-boundary* (fx+ 1 #x10ffff))))

(define (char-set-copy cs)
  (make-char-set (copy-simple-cset (char-set-simple cs))
                 (inversion-list-copy (char-set-i-list cs))))

;; n-ary version

(define (char-set= . rest)
  (or (null? rest)
      (let ((cs1  (car rest))
            (rest (cdr rest)))
        (let loop ((rest rest))
          (or (not (pair? rest))
              (and (char-set=/2 cs1 (car rest))
                   (loop (cdr rest))))))))

;; binary version

(define (char-set=/2 cs-1 cs-2)
  (and (simple-cset=? (char-set-simple cs-1) (char-set-simple cs-2))
       (inversion-list=? (char-set-i-list cs-1)
                         (char-set-i-list cs-2))))

;; n-ary

(define (char-set<= . rest)
  (or (null? rest)
      (let ((cs1  (car rest))
            (rest (cdr rest)))
        (let loop ((cs1 cs1)  (rest rest))
          (or (not (pair? rest))
              (and (char-set<=/2 cs1 (car rest))
                   (loop (car rest) (cdr rest))))))))

;; binary

(define (char-set<=/2 cs-1 cs-2)
  (and (simple-cset<=? (char-set-simple cs-1) (char-set-simple cs-2))
       (inversion-list<=? (char-set-i-list cs-1)
                          (char-set-i-list cs-2))))

(define (inversion-list<=? i-list-1 i-list-2)
  (inversion-list=? i-list-1
                    (inversion-list-intersection i-list-1 i-list-2)))

;;; Hash

;; We follow Olin's reference implementation:
                                        ;
;; If you keep BOUND small enough, the intermediate calculations will
;; always be fixnums. How small is dependent on the underlying Scheme
;; system; we use a default BOUND of 2^22 = 4194304, which should hack
;; it in Schemes that give you at least 29 signed bits for
;; fixnums. The core calculation that you don't want to overflow is,
;; worst case,
;;
;;     (fx+ 65535 (* 37 (fx- bound 1)))
;;
;; where 65535 is the max character code. Choose the default BOUND to
;; be the biggest power of two that won't cause this expression to
;; fixnum overflow, and everything will be copacetic.

(define char-set-hash
  (case-lambda
   ;; TODO: where does 4194304 comes from
   ((cs) (char-set-hash cs 4194304))
   ((cs bound)
    (unless (and (integer? bound)
                 (exact? bound)
                 (fx<=? 0 bound))
      (error 'char-set-hash "invalid bound" bound))
    (let ((bound (if (fxzero? bound) 4194304 bound)))
      (modulo (fx+ (simple-cset-hash (char-set-simple cs) bound)
                   ;; TODO: what is 37
                   (* 37 (inversion-list-hash (char-set-i-list cs) bound)))
              bound)))))

(define (char-set-contains? cs char)
  (if (simple-char? char)
      (simple-cset-contains? (char-set-simple cs) char)
      (inversion-list-member? (char->integer char)
                              (char-set-i-list cs))))

(define (char-set-size cs)
  (fx+ (simple-cset-size (char-set-simple cs))
       (inversion-list-size (char-set-i-list cs))))

(define (char-set-count pred cset)
  (fx+ (simple-cset-count pred (char-set-simple cset))
       (inversion-list-count pred (char-set-i-list cset))))

(define (inversion-list-count pred i-list)
  (inversion-list-fold/done? (lambda (v count)
                               (if (pred (integer->char v))
                                   (fx+ 1 count)
                                   count))
                             0
                             (lambda (v) #f)
                             i-list))

(define (make-char-set-char-op simple-cset-op inversion-list-op)
  (lambda (cs . chars)
    (call-with-values
        (lambda () (list-partition simple-char? chars))
      (lambda (simple-chars non-simple-chars)
        (make-char-set (apply simple-cset-op (char-set-simple cs) simple-chars)
                       (apply inversion-list-op (char-set-i-list cs)
                              (map char->integer non-simple-chars)))))))

(define (make-char-set-char-op! simple-cset-op! simple-cset-op
                                inversion-list-op)
  (lambda (cs . chars)
    (call-with-values
        (lambda () (list-partition simple-char? chars))
      (lambda (simple-chars non-simple-chars)
        (if (null? non-simple-chars)
            (apply simple-cset-op! (char-set-simple cs) simple-chars)
            (begin
              (set-char-set-simple! cs
                                    (apply simple-cset-op (char-set-simple cs)
                                           simple-chars))
              (char-set-i-list! cs
                                    (apply inversion-list-op (char-set-i-list cs)
                                           (map char->integer non-simple-chars)))))))
    cs))

(define char-set-adjoin
  (make-char-set-char-op simple-cset-adjoin inversion-list-adjoin))

(define char-set-adjoin!
  (make-char-set-char-op! simple-cset-adjoin! simple-cset-adjoin
                          inversion-list-adjoin))

(define char-set-delete
  (make-char-set-char-op simple-cset-delete inversion-list-remove))

(define char-set-delete!
  (make-char-set-char-op! simple-cset-delete! simple-cset-delete
                          inversion-list-remove))

;;; Cursors

;; A cursor is either an integer index into the mark vector -1 for the
;; end-of-char-set cursor) as in the reference implementation, and an
;; inversion-list cursor otherwise.

(define (char-set-cursor cset)
  (let ((simple-cursor
         (simple-cset-cursor-next (char-set-simple cset)
                                  *simple-cset-boundary*)))
    (if (end-of-simple-cset? simple-cursor)
        (inversion-list-cursor (char-set-i-list cset))
        simple-cursor)))

(define (end-of-char-set? cursor)
  (and (inversion-list-cursor? cursor)
       (inversion-list-cursor-at-end? cursor)))

(define (char-set-ref cset cursor)
  (if (number? cursor)
      (simple-cset-cursor-ref cursor)
      (integer->char (inversion-list-cursor-ref cursor))))

(define (char-set-cursor-next cset cursor)
  (cond
   ((number? cursor)
    (let ((next (simple-cset-cursor-next (char-set-simple cset) cursor)))
      (if (end-of-simple-cset? next)
          (inversion-list-cursor (char-set-i-list cset))
          next)))
   (else
    (inversion-list-cursor-next (char-set-i-list cset) cursor))))

(define (char-set-for-each proc cs)
  (simple-cset-for-each proc (char-set-simple cs))
  (inversion-list-fold/done? (lambda (n _)
                               (proc (integer->char n))
                               (chez-void))
                             #f
                             (lambda (_) #f)
                             (char-set-i-list cs)))

;; this is pretty inefficent
(define (char-set-map proc cs)
  (let ((simple-cset (make-empty-simple-cset))
        (other-scalar-values '()))

    (define (adjoin! c)
      (let ((c (proc c)))
        (if (simple-char? c)
            (simple-cset-adjoin! simple-cset c)
            (set! other-scalar-values
                  (cons (char->integer c) other-scalar-values)))))

    (char-set-for-each adjoin! cs)

    (make-char-set simple-cset
                   (apply numbers->inversion-list
                          *simple-cset-boundary* (fx+ 1 #x10ffff)
                          other-scalar-values))))

(define (char-set-fold kons knil cs)
  (inversion-list-fold/done? (lambda (n v)
                               (kons (integer->char n) v))
                             (simple-cset-fold kons knil (char-set-simple cs))
                             (lambda (_) #f)
                             (char-set-i-list cs)))

(define (char-set-every pred cs)
  (and (simple-cset-every? pred (char-set-simple cs))
       (inversion-list-fold/done? (lambda (n v)
                                    (and v
                                         (pred (integer->char n))))
                                  #t
                                  not
                                  (char-set-i-list cs))))

(define (char-set-any pred cs)
  (or (simple-cset-any pred (char-set-simple cs))
      (inversion-list-fold/done? (lambda (n v)
                                   (or v
                                       (pred (integer->char n))))
                                 #f
                                 values
                                 (char-set-i-list cs))))

(define (base-char-set maybe-base-cs)
  (if maybe-base-cs
      (char-set-copy maybe-base-cs)
      (make-empty-char-set)))

(define char-set-unfold
  (case-lambda
   ((p f g seed)
    (char-set-unfold p f g seed #f))
   ((p f g seed maybe-base-cs)
    (char-set-unfold! p f g seed
                      (base-char-set maybe-base-cs)))))

(define (char-set-unfold! p f g seed base-cs)
  (let loop ((seed seed) (cs base-cs))
    (if (p seed) cs			;; P says we are done.
        (loop (g seed)			;; Loop on (G SEED).
              (char-set-adjoin! cs (f seed)))))) ;; Add (F SEED) to set.

;; converting from and to lists

(define (char-set . chars)
  (list->char-set chars))

(define list->char-set
  (case-lambda
   ((chars)
    (list->char-set chars #f))
   ((chars maybe-base-cs)
    (list->char-set! chars
                     (base-char-set maybe-base-cs)))))

(define (list->char-set! chars cs)
  (for-each (lambda (c)
              (char-set-adjoin! cs c))
            chars)
  cs)

(define (char-set->list cs)
  (char-set-fold cons '() cs))

;; converting to and from strings

(define string->char-set
  (case-lambda
   ((str)
    (string->char-set str #f))
   ((str maybe-base-cs)
    (string->char-set! str
                       (base-char-set maybe-base-cs)))))

(define (string->char-set! str cs)
  (do ((i (fx- (string-length str) 1) (fx- i 1)))
      ((fx<? i 0))
    (char-set-adjoin! cs (string-ref str i)))
  cs)

(define (char-set->string cs)
  (let ((ans (make-string (char-set-size cs))))
    (char-set-fold (lambda (ch i)
                     (string-set! ans i ch)
                     (fx+ i 1))
                   0
                   cs)
    ans))

(define ucs-range->char-set
  (case-lambda
   ((lower upper)
    (ucs-range->char-set lower upper #f))
   ((lower upper error?)
    (ucs-range->char-set lower upper #f #f))
   ((lower upper error? maybe-base-cs)
    (ucs-range->char-set! lower upper error?
                          (base-char-set maybe-base-cs)))))

(define (ucs-range->char-set! lower upper error? base-cs)
  (if (fxnegative? lower)
      (error 'ucs-range->char-set! "negative lower bound" lower))
  (if (> lower #x10ffff)
      (error 'ucs-range->char-set! "invalid lower bound" lower))
  (if (fxnegative? upper)
      (error 'ucs-range->char-set! "negative upper bound" upper))
  (if (> upper #x110000)
      (error 'ucs-range->char-set! "invalid lower bound" upper))
  (if (not (fx<=? lower upper))
      (error 'ucs-range->char-set! "decreasing bounds" lower upper))

  (let ((create-inversion-list
         (lambda (lower upper)
           (cond
            ((and (fx>=? lower #xD800)
                  (fx>=? #xe000 upper))
             (make-empty-inversion-list *simple-cset-boundary* (fx+ 1 #x10ffff)))
            ((fx<=? upper #xe000)
             (range->inversion-list *simple-cset-boundary* (fx+ 1 #x10ffff)
                                    lower (min #xd800 upper)))
            ((fx>=? lower #xd800)
             (range->inversion-list *simple-cset-boundary* (fx+ 1 #x10ffff)
                                    (max #xe000 lower) upper))
            (else
             ;; hole
             (ranges->inversion-list *simple-cset-boundary* (fx+ 1 #x10ffff)
                                     (cons lower #xd800)
                                     (cons #xe000 upper)))))))
    (char-set-union!
     base-cs
     (cond
      ((fx>=? lower *simple-cset-boundary*)
       (make-char-set (make-empty-simple-cset)
                      (create-inversion-list lower upper)))
      ((fx<? upper *simple-cset-boundary*)
       (make-char-set (ucs-range->simple-cset lower upper)
                      (make-empty-inversion-list *simple-cset-boundary* (fx+ 1 #x10ffff))))
      (else
       (make-char-set (ucs-range->simple-cset lower *simple-cset-boundary*)
                      (create-inversion-list *simple-cset-boundary* upper)))))))

(define char-set-filter
  (case-lambda
   ((predicate domain)
    (char-set-filter predicate domain #f))
   ((predicate domain maybe-base-cs)
    (char-set-filter! predicate
                      domain
                      (base-char-set maybe-base-cs)))))

(define (char-set-filter! predicate domain base-cs)
  (char-set-fold (lambda (ch _)
                   (if (predicate ch)
                       (char-set-adjoin! base-cs ch)))
                 (chez-void)
                 domain)
  base-cs)

;; {string, char, char-set, char predicate} -> char-set

;; This is called ->CHAR-SET in the SRFI, but that's not a valid R5RS
;; identifier.

(define (->char-set x)
  (cond ((char-set? x) x)
        ((string? x) (string->char-set x))
        ((char? x) (char-set x))
        (else (error '->char-set "Not a charset, string or char."))))


;; Set algebra

(define *surrogate-complement-i-list*
  (inversion-list-complement
   (range->inversion-list *simple-cset-boundary* (fx+ 1 #x10ffff)
                          #xd800 #xe000)))

(define (char-set-complement cs)
  (make-char-set (simple-cset-complement (char-set-simple cs))
                 (inversion-list-intersection
                  (inversion-list-complement (char-set-i-list cs))
                  *surrogate-complement-i-list*)))

(define (char-set-complement! cs)
  (set-char-set-simple! cs
                        (simple-cset-complement! (char-set-simple cs)))
  (char-set-i-list! cs
                        (inversion-list-intersection
                         (inversion-list-complement (char-set-i-list cs))
                         *surrogate-complement-i-list*))
  cs)

(define (make-char-set-op! simple-cset-op! inversion-list-op)
  (lambda (cset1 . csets)
    (set-char-set-simple! cset1
                          (apply simple-cset-op!
                                 (char-set-simple cset1)
                                 (map char-set-simple csets)))
    (char-set-i-list! cset1
                          (apply inversion-list-op
                                 (char-set-i-list cset1)
                                 (map char-set-i-list csets)))
    cset1))

(define (make-char-set-op char-set-op! make-neutral)
  (lambda csets
    (if (pair? csets)
        (apply char-set-op! (char-set-copy (car csets)) (cdr csets))
        (make-neutral))))

(define char-set-union!
  (make-char-set-op! simple-cset-union! inversion-list-union))

(define char-set-union
  (make-char-set-op char-set-union! make-empty-char-set))

(define char-set-intersection!
  (make-char-set-op! simple-cset-intersection! inversion-list-intersection))

(define char-set-intersection
  (make-char-set-op char-set-intersection! make-full-char-set))

(define char-set-difference!
  (make-char-set-op! simple-cset-difference! inversion-list-difference))

(define (char-set-difference cset1 . csets)
  (apply char-set-difference! (char-set-copy cset1) csets))

(define inversion-list-xor
  (binary->n-ary
   (lambda (i-list-1 i-list-2)
     (inversion-list-union (inversion-list-intersection
                            (inversion-list-complement i-list-1)
                            i-list-2)
                           (inversion-list-intersection
                            i-list-1
                            (inversion-list-complement i-list-2))))))

;; Really inefficient for things outside Latin-1 WHO NEEDS THIS
;; NONSENSE, ANYWAY?
(define char-set-xor!
  (make-char-set-op! simple-cset-xor! inversion-list-xor))

(define char-set-xor
  (make-char-set-op char-set-xor! make-empty-char-set))

(define (char-set-diff+intersection! cs1 cs2 . csets)
  (call-with-values
      (lambda () (apply simple-cset-diff+intersection!
                        (char-set-simple cs1) (char-set-simple cs2)
                        (map char-set-simple csets)))
    (lambda (simple-diff simple-intersection)
      (set-char-set-simple! cs1 simple-diff)
      (set-char-set-simple! cs2 simple-intersection)
      (let ((i-list-1 (char-set-i-list cs1))
            (i-list-2 (char-set-i-list cs2))
            (i-list-rest (map char-set-i-list csets)))
        (char-set-i-list! cs1
                              (apply inversion-list-difference
                                     i-list-1 i-list-2
                                     i-list-rest))
        (char-set-i-list! cs2
                              (inversion-list-intersection
                               i-list-1
                               (apply inversion-list-union
                                      i-list-2
                                      i-list-rest)))
        (values cs1 cs2)))))

(define (char-set-diff+intersection cs1 . csets)
  (apply char-set-diff+intersection!
         (char-set-copy cs1)
         (make-empty-char-set)
         csets))

;; helpers

(define (bytevector-for-each p s)
  (let loop ((i (fx- (bytevector-length s) 1)))
    (unless (fxzero? i)
      (p i (bytevector-u8-ref s i))
      (loop (fx- i 1)))))

;; The range vector is an even-sized vector with [lower, upper)
;; pairs.

(define (range-vector->char-set range-vector)
  (let ((size (vector-length range-vector))
        (simple-cset (make-empty-simple-cset)))

    (let loop ((index 0) (ranges '()))
      (if (fx>=? index size)
          (make-char-set simple-cset
                         (apply ranges->inversion-list
                                *simple-cset-boundary* (fx+ 1 #x10ffff)
                                ranges))
          (let ((lower (vector-ref range-vector index))
                (upper (vector-ref range-vector (fx+ 1 index))))

            (define (fill-simple-cset! lower upper)
              (let loop ((scalar-value lower))
                (if (fx<? scalar-value upper)
                    (begin
                      (simple-cset-adjoin-code! simple-cset scalar-value)
                      (loop (fx+ 1 scalar-value))))))


            (cond
             ((fx>=? lower *simple-cset-boundary*)
              (loop (fx+ 2 index) (cons (cons lower upper) ranges)))
             ((fx<? upper *simple-cset-boundary*)
              (fill-simple-cset! lower upper)
              (loop (fx+ 2 index) ranges))
             (else
              (fill-simple-cset! lower *simple-cset-boundary*)
              (loop (fx+ 2 index)
                    (cons (cons *simple-cset-boundary* upper) ranges)))))))))
