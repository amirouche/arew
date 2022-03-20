;; Part of Scheme 48 1.9.  See file COPYING for notices and license.

;; Authors: Mike Sperber
;; Copyright (c) 2005-2006 by Basis Technology Corporation.

;; Inversion lists are representations for sets of integers,
;; represented as sorted sets of ranges.

;; This was taken from Chapter 13 of Richard Gillam: Unicode Demystified.
;; Mike doesn't know what the original source is.

;; This was written as support code for the implementation of SRFI 14,
;; which is why there's so many exports here nobody really needs.

(define-record-type <inversion-list>
  (make-inversion-list min max range-vector)
  inversion-list?
  ;; minimum element, needed for complement & difference
  (min inversion-list-min)
  ;; maximum element, needed size
  ;; we pretty much assume consistency for union / intersection for MIN and MAX
  (max inversion-list-max)
  ;; consecutive elements are paired to form ranges of the form
  ;; [ (vector-ref v i) (vector-ref v (+ 1 i)) )
  ;; (except the last one, possibly)
  (range-vector inversion-list-range-vector))

(define (make-empty-inversion-list min max)
  (make-inversion-list min max '#()))

(define (inversion-list-member? n i-list)
  (let ((ranges (inversion-list-range-vector i-list)))
    (let loop ((low 0)
	       (high (vector-length ranges)))
      (if (fx<? low high)
	  (let ((mid (fxquotient (fx+ low high) 2)))
	    (if (fx>=? n (vector-ref ranges mid))
		(loop (fx+ mid 1) high)
		(loop low mid)))
	  (fxodd? high)))))

(define (inversion-list-complement i-list)
  (let* ((ranges (inversion-list-range-vector i-list))
	 (min (inversion-list-min i-list))
	 (max (inversion-list-max i-list))
	 (size (vector-length ranges)))
    (make-inversion-list
     min max
     (cond
      ((fxzero? size)
       (vector min))
      ((not (fx=? min (vector-ref ranges 0)))
       (if (and (fxeven? size)
		(fx=? max (vector-ref ranges (fx- size 1))))
	   (let ((result (make-vector size)))
	     (vector-set! result 0 min)
	     (vector-copy! ranges 0 result 1 (fx- size 1))
	     result)
	   (let ((result (make-vector (fx+ 1 size))))
	     (vector-set! result 0 min)
	     (vector-copy! ranges 0 result 1 size)
	     result)))
      ((and (even? size)
	    (fx=? max (vector-ref ranges (fx- size 1))))
       (let ((result (make-vector (fx- size 2))))
	 (vector-copy! ranges 1 result 0 (fx- size 2))
	 result))
      (else
       (let ((result (make-vector (fx- size 1))))
	 (vector-copy! ranges 1 result 0 (fx- size 1))
	 result))))))

(define (make-inversion-list-union/intersection
	 proc-thunk ;; for CALL-ERROR
	 write-increment-count write-decrement-count
	 process-first? decrement-count?
	 middle-increment
	 copy-extra-count)

  (lambda (i-list-1 i-list-2)
    (if (or (not (fx=? (inversion-list-min i-list-1)
		       (inversion-list-min i-list-2)))
	    (not (fx=? (inversion-list-max i-list-1)
		       (inversion-list-max i-list-2))))
	(error 'make-inversion-list-union/intersection
			     "min/max mismatch" (proc-thunk) i-list-1 i-list-2))
    (let ((ranges-1 (inversion-list-range-vector i-list-1))
	  (ranges-2 (inversion-list-range-vector i-list-2))
	  (min (inversion-list-min i-list-1))
	  (max (inversion-list-max i-list-1)))

      (let ((size-1 (vector-length ranges-1))
	    (size-2 (vector-length ranges-2)))
	(let ((temp (make-vector (fx+ size-1 size-2))))

	  (let loop ((index-1 0) (index-2 0)
		     (count 0)
		     (index-result 0))

	    (if (and (fx<? index-1 size-1)
		     (fx<? index-2 size-2))
		(let ((el-1 (vector-ref ranges-1 index-1))
		      (el-2 (vector-ref ranges-2 index-2)))
		  (call-with-values
		      (lambda ()
			(if (or (fx<? el-1 el-2)
				(and (fx=? el-1 el-2)
				     (process-first? index-1)))
			    (values index-1 el-1 (fx+ 1 index-1) index-2)
			    (values index-2 el-2 index-1 (fx+ 1 index-2))))
		    (lambda (index el index-1 index-2)
		      (if (even? index)
			  (if (fx=? write-increment-count count)
			      (begin
				(vector-set! temp index-result el)
				(loop index-1 index-2 (fx+ 1 count) (fx+ 1 index-result)))
			      (loop index-1 index-2 (fx+ 1 count) index-result))
			  (if (fx=? write-decrement-count count)
			      (begin
				(vector-set! temp index-result el)
				(loop index-1 index-2 (fx- count 1) (fx+ 1 index-result)))
			      (loop index-1 index-2 (fx- count 1) index-result))))))
		(let* ((count
			(if (or (and (not (fx=? index-1  size-1))
				     (decrement-count? index-1))
				(and (not (fx=? index-2 size-2))
				     (decrement-count? index-2)))
			    (fx+ count middle-increment)
			    count))
		       (result-size
			(if (fx=? copy-extra-count count)
			    (fx+ index-result
			         (fx- size-1 index-1)
			         (fx- size-2 index-2))
			    index-result))
		       (result (make-vector result-size)))
		  (vector-copy! temp 0 result 0 index-result)
		  (if (fx=? copy-extra-count count)
		      (begin
			(vector-copy! ranges-1 index-1 result index-result
				      (fx- size-1 index-1))
			(vector-copy! ranges-2 index-2 result index-result
				      (fx- size-2 index-2))))
		  (make-inversion-list min max result)))))))))

;; for associative procedures only
(define (binary->n-ary proc/2)
  (lambda (arg-1 . args)
    (if (and (pair? args)
	     (null? (cdr args)))
	(proc/2 arg-1 (car args))
	(let loop ((args args)
		   (result arg-1))
	  (if (null? args)
	      result
	      (loop (cdr args) (proc/2 result (car args))))))))

(define inversion-list-union
  (binary->n-ary
   (make-inversion-list-union/intersection (lambda () inversion-list-union)
					   0 1 fxeven? fxodd? -1 0)))


(define inversion-list-intersection
  (binary->n-ary
   (make-inversion-list-union/intersection (lambda () inversion-list-intersection)
					   1 2 fxodd? fxeven? +1 2)))

(define inversion-list-difference
  (binary->n-ary
   (lambda (i-list-1 i-list-2)
     (inversion-list-intersection i-list-1
				  (inversion-list-complement i-list-2)))))

(define (number->inversion-list min max n)
  (if (or (fx<? n min)
	  (fx>=? n max))
      (error 'number->inversion-list "invalid number"
			   min max n))
  (make-inversion-list min max
		       (if (fx=? n (fx- max 1))
			   (vector n)
			   (vector n (fx+ n 1)))))

(define (numbers->inversion-list min max . numbers)
  (cond
   ((null? numbers) (make-empty-inversion-list min max))
   ((null? (cdr numbers)) (number->inversion-list min max (car numbers)))
   (else
    (let loop ((numbers (cdr numbers))
	       (i-list (number->inversion-list min max (car numbers))))
      (if (null? numbers)
	  i-list
	  (loop (cdr numbers)
		(inversion-list-union
		 i-list
		 (number->inversion-list min max (car numbers)))))))))

(define (range->inversion-list min max left right)
  (if (or (fx>? min max)
	  (fx>? left right)
	  (fx<? left min)
	  (fx>? right max))
      (error 'range->inversion-list "invalid range"
			   min max left right))
  (make-inversion-list min max
		       (if (fx=? right max)
			   (vector left)
			   (vector left right))))

(define (ranges->inversion-list min max . ranges)
  (let loop ((ranges ranges)
	     (result (make-empty-inversion-list min max)))
    (if (null? ranges)
	result
	(let ((range-pair (car ranges)))
	  (let ((left (car range-pair))
		(right (cdr range-pair)))
	    (if (not (and (number? left)
			  (number? right)))
		(error 'ranges->inversion-list "invalid range"
				     min max (cons left right)))
	    (loop (cdr ranges)
		  (inversion-list-union result
					(range->inversion-list min max left right))))))))

(define (inversion-list-adjoin i-list . numbers)
  (inversion-list-union i-list
			(apply
			 numbers->inversion-list
			 (inversion-list-min i-list)
			 (inversion-list-max i-list)
			 numbers)))

(define (inversion-list-remove i-list . numbers)
  (inversion-list-difference i-list
			     (apply
			      numbers->inversion-list
			      (inversion-list-min i-list)
			      (inversion-list-max i-list)
			      numbers)))

(define (inversion-list-size i-list)
  (let* ((ranges (inversion-list-range-vector i-list))
	 (size (vector-length ranges)))
    (let loop ((index 0)
	       (count 0))
      (cond
       ((fx>=? index size) count)
       ((fx=? (fx+ 1 index) size)
	(fx+ count (fx- (inversion-list-max i-list)
		        (vector-ref ranges index))))
       (else
	(loop (fx+ 2 index)
	      (fx+ count
		   (fx- (vector-ref ranges (fx+ 1 index))
		        (vector-ref ranges index)))))))))

(define (inversion-list=? i-list-1 i-list-2)
  (and (fx=? (inversion-list-min i-list-1)
	     (inversion-list-min i-list-2))
       (fx=? (inversion-list-max i-list-1)
	     (inversion-list-max i-list-2))
       (equal? (inversion-list-range-vector i-list-1)
	       (inversion-list-range-vector i-list-2))))

(define (inversion-list-copy i-list)
  (make-inversion-list (inversion-list-min i-list)
		       (inversion-list-max i-list)
		       (vector-copy (inversion-list-range-vector i-list))))

;; Iterate over the elements until DONE? (applied to the accumulator)
;; returns #t
(define (inversion-list-fold/done? kons knil done? i-list)
  (let* ((ranges (inversion-list-range-vector i-list))
	 (size (vector-length ranges)))
    (let loop ((v knil)
	       (i 0))
      (if (fx>=? i size)
	  v
	  (let ((left (vector-ref ranges i))
		(right (if (fx<? i (fx- size 1))
			   (vector-ref ranges (fx+ 1 i))
			   (inversion-list-max i-list))))
	    (let inner-loop ((v v) (n left))
	      (if (fx>=? n right)
		  (loop v (fx+ 2 i))
		  (let ((v (kons n v)))
		    (if (done? v)
			v
			(inner-loop v (fx+ 1 n)))))))))))

;; It never ends with Olin

(define-record-type <inversion-list-cursor>
  (make-inversion-list-cursor index number)
  inversion-list-cursor?
  ;; index into the range vector (always even), #f if we're at the end
  (index inversion-list-cursor-index)
  ;; number within that index
  (number inversion-list-cursor-number))

(define (inversion-list-cursor i-list)
  (let ((ranges (inversion-list-range-vector i-list)))
    (if (fxzero? (vector-length ranges))
	(make-inversion-list-cursor #f #f)
	(make-inversion-list-cursor 0 (vector-ref ranges 0)))))

(define (inversion-list-cursor-at-end? cursor)
  (not (inversion-list-cursor-index cursor)))

(define (inversion-list-cursor-next i-list cursor)
  (let ((index (inversion-list-cursor-index cursor))
	(number (inversion-list-cursor-number cursor)))
    (let* ((ranges (inversion-list-range-vector i-list))
	   (size (vector-length ranges))
	   (right (if (fx>=? (fx+ index 1) size)
		      (inversion-list-max i-list)
		      (vector-ref ranges (fx+ index 1)))))
      (cond
       ((fx<? number (fx- right 1))
	(make-inversion-list-cursor index (fx+ 1 number)))
       ((fx<? (fx+ index 2) size)
	(make-inversion-list-cursor (fx+ index 2)
				    (vector-ref ranges (fx+ index 2))))
       (else
	(make-inversion-list-cursor #f #f))))))

(define (inversion-list-cursor-ref cursor)
  (inversion-list-cursor-number cursor))

;; Uses the same method as Olin's reference implementation for SRFI 14.

(define (inversion-list-hash i-list bound)
  (let ((mask (let loop ((i #x10000)) ;; skip first 16 iterations
		(if (fx>=? i bound)
		    (fx- i 1)
		    (loop (fx+ i i))))))
    (let* ((range-vector (inversion-list-range-vector i-list))
	   (size (vector-length range-vector)))
      (let loop ((i 0) (ans 0))
	(if (fx>=? i size)
	    (modulo ans bound)
	    (loop (fx+ 1 i)
		  (fxand mask
			 (fx+ (fx* 37 ans)
			      (vector-ref range-vector i)))))))))

;; Utilities

;; (define (vector-copy v)
;;   (let* ((size (vector-length v))
;; 	 (copy (make-vector size)))
;;     (vector-copy! v 0 copy 0 size)
;;     copy))
