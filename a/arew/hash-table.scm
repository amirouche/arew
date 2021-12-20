(alias hash-table-size chez-hashtable-size)

(define (hash-table-keys ht)
  (vector->list (chez-hashtable-keys ht)))

(define (hash-table-values ht)
  (vector->list (chez-hashtable-values ht)))

(define (hash-table-entries ht)
  (values (hash-table-keys ht) (hash-table-values ht)))


(define (hash-table-for-each proc ht)
  ;; XXX: With this particular implementation, the proc can safely
  ;; mutate ht.  That property is not guaranteed by the specification,
  ;; but can be relied upon by procedures defined in this file.
  (call-with-values (lambda () (hash-table-entries ht))
    (lambda (keys vals)
      (for-each proc keys vals))))

;; A unique (in the sense of eq?) value that will never be found
;; within a hash-table.

(define hash-table-singleton '(hash-table-singleton))

(define (make-hash-table comparator . args)
  (let ((hash (comparator-hash-function comparator))
        (eqv? (comparator-equality-predicate comparator)))
    (chez-make-hashtable hash eqv?)))

(define (hash-table comparator . rest)
  (let ((ht (make-hash-table comparator)))
    (let loop ((kvs rest))
      (cond
       ((null? kvs) #f)
       ((null? (cdr kvs)) (error 'hash-table "hash-table: wrong number of arguments"))
       ((hash-table-contains? ht (car kvs))
        (error 'hash-table "hash-table: two equivalent keys were provided" (car kvs)))
       (else (hash-table-set! ht (car kvs) (cadr kvs))
             (loop (cddr kvs)))))
    ht))

(define (hash-table-unfold stop? mapper successor seed comparator . rest)
  (let ((ht (apply make-hash-table comparator rest)))
    (let loop ((seed seed))
      (if (stop? seed)
          ht
          (call-with-values
              (lambda () (mapper seed))
            (lambda (key val)
              (hash-table-set! ht key val)
              (loop (successor seed))))))))

(define (alist->hash-table alist comparator . rest)
  (let ((ht (apply make-hash-table comparator rest))
        (entries alist))
    (for-each (lambda (entry) (hash-table-set! ht (car entry) (cdr entry))) entries)
    ht))

;; Predicates.

(alias hash-table? chez-hashtable?)

(alias hash-table-contains? chez-hashtable-contains?)

(define (hash-table-empty? ht)
  (fxzero? (hash-table-size ht)))

(define (hash-table=? value-comparator ht1 ht2)
  ;; FIXME: walks both hash tables because their key comparators might
  ;; be different
  (let ((val=? (comparator-equality-predicate value-comparator))
        (n1 (hash-table-size ht1))
        (n2 (hash-table-size ht2)))
    (and (= n1 n2)
         (call/cc
          (lambda (return)
            (hash-table-for-each (lambda (key val1)
                                   (or (and (hash-table-contains? ht2 key)
                                            (val=? val1
                                                   (hash-table-ref ht2 key 'ignored)))
                                       (return #f)))
                                 ht1)
            (hash-table-for-each (lambda (key val2)
                                   (or (and (hash-table-contains? ht1 key)
                                            (val=? val2
                                                   (hash-table-ref ht1 key 'ignored)))
                                       (return #f)))
                                 ht2)
            (return #t))))))

(define (hash-table-mutable? ht)
  #t)

(define (hash-table-ref-failure-default ht key)
  (error 'hash-table "key not found" ht key))

(define hash-table-ref
  (case-lambda
   ((ht key) (hash-table-ref ht key (lambda () (hash-table-ref-failure-default ht key))))
   ((ht key failure) (hash-table-ref ht key failure values))
   ((ht key failure success)
    (let ((out (chez-hashtable-ref ht key hash-table-singleton)))
      (if (eq? hash-table-singleton out)
          (success (failure))
          (success out))))))

(define (hash-table-ref/default ht key default)
  (hash-table-ref ht key (lambda () default)))

(define (hash-table-set! ht key value . rest)
  (chez-hashtable-set! ht key value)
  (unless (null? rest)
    (let loop ((kvs rest))
      (cond
       ((and (not (null? kvs))
             (not (null? (cdr kvs))))
        (hash-table-set! ht (car kvs) (cadr kvs))
        (loop (cddr kvs)))
       ((not (null? kvs))
        (error 'hash-table "hash-table-set!: wrong number of arguments"
               (cons ht rest)))))))

(define (hash-table-delete! ht . keys)
  (let loop ((keys keys)
             (cnt 0))
    (cond ((null? keys) cnt)
          ((hash-table-contains? ht (car keys))
           (chez-hashtable-delete! ht (car keys))
           (loop (cdr keys) (fx+ cnt 1)))
          (else
           (loop (cdr keys) cnt)))))

(define (hash-table-intern! ht key failure)
  (if (hash-table-contains? ht key)
      (hash-table-ref ht key)
      (let ((val (failure)))
        (hash-table-set! ht key val)
        val)))

(define (hash-table-update!% ht key updater failure success)
  (chez-hashtable-update! ht
                          key
                          (lambda (value)
                            (if (eq? value hash-table-singleton)
                                (updater (success (failure)))
                                (updater (success value))))
                          hash-table-singleton))

(define hash-table-update!
  (case-lambda
   ((ht key updater) (hash-table-update! ht key updater hash-table-ref-failure-default))
   ((ht key updater failure) (hash-table-update! ht key updater failure values))
   ((ht key updater failure success) (hash-table-update!% ht key updater failure success))))

(define (hash-table-update!/default ht key updater default)
  (hash-table-update! ht key updater (lambda () default)))

(define (hash-table-pop! ht)
  (define cell (chez-hashtable-cells ht 1))
  (hash-table-delete! ht (vector-ref (vector-ref cell 0) 0))
  (values (vector-ref (vector-ref cell 0) 0) (vector-ref (vector-ref cell 1) 0)))

(alias hash-table-clear! chez-hashtable-clear!)

;; The whole hash table.

(define (hash-table-find proc ht failure)
  (call-with-values (lambda () (hash-table-entries ht))
    (lambda (keys vals)
      (let loop ((keys keys)
                 (vals vals))
        (if (null? keys)
            (failure)
            (let* ((key (car keys))
                   (val (car vals))
                   (x   (proc key val)))
              (or x
                  (loop (cdr keys)
                        (cdr vals)))))))))

(define (hash-table-count pred ht)
  (call-with-values (lambda () (hash-table-entries ht))
    (lambda (keys vals)
      (let loop ((keys keys)
                 (vals vals)
                 (n 0))
        (if (null? keys)
            n
            (let* ((key (car keys))
                   (val (car vals))
                   (x   (pred key val)))
              (loop (cdr keys)
                    (cdr vals)
                    (if x (fx+ n 1) n))))))))

;; Mapping and folding.

(define (hash-table-map proc comparator ht)
  (let ((result (make-hash-table comparator)))
    (hash-table-for-each
     (lambda (key val)
       (hash-table-set! result key (proc val)))
     ht)
    result))

(define (hash-table-map->list proc ht)
  (call-with-values (lambda () (hash-table-entries ht))
    (lambda (keys vals)
      (map proc keys vals))))

(define (hash-table-map! proc ht)
  (hash-table-for-each (lambda (key val)
                         (hash-table-set! ht key (proc key val)))
                       ht))

(define (hash-table-fold kons seed ht)
  (let-values (((keys values) (hash-table-entries ht)))
    (let loop ((keys keys)
               (values values)
               (out seed))
        (if (null? keys)
            out
            (loop (cdr keys)
                  (cdr values)
                  (kons (car keys) (car values) out))))))

(define (hash-table-prune! proc ht)
  (hash-table-for-each (lambda (key val)
                         (if (proc key val)
                             (hash-table-delete! ht key)))
                       ht))

;; Copying and conversion.

(define (hash-table-copy ht . rest)
  (chez-hashtable-copy ht #t))

(define (hash-table-empty-copy ht)
  (let ((ht2 (hash-table-copy ht #t)))
    (hash-table-clear! ht2)
    ht2))

(define (hash-table->alist ht)
  (call-with-values (lambda () (hash-table-entries ht))
    (lambda (keys vals)
      (map cons keys vals))))

;; Hash tables as sets.

(define (hash-table-union! ht1 ht2)
  (hash-table-for-each
   (lambda (key2 val2)
     (if (not (hash-table-contains? ht1 key2))
         (hash-table-set! ht1 key2 val2)))
   ht2)
  ht1)

(define (hash-table-intersection! ht1 ht2)
  (hash-table-for-each
   (lambda (key1 val1)
     (unless (hash-table-contains? ht2 key1)
       (hash-table-delete! ht1 key1)))
   ht1)
  ht1)

(define (hash-table-difference! ht1 ht2)
  (hash-table-for-each
   (lambda (key1 val1)
     (if (hash-table-contains? ht2 key1)
         (hash-table-delete! ht1 key1)))
   ht1)
  ht1)

(define (hash-table-xor! ht1 ht2)
  (hash-table-for-each
   (lambda (key2 val2)
     (if (hash-table-contains? ht1 key2)
         (hash-table-delete! ht1 key2)
         (hash-table-set! ht1 key2 val2)))
   ht2)
  ht1)
