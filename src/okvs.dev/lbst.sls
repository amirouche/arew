(library (okvs.dev lbst)
  (export make-lbst
          lbst-set
          lbst->alist)

  (import (chezscheme))

  ;; Balanced binary tree based on Log-Balanced Search Trees (LBST)
  ;; and slib's wttree.
  ;;
  ;; ref: A new method for balancing binary search trees, S. Roura
  ;; ref: https://scholar.google.fr/scholar?cluster=16806430159882137269

  (define-record-type <lbst>
    (make-lbst% root)
    lbst?
    (root lbst-root))

  (define-record-type <node>
    (make-node key value size bytes left right)
    node?
    (key node-key)
    (value node-value)
    (size node-size%)
    (bytes node-bytes%)
    (left node-left)
    (right node-right))

  (define (node-size maybe-node)
    (if maybe-node (node-size% maybe-node) 0))

  (define (node-bytes maybe-node)
    (if maybe-node (node-bytes% maybe-node) 0))

  (define (make-lbst)
    (make-lbst% #f))

  (define (lbst-empty? lbst)
    (not (lbst-root lbst)))

  (define (log2<? a b)
    ;; This procedure as the same truth table as the procedure that
    ;; compares the position of the most significant bit that is set.
    ;; A is less than B if A's most significant bit set is at a lower
    ;; position, than the most significant bit set in B.
    (and (fx<? a b) (fx<? (fxarithmetic-shift-left (fxand a b) 1) b)))

  (define (too-big? a b)
    (log2<? a (fxarithmetic-shift-right b 1)))

  (define (single-rotation? a b)
    (not (log2<? b a)))

  (define (node-join key value left right)
    (make-node key
               value
               (fx+ (node-size left)
                    (node-size right)
                    1)
               (fx+ (node-bytes left)
                    (node-bytes right)
                    (bytevector-length key)
                    (bytevector-length value))
               left
               right))

  (define (single-left-rotation key value left right)
    (node-join (node-key right)
               (node-value right)
               (node-join key value left (node-left right))
               (node-right right)))

  (define (double-left-rotation key value left right)
    (node-join (node-key (node-left right))
               (node-value (node-left right))
               (node-join key value left (node-left (node-left right)))
               (node-join (node-key right)
                          (node-value right)
                          (node-right (node-left right))
                          (node-right right))))

  (define (single-right-rotation key value left right)
    (node-join (node-key left)
               (node-value left)
               (node-left left)
               (node-join key value (node-right left) right)))

  (define (double-right-rotation key value left right)
    (node-join (node-key (node-right left))
               (node-value (node-right left))
               (node-join (node-key left)
                          (node-value left)
                          (node-left left)
                          (node-left (node-right left)))
               (node-join key value (node-right (node-right left)) right)))

  (define (node-rebalance key value left right)
    (cond
     ((too-big? (node-size left) (node-size right))
      ;; right is too big
      (if (single-rotation? (node-size (node-left right))
                            (node-size (node-right right)))
          (single-left-rotation key value left right)
          (double-left-rotation key value left right)))
     ((too-big? (node-size right) (node-size left))
      ;; left is too big
      (if (single-rotation? (node-size (node-right left))
                            (node-size (node-left left)))
          (single-right-rotation key value left right)
          (double-right-rotation key value left right)))
     (else (make-node key
                      value
                      (fx+ (node-size left)
                           (node-size right)
                           1)
                      (fx+ (node-bytes left)
                           (node-bytes right)
                           (bytevector-length key)
                           (bytevector-length value))
                      left
                      right))))

  (define (bytevector-lexicographic-compare bytevector other)
    ;; Lexicographic comparison between bytevector and other:
    ;;
    ;; - if bytevector < other, returns 'smaller;
    ;; - if bytevector = other, returns 'equal;
    ;; - if bytevector > other, returns 'bigger;
    ;;
    (let ((end (min (bytevector-length bytevector)
                    (bytevector-length other))))
      (let loop ((index 0))
        (if (fxzero? (- end index))
            (if (fx=? (bytevector-length bytevector)
                      (bytevector-length other))
                'equal
                (if (fx<? (bytevector-length bytevector)
                          (bytevector-length other))
                    'smaller
                    'bigger))
            (let ((delta (fx- (bytevector-u8-ref bytevector index)
                              (bytevector-u8-ref other index))))
              (if (fxzero? delta)
                  (loop (fx+ 1 index))
                  (if (fxnegative? delta)
                      'smaller
                      'bigger)))))))

  (define (if3 bytevector other smaller equal bigger)
    (case (bytevector-lexicographic-compare bytevector other)
      (smaller (smaller))
      (equal (equal))
      (bigger (bigger))))

  (define (node-set node key value)

    (define (node-set-less)
      ;; KEY is smaller than current key, recurse left side
      (node-rebalance (node-key node)
                      (node-value node)
                      (node-set (node-left node) key value)
                      (node-right node)))

    (define (node-set-equal)
      ;; The current KEY is the one, create a new node with
      ;; associated VALUE.
      (make-node key
                 value
                 (fx+ (node-size (node-left node))
                      (node-size (node-right node))
                      1)
                 (fx+ (node-bytes (node-left node))
                      (node-bytes (node-right node))
                      (bytevector-length key)
                      (bytevector-length value))
                 (node-left node)
                 (node-right node)))

    (define (node-set-more)
      ;; KEY is bigger than current key, recurse right side
      (node-rebalance (node-key node)
                      (node-value node)
                      (node-left node)
                      (node-set (node-right node) key value)))

    (if (not node)
        (make-node key value 1 (fx+ (bytevector-length key) (bytevector-length value)) #f #f)
        (if3 key (node-key node)
             node-set-less
             node-set-equal
             node-set-more)))

  (define (lbst-set lbst key value)
    (make-lbst% (node-set (lbst-root lbst)
                          key
                          value)))

  (define (lbst->alist lbst)
    (define (node->alist node)
      (append (if (node-left node) (node->alist (node-left node)) '())
              (list (cons (node-key node) (node-value node)))
              (if (node-right node) (node->alist (node-right node)) '())))

    (node->alist (lbst-root lbst))))
