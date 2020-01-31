#lang racket

(require racket/format)
(require racket/trace)
(require srfi/2)
(require srfi/43)
(require errortrace)
(require rackunit)

; unimplemented stuffs and todos
(define (unimplemented) (error "unimplemented"))

; distance function
(define (distance x y) (abs (- x y)))

; for marking unused parameters
(define (unused x . xs) (void))

; sometimes useful for debugging
(define (id x) x)

(define (display-n-ret x)
  (display x)
  x)

(define (displayln-n-ret x)
  (displayln x)
  x)

; procedure version of builtin special form `and`/`or`, does NOT have short circuit eval!
(define (and-proc x y) (and x y))
(define (or-proc x y) (or x y))

; making eval work
(current-namespace (make-base-namespace))

; a shorthand
(define (not-null? blah) (not (null? blah)))

; hard-coded comparators, considered nasty, but out data types are limited to numbers and strings
(define (less? x y)
  (cond [(number? x) (< x y)]
        [(string? x) (string<? x y)]))

(define (greater? x y)
  (cond [(number? x) (> x y)]
        [(string? x) (string>? x y)]))

(define (less-or-eq? x y) (or (less? x y) (equal? x y)))

(define (greater-or-eq? x y) (or (greater? x y) (equal? x y)))


(check-false (less-or-eq? 13 12))
(check-true (less-or-eq? 12 12))
(check-true (less-or-eq? 11 12))

(check-true (greater-or-eq? 13 12))
(check-true (greater-or-eq? 12 12))
(check-false (greater-or-eq? 11 12))

; ordered indexing mechanism implemented with sorted vectors
(define (list->svec list-of-pairs)
  (vector-sort (list->vector list-of-pairs)
               (lambda (p1 p2) (less? (car p1) (car p2)))))

(define (svec-get svec key)
  (define (cmp x1 x2)
    (cond [(pair? x1) (cmp (car x1) x2)]
          [(pair? x2) (cmp x1 (car x2))]
          [(equal? x1 x2) 0]
          [(less? x1 x2) -1]
          [(greater? x1 x2) 1]))
  (let ([subscript (vector-binary-search svec key cmp)])
    (if (false? subscript)
        false
        (cdr (vector-ref svec subscript)))))

(define (svec-ref svec idx)
  (if (>= idx (vector-length svec))
      #f
      (vector-ref svec idx)))

(define (svec-lower-bound svec key)
  (define (lower-bound-int idx1 idx2)
    (let ([idx-distance (distance idx1 idx2)])
      (if (= idx-distance 0) idx1
          (let* ([step (quotient (distance idx1 idx2) 2)]
                 [mid (+ idx1 step)]
                 [mid-kv (vector-ref svec mid)]
                 [mid-key (car mid-kv)])
            (cond [(greater? mid-key key) (lower-bound-int idx1 mid)]
                  [(equal? mid-key key) mid]
                  [(less? mid-key key) (lower-bound-int (+ mid 1) idx2)])))))
  (let* ([svec-length (vector-length svec)]
         [lower-bound-index (lower-bound-int 0 svec-length)])
    (cond [(false? lower-bound-index) false]
          [(= lower-bound-index svec-length) false]
          [else lower-bound-index])))

; testcase for sorted vectors
(define svec-1
  (list->svec
    (list (cons 2 "CKX")
          (cons 1 "CTZ")
          (cons 3 "GZS")
          (cons 5 "WXB"))))

(check-equal? (svec-get svec-1 2) "CKX")
(check-equal? (svec-get svec-1 3) "GZS")
(check-equal? (svec-get svec-1 5) "WXB")
(check-equal? (svec-get svec-1 1) "CTZ")
(check-false (svec-get svec-1 4))

(check-equal? (svec-lower-bound svec-1 4) 3)
(check-equal? (svec-lower-bound svec-1 5) 3)
(check-equal? (svec-lower-bound svec-1 3) 2)
(check-equal? (svec-lower-bound svec-1 2) 1)
(check-equal? (svec-lower-bound svec-1 1) 0)
(check-equal? (svec-lower-bound svec-1 0) 0)
(check-equal? (svec-lower-bound svec-1 -1) 0)

(define svec-2
  (list->svec
    (list (cons 1 1)
          (cons 2 2)
          (cons 4 4)
          (cons 5 5)
          (cons 6 6)
          (cons 9 9))))

(check-equal? (svec-lower-bound svec-2 0) 0)
(check-equal? (svec-lower-bound svec-2 1) 0)
(check-equal? (svec-lower-bound svec-2 2) 1)
(check-equal? (svec-lower-bound svec-2 3) 2)
(check-equal? (svec-lower-bound svec-2 4) 2)
(check-equal? (svec-lower-bound svec-2 5) 3)
(check-equal? (svec-lower-bound svec-2 6) 4)
(check-equal? (svec-lower-bound svec-2 7) 5)
(check-equal? (svec-lower-bound svec-2 8) 5)
(check-equal? (svec-lower-bound svec-2 9) 5)
(check-false (svec-lower-bound svec-2 10))

; we need this for tree structure traversing
(define (map-recur proc x)
  (define (map-one item)
    (if (list? item) (map map-one item) (proc item)))
  (map map-one x))

; basic table
(struct rl-table (name columns tuples indexed-columns index-maps))

; used in many situations
(define (rl-build-column-selector table-columns column-name)
  (let ([column-index (index-of table-columns column-name)])
    (if (equal? column-index false)
        (error (string-append "column " column-name " does not exist"))
        (lambda (tuple) (list-ref tuple column-index)))))

(define (rl-build-table name columns tuples . indexed-columns)
  (define (rl-build-one-index indexed-column)
    (let* ([column-selector (rl-build-column-selector columns indexed-column)]
           [tuples-transformer (lambda (tuple) (cons (column-selector tuple) tuple))])
      (list->svec (map tuples-transformer tuples))))
  (if (null? indexed-columns)
      (rl-table name 
                columns
                tuples
                null
                null)
      (rl-table name
                columns
                tuples
                indexed-columns
                (map rl-build-one-index indexed-columns))))

; for building name references in raw expressions
(struct rl-ref (name))

(define (display-table table)
  (define (display-tuples tuples)
    (if (null? (cdr tuples))
        (begin
          (display " ")
          (display (car tuples)))
        (begin
          (display " ")
          (displayln (car tuples))
          (display-tuples (cdr tuples)))))
  (begin (displayln (rl-table-name table))
         (display " ") (displayln (rl-table-columns table))
         (display-tuples (rl-table-tuples table))))

; interface of dynamic method dispatch
(struct rl-iter-procs (get next test rewind name columns indexable-check
                       index index-lower-bound index-ref))

(struct rl-iter (repr procset))

; generic methods
(define (rl-iter-get iter)
  (let ([repr (rl-iter-repr iter)]
        [get-proc (rl-iter-procs-get (rl-iter-procset iter))])
    (get-proc repr)))

(define (rl-iter-next iter)
  (let* ([repr (rl-iter-repr iter)]
         [procs (rl-iter-procset iter)]
         [next-proc (rl-iter-procs-next procs)])
    (rl-iter (next-proc repr) procs)))

(define (rl-iter-test iter)
  (let ([repr (rl-iter-repr iter)]
        [test-proc (rl-iter-procs-test (rl-iter-procset iter))])
    (test-proc repr)))

(define (rl-iter-rewind iter)
  (let* ([repr (rl-iter-repr iter)]
         [procs (rl-iter-procset iter)]
         [rewind-proc (rl-iter-procs-rewind procs)])
    (rl-iter (rewind-proc repr) procs)))

(define (rl-iter-name iter)
  (let ([name-proc (rl-iter-procs-name (rl-iter-procset iter))])
    (name-proc)))

(define (rl-iter-columns iter)
  (let ([columns-proc (rl-iter-procs-columns (rl-iter-procset iter))])
    (columns-proc)))

(define (rl-iter-indexable? iter indexed-field)
  (let* ([iter-indexable-check (rl-iter-procs-indexable-check iter)])
    (and (not-null? iter-indexable-check)
         (iter-indexable-check indexed-field))))

(define (rl-iter-index iter column-name)
  (let* ([procs (rl-iter-procset iter)]
         [index (rl-iter-procs-index procs)])
    (index column-name)))

(define (rl-iter-index-lower-bound iter column-name)
  (let* ([procs (rl-iter-procset iter)]
         [index-lower-bound (rl-iter-procs-index-lower-bound procs)])
    (index-lower-bound column-name)))

(define (rl-iter-index-ref iter column-name)
  (let* ([procs (rl-iter-procset iter)]
         [index-ref (rl-iter-procs-index-ref procs)])
    (index-ref column-name)))

; phantom tuple, used when implementing basic-iter
(struct rl-phantom-tuple ())

(struct rl-basic-iter (table cur-tuples))

(define (rl-build-basic-iter table)
  (define (rl-basic-iter-get basic-iter)
    (let ([cur-tuples (rl-basic-iter-cur-tuples basic-iter)])
      (cond [(rl-phantom-tuple? cur-tuples) (error "iterator not ready")]
            [(null? cur-tuples) (error "iterator at end")]
            [else (car cur-tuples)])))
  (define (rl-basic-iter-next basic-iter)
    (let ([table (rl-basic-iter-table basic-iter)]
          [cur-tuples (rl-basic-iter-cur-tuples basic-iter)])
      (cond [(null? cur-tuples) (error "iterator already at end")]
            [(rl-phantom-tuple? cur-tuples) (rl-basic-iter table (rl-table-tuples table))]
            [(rl-basic-iter table (cdr cur-tuples))])))
  (define (rl-basic-iter-test basic-iter)
    (let ([cur-tuples (rl-basic-iter-cur-tuples basic-iter)])
      (null? cur-tuples)))
  (define (rl-basic-iter-rewind basic-iter)
    (let ([table (rl-basic-iter-table basic-iter)])
      (rl-basic-iter table (rl-phantom-tuple))))
  (define (rl-basic-iter-name) (rl-table-name table))
  (define (rl-basic-iter-columns) (rl-table-columns table))
  (define (rl-basic-iter-indexable-check column-name)
    (let* ([indexed-columns (rl-table-indexed-columns table)])
      (if (null? indexed-columns)
          false
          (not (false? (index-of indexed-columns column-name))))))
  (define (rl-basic-iter-index column-name)
    (let* ([indexed-columns (rl-table-indexed-columns table)]
           [index-maps (rl-table-index-maps table)]
           [subscript (index-of indexed-columns column-name)])
      (if (false? subscript)
          (error "column index does not exist")
          (lambda (column-value) (svec-get (list-ref index-maps subscript) column-value)))))
  (define (rl-basic-iter-index-lower-bound column-name)
    (let* ([indexed-columns (rl-table-indexed-columns table)]
           [index-maps (rl-table-index-maps table)]
           [subscript (index-of indexed-columns column-name)])
      (if (false? subscript)
          (error "column index does not exist")
          (lambda (column-value) (svec-lower-bound (list-ref index-maps subscript) column-value)))))
  (define (rl-basic-iter-index-ref column-name)
    (let* ([indexed-columns (rl-table-indexed-columns table)]
           [index-maps (rl-table-index-maps table)]
           [subscript (index-of indexed-columns column-name)])
      (if (false? subscript)
          (error "column index does not exist")
          (lambda (idx) (svec-ref (list-ref index-maps subscript) idx)))))
  (rl-iter #|repr|#  (rl-basic-iter table (rl-phantom-tuple))
           #|procs|# (rl-iter-procs rl-basic-iter-get
                                    rl-basic-iter-next
                                    rl-basic-iter-test
                                    rl-basic-iter-rewind
                                    rl-basic-iter-name
                                    rl-basic-iter-columns
                                    rl-basic-iter-indexable-check
                                    rl-basic-iter-index
                                    rl-basic-iter-index-lower-bound
                                    rl-basic-iter-index-ref)))

(struct rl-cartesian-iter (base1 base2))

(define (rl-build-cartesian-iter base1 base2)
  (define (rl-cartesian-iter-get cartesian-iter)
    (let ([base1 (rl-cartesian-iter-base1 cartesian-iter)]
          [base2 (rl-cartesian-iter-base2 cartesian-iter)])
      (append (rl-iter-get base1) (rl-iter-get base2))))
  (define (rl-cartesian-iter-next cartesian-iter)
    (define (rl-cartesian-next-post-check cartesian-iter)
      (let ([base1 (rl-cartesian-iter-base1 cartesian-iter)]
            [base2 (rl-cartesian-iter-base2 cartesian-iter)])
        (if (rl-iter-test base2)
            (rl-cartesian-iter-next cartesian-iter)
            cartesian-iter)))
    (let ([base1 (rl-cartesian-iter-base1 cartesian-iter)]
          [base2 (rl-cartesian-iter-base2 cartesian-iter)])
      (cond [(rl-iter-test base1) (error "cartesian iterator at end")]
            [(rl-iter-test base2) (rl-cartesian-iter (rl-iter-next base1)
                                                     (rl-iter-next (rl-iter-rewind base2)))]
            [else (rl-cartesian-next-post-check (rl-cartesian-iter base1 (rl-iter-next base2)))])))
  (define (rl-cartesian-iter-test cartesian-iter)
    (let ([base1 (rl-cartesian-iter-base1 cartesian-iter)])
      (rl-iter-test base1)))
  (define (rl-cartesian-iter-rewind cartesian-iter)
    (let ([base1 (rl-cartesian-iter-base1 cartesian-iter)]
          [base2 (rl-cartesian-iter-base2 cartesian-iter)])
      (rl-cartesian-iter (rl-iter-next (rl-iter-rewind base1))
                         (rl-iter-rewind base2))))
  (define (rl-cartesian-iter-name)
    (string-append "<" (rl-iter-name base1) " * " (rl-iter-name base2) ">"))
  (define (rl-cartesian-iter-columns)
      (append (rl-iter-columns base1) (rl-iter-columns base2)))
  (rl-iter #|repr|#  (rl-cartesian-iter (rl-iter-next base1) base2)
           #|procs|# (rl-iter-procs rl-cartesian-iter-get
                                    rl-cartesian-iter-next
                                    rl-cartesian-iter-test
                                    rl-cartesian-iter-rewind
                                    rl-cartesian-iter-name
                                    rl-cartesian-iter-columns
                                    null
                                    null
                                    null
                                    null)))

(struct rl-projection-iter (base))

(define (rl-build-projection-iter base column-names)
  (define (rl-build-all-column-selectors table-columns column-names)
    (map (lambda (column-name) (rl-build-column-selector table-columns column-name)) 
         column-names))
  (let* ([base-columns (rl-iter-columns base)]
         [all-column-selectors (rl-build-all-column-selectors base-columns column-names)])
    (define (rl-projection-iter-get projection-iter)
      (let ([cur-tuple (rl-iter-get (rl-projection-iter-base projection-iter))])
        (map (lambda (f) (f cur-tuple))
             all-column-selectors)))
    (define (rl-projection-iter-next projection-iter)
      (let ([base (rl-projection-iter-base projection-iter)])
        (rl-projection-iter (rl-iter-next base))))
    (define (rl-projection-iter-test projection-iter)
      (let ([base (rl-projection-iter-base projection-iter)])
        (rl-iter-test base)))
    (define (rl-projection-iter-rewind projection-iter)
      (let ([base (rl-projection-iter-base projection-iter)])
        (rl-projection-iter (rl-iter-rewind base))))
    (define (rl-projection-iter-name)
      (string-append "PI<" 
                     (rl-iter-name base)
                     "; "
                     (~a column-names)
                     ">"))
    (define (rl-projection-iter-columns) column-names)
    (rl-iter #|repr|#  (rl-projection-iter base)
             #|procs|# (rl-iter-procs rl-projection-iter-get
                                      rl-projection-iter-next
                                      rl-projection-iter-test
                                      rl-projection-iter-rewind
                                      rl-projection-iter-name
                                      rl-projection-iter-columns
                                      null
                                      null
                                      null
                                      null))))

(struct rl-select-iter (base))

(define (rl-build-select-iter base raw-condition)
  (define (raw-expr->string raw-expr)
    (define (rl-ref-replace raw-expr)
      (map-recur (lambda (x) (if (rl-ref? x) (rl-ref-name x) x))
                 raw-expr))
    (string-replace (~a (rl-ref-replace raw-expr)) "procedure:" ""))
  (define (rl-compile-expr table-columns incomplete-expr)
    (define (rl-compile-item item tuple)
      (cond [(rl-ref? item)
             (let ([column-name (rl-ref-name item)])
               (list (rl-build-column-selector table-columns column-name) 
                     (list (lambda () tuple))))]
            [(list? item) (rl-compile-list item tuple)]
            [else item]))
    (define (rl-compile-list the-list tuple)
      (map (lambda (item) (rl-compile-item item tuple)) the-list))
    (lambda (tuple) (eval (map (lambda (item) (rl-compile-item item tuple))
                               incomplete-expr))))
  (let ([compiled-condition (rl-compile-expr (rl-iter-columns base) raw-condition)])
    (define (rl-select-iter-get select-iter)
      (let ([base (rl-select-iter-base select-iter)])
        (rl-iter-get base)))
    (define (rl-select-iter-next select-iter)
      (define (rl-select-iter-next-intern base)
        (cond [(rl-iter-test base) base]
              [(compiled-condition (rl-iter-get base)) base]
              [else (rl-select-iter-next-intern (rl-iter-next base))]))
      (rl-select-iter (rl-select-iter-next-intern (rl-iter-next (rl-select-iter-base select-iter)))))
    (define (rl-select-iter-test select-iter)
      (rl-iter-test (rl-select-iter-base select-iter)))
    (define (rl-select-iter-rewind select-iter)
      (rl-select-iter (rl-iter-rewind (rl-select-iter-base select-iter))))
    (define (rl-select-iter-name)
      (string-append "SIGMA<"
                     (rl-iter-name base)
                     ";"
                     (~a (raw-expr->string raw-condition))
                     ">"))
    (define (rl-select-iter-columns) (rl-iter-columns base))
    (rl-iter #|repr|#  (rl-select-iter base)
             #|procs|# (rl-iter-procs rl-select-iter-get
                                      rl-select-iter-next
                                      rl-select-iter-test
                                      rl-select-iter-rewind
                                      rl-select-iter-name
                                      rl-select-iter-columns
                                      null
                                      null
                                      null
                                      null))))

(struct rl-equiv-join-iter (iter1))

(define (rl-build-equiv-join-iter iter1 iter2 iter1-column iter2-column)
  (let ([iter1-column-selector (rl-build-column-selector (rl-iter-columns iter1) iter1-column)]
        [iter2-index (rl-iter-index iter2 iter2-column)])
    (define (rl-equiv-join-iter-get equiv-join-iter)
      (let* ([iter1 (rl-equiv-join-iter-iter1 equiv-join-iter)]
             [iter1-tuple (rl-iter-get iter1)]
             [iter1-key (iter1-column-selector iter1-tuple)]
             [iter2-tuple (iter2-index iter1-key)])
          (append iter1-tuple iter2-tuple)))
    (define (rl-equiv-join-iter-next equiv-join-iter)
      (define (rl-equiv-join-iter-next-int prim-iter)
        (if (rl-iter-test prim-iter)
            prim-iter
            (let* ([iter1-tuple (rl-iter-get prim-iter)]
                   [iter1-key (iter1-column-selector iter1-tuple)]
                   [iter2-tuple (iter2-index iter1-key)])
              (if (false? iter2-tuple)
                  (rl-equiv-join-iter-next-int (rl-iter-next prim-iter))
                  prim-iter))))
      (let ([iter1 (rl-equiv-join-iter-iter1 equiv-join-iter)])
        (rl-equiv-join-iter (rl-equiv-join-iter-next-int (rl-iter-next iter1)))))
    (define (rl-equiv-join-iter-test equiv-join-iter)
      (let ([iter1 (rl-equiv-join-iter-iter1 equiv-join-iter)])
        (rl-iter-test iter1)))
    (define (rl-equiv-join-iter-rewind equiv-join-iter)
      (let ([iter1 (rl-equiv-join-iter-iter1 equiv-join-iter)])
        (rl-equiv-join-iter (rl-iter-rewind iter1))))
    (define (rl-equiv-join-iter-name)
      (string-append "<" (rl-iter-name iter1) " |X| " (rl-iter-name iter2) ">"))
    (define (rl-equiv-join-iter-columns)
      (append (rl-iter-columns iter1) (rl-iter-columns iter2)))
    (define (rl-equiv-join-iter-indexable-check column-name)
        (rl-iter-indexable? iter1 column-name))
    (define (rl-equiv-join-iter-index column-name)
      (let ([iter1-index (rl-iter-index iter1)])
        (lambda (column-value)
          (and-let* ([iter1-tuple (iter1-index column-value)]
                     [iter1-key (iter1-column-selector iter1-tuple)]
                     [iter2-tuple (iter2-index iter1-key)])
                    (append iter1-tuple iter2-tuple)))))
    (define (rl-equiv-join-iter-index-lower-bound column-name)
      (let ([iter1-index-lower-bound (rl-iter-index-lower-bound iter1)])
        (lambda (column-name) (iter1-index-lower-bound column-name))))
    (define (rl-equiv-join-iter-index-ref column-name)
      (let ([iter1-index-ref (rl-iter-index-ref iter1)])
        (lambda (idx)
          (and-let* ([iter1-tuple (iter1-index-ref idx)]
                     [iter1-key (iter1-column-selector iter1-tuple)]
                     [iter2-tuple (iter2-index)])
             (append iter1-tuple iter2-tuple)))))
    (rl-iter #|repr|#  (rl-equiv-join-iter iter1)
             #|procs|# (rl-iter-procs rl-equiv-join-iter-get
                                      rl-equiv-join-iter-next
                                      rl-equiv-join-iter-test
                                      rl-equiv-join-iter-rewind
                                      rl-equiv-join-iter-name
                                      rl-equiv-join-iter-columns
                                      rl-equiv-join-iter-indexable-check
                                      rl-equiv-join-iter-index
                                      rl-equiv-join-iter-index-lower-bound
                                      rl-equiv-join-iter-index-ref))))

(struct rl-regular-ranged-iter (base))

(struct rl-indexed-ranged-iter (subscript))

(define (rl-build-regular-ranged-iter base column lower-bound upper-bound)
  (define (bound-check value)
    (and (if (not-null? lower-bound)
             (greater-or-eq? value lower-bound)
             true)
         (if (not-null? upper-bound)
             (less-or-eq? value upper-bound)
             true)))
  (let* ([base-columns (rl-iter-columns base)]
         [column-selector (rl-build-column-selector base-columns column)])
    (define (rl-ranged-iter-get ranged-iter)
      (let ([base (rl-regular-ranged-iter-base ranged-iter)])
        (rl-iter-get base)))
    (define (rl-ranged-iter-next ranged-iter)
      (define (rl-ranged-iter-next-int base)
        (cond [(rl-iter-test base) base]
              [(bound-check (column-selector (rl-iter-get base))) base]
              [else (rl-ranged-iter-next-int (rl-iter-next base))]))
      (let* ([base (rl-regular-ranged-iter-base ranged-iter)]
             [base-next (rl-iter-next base)])
        (rl-regular-ranged-iter (rl-ranged-iter-next-int base-next))))
    (define (rl-ranged-iter-test ranged-iter)
      (let ([base (rl-regular-ranged-iter-base ranged-iter)])
        (rl-iter-test base)))
    (define (rl-ranged-iter-rewind ranged-iter)
      (rl-regular-ranged-iter (rl-iter-rewind (rl-regular-ranged-iter-base ranged-iter))))
    (define (rl-ranged-iter-name)
      (string-append "RANGE<" (rl-iter-name base) "; "
                     column "; "
                     (~a lower-bound) "; "
                     (~a upper-bound) ">"))
    (define (rl-ranged-iter-columns)
      (rl-iter-columns base))
    (rl-iter #|repr|#  (rl-regular-ranged-iter base)
             #|procs|# (rl-iter-procs rl-ranged-iter-get
                                      rl-ranged-iter-next
                                      rl-ranged-iter-test
                                      rl-ranged-iter-rewind
                                      rl-ranged-iter-name
                                      rl-ranged-iter-columns
                                      null
                                      null
                                      null
                                      null))))

(define (rl-build-indexed-ranged-iter base column lower-bound upper-bound)
  (let* ([index-lower-bound (rl-iter-index-lower-bound base column)]
         [index-ref (rl-iter-index-ref base column)]
         [first-subscript (if (null? lower-bound)
                              0
                              (index-lower-bound lower-bound))])
    (define (rl-ranged-iter-get ranged-iter)
      (let ([subscript (rl-indexed-ranged-iter-subscript ranged-iter)])
        (cdr (index-ref subscript))))
    (define (rl-ranged-iter-next ranged-iter)
      (define (rl-ranged-iter-next-int subscript)
        (and-let* ([kvpair (index-ref subscript)]
                   [key (car kvpair)])
          (cond [(and (not-null? upper-bound) (greater? key upper-bound)) false]
                [else subscript])))
      (let ([subscript (rl-indexed-ranged-iter-subscript ranged-iter)])
        (rl-indexed-ranged-iter
         (cond [(null? subscript) first-subscript]
               [(false? subscript) (error "iterator already at end")]
               [else (rl-ranged-iter-next-int (+ subscript 1))]))))
    (define (rl-ranged-iter-test ranged-iter)
      (let ([subscript (rl-indexed-ranged-iter-subscript ranged-iter)])
        (false? subscript)))
    (define (rl-ranged-iter-rewind ranged-iter)
      (unused ranged-iter)
      (rl-indexed-ranged-iter null))
    (define (rl-ranged-iter-name)
      (string-append "INDEXED-RANGE<" (rl-iter-name base) "; "
                     column "; "
                     (~a lower-bound) "; "
                     (~a upper-bound) ">"))
    (define (rl-ranged-iter-columns)
      (rl-iter-columns base))
    (rl-iter #|repr|#  (rl-indexed-ranged-iter null)
             #|procs|# (rl-iter-procs rl-ranged-iter-get
                                      rl-ranged-iter-next
                                      rl-ranged-iter-test
                                      rl-ranged-iter-rewind
                                      rl-ranged-iter-name
                                      rl-ranged-iter-columns
                                      null
                                      null
                                      null
                                      null))))

(define (rl-iter-traverse iter)
  (displayln (rl-iter-name iter))
  (displayln (~a (rl-iter-columns iter)))
  (define (rl-iter-traverse-int iter)
    (if (rl-iter-test iter)
        (displayln "")
        (begin (writeln (rl-iter-get iter))
               (rl-iter-traverse-int (rl-iter-next iter)))))
  (rl-iter-traverse-int (rl-iter-next iter)))

(struct rl-query-node (sub-nodes selections))

(struct rl-query (root-node projection))

(define (rl-preprocess fields from-tables where-clauses)
  (unimplemented))

(define players-table
  (rl-build-table "players-table"
                  '("pno" "pname" "pteam")
                  '((1 "QDU.Sumoon" "Qingdao University")
                    (2 "BUG.Chu1gda" "BUGaming")
                    (3 "ICE.1000" "Internal Compiler Error")
                    (4 "CHUK-SZ.ZYF" "CHinese University of HongKong (Shenzhen)")
                    (5 "ICE.Hoshino" "Internal Compilter Error"))
                  "pno" "pname"))

(define tools-table
  (rl-build-table "tools-table"
                  '("tno" "tname" "tvendor")
                  '((1 "Dev-CPP" "ACM-ICPC")
                    (2 "Intellij-IDEA" "Jetbrains")
                    (3 "QtCreator" "Digia")
                    (4 "CLion" "Jetbrains"))
                  "tno" "tname"))
  
(define players-tools-table
  (rl-build-table "players-tools-table"
                  '("pno1" "tno1")
                  '((1 1)
                    (2 3)
                    (3 2)
                    (4 4)
                    (5 2))
                  "pno1" "tno1"))

(define players-table-iter (rl-build-basic-iter players-table))

(writeln ((rl-iter-index players-table-iter "pno") 2))
(writeln ((rl-iter-index players-table-iter "pname") "ICE.1000"))
(displayln "")

(define tools-table-iter (rl-build-basic-iter tools-table))

(define players-tools-table-iter (rl-build-basic-iter players-tools-table))

(define pt-iter players-tools-table-iter)

(define jetbrains-tools-table-iter
  (rl-build-select-iter tools-table-iter (list equal? (rl-ref "tvendor") "Jetbrains")))

(define three-cartesian-iter
  (rl-build-cartesian-iter (rl-build-cartesian-iter players-table-iter
                                                    tools-table-iter)
                           players-tools-table-iter))

(define selected-iter1
  (rl-build-select-iter three-cartesian-iter (list = (rl-ref "tno") (rl-ref "tno1"))))

(define selected-iter2
  (rl-build-select-iter selected-iter1 (list = (rl-ref "pno") (rl-ref "pno1"))))

(define final-projection-iter
  (rl-build-projection-iter selected-iter2 (list "pno" "pname" "pteam" "tname" "tvendor")))

(define final-selected-iter
  (rl-build-select-iter final-projection-iter (list equal? (rl-ref "tvendor") "Jetbrains")))

(rl-iter-traverse players-table-iter)

(rl-iter-traverse jetbrains-tools-table-iter)

(rl-iter-traverse final-projection-iter)

(rl-iter-traverse final-selected-iter)

(define players-tools-equiv-join-iter
  (rl-build-equiv-join-iter
   (rl-build-equiv-join-iter pt-iter players-table-iter "pno1" "pno")
   tools-table-iter "tno1" "tno"))

(rl-iter-traverse players-tools-equiv-join-iter)

(define students-table
  (rl-build-table "students-table"
                  '("name" "score")
                  '(("dyj" 59)
                    ("ctz" 80)
                    ("lc" 70)
                    ("ly" 62)
                    ("lhz" 65)
                    ("glc" 55))
                  "score"))
(define students-table-iter
  (rl-build-basic-iter students-table))

(define students-table-ranged-iter1
  (rl-build-regular-ranged-iter students-table-iter "score" 60 79))

(define students-table-ranged-iter2
  (rl-build-indexed-ranged-iter students-table-iter "score" 60 79))

(define students-table-ranged-iter3
  (rl-build-regular-ranged-iter students-table-iter "score" null 79))

(define students-table-ranged-iter4
  (rl-build-indexed-ranged-iter students-table-iter "score" null 79))

(define students-table-ranged-iter5
  (rl-build-regular-ranged-iter students-table-iter "score" 60 null))

(define students-table-ranged-iter6
  (rl-build-indexed-ranged-iter students-table-iter "score" 60 null))

(rl-iter-traverse students-table-iter)

(rl-iter-traverse students-table-ranged-iter1)
(rl-iter-traverse students-table-ranged-iter2)
(rl-iter-traverse students-table-ranged-iter3)
(rl-iter-traverse students-table-ranged-iter4)
(rl-iter-traverse students-table-ranged-iter5)
(rl-iter-traverse students-table-ranged-iter6)
