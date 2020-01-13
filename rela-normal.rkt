#lang racket

(require racket/format)

; we need this for tree structure traversing
(define (map-recur proc x)
  (define (map-one item)
    (if (list? item) (map map-one item) (proc item)))
  (map map-one x))

; basic table
(struct rl-table (name columns tuples))

; interface of dynamic method dispatch
(struct rl-iter-procs (get next test rewind name columns))

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
         [rewind-proc (rl-iter-procs-rewind iter)])
    (rl-iter (rewind-proc repr) procs)))

(define (rl-iter-name iter)
  (let ([repr (rl-iter-repr iter)]
        [name-proc (rl-iter-procs-name (rl-iter-procset iter))])
    (name-proc repr)))

(define (rl-iter-columns iter)
  (let ([repr (rl-iter-repr iter)]
        [columns-proc (rl-iter-procs-columns (rl-iter-procset iter))])
    (columns-proc repr)))

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
      (if (null? cur-tuples)
          (error "iterator already at end")
          (rl-basic-iter table (cdr cur-tuples)))))
  (define (rl-basic-iter-test basic-iter)
    (let ([cur-tuples (rl-basic-iter-cur-tuples basic-iter)])
      (null? cur-tuples)))
  (define (rl-basic-iter-rewind basic-iter)
    (let ([table (rl-basic-iter-table basic-iter)])
      (rl-basic-iter table (rl-phantom-tuple))))
  (define (rl-basic-iter-name basic-iter)
    (let ([table (rl-basic-iter-table basic-iter)])
      (rl-table-name table)))
  (define (rl-basic-iter-columns basic-iter)
    (let ([table (rl-basic-iter-table basic-iter)])
      (rl-table-columns table)))
  (rl-iter #|repr|#  (rl-basic-iter table (rl-phantom-tuple))
           #|procs|# (rl-iter-procs rl-basic-iter-get
                                    rl-basic-iter-next
                                    rl-basic-iter-test
                                    rl-basic-iter-rewind
                                    rl-basic-iter-name
                                    rl-basic-iter-columns)))

(struct rl-cartesian-iter (base1 base2))

(define (rl-build-cartesian-iter base1 base2)
  (define (rl-cartesian-iter-get cartesian-iter)
    (append (rl-iter-get base1) (rl-iter-get base2)))
  (define (rl-cartesian-iter-next cartesian-iter)
    (let ([base1 (rl-cartesian-iter-base1 cartesian-iter)]
          [base2 (rl-cartesian-iter-base2 cartesian-iter)])
      (cond [(rl-iter-test base1) (error "iterator at end")]
            [(rl-iter-test base2) (rl-cartesian-iter (rl-iter-next base1) 
                                                     (rl-iter-next (rl-iter-rewind base2)))]
            [else (rl-cartesian-iter base1 (rl-iter-next base2))])))
  (define (rl-cartesian-iter-test cartesian-iter)
    (let ([base1 (rl-cartesian-iter-base1 cartesian-iter)])
      (rl-iter-test base1)))
  (define (rl-cartesian-iter-rewind cartesian-iter)
    (let ([base1 (rl-cartesian-iter-base1 cartesian-iter)]
          [base2 (rl-cartesian-iter-base2 cartesian-iter)])
      (rl-cartesian-iter (rl-iter-next (rl-iter-rewind base1))
                         (rl-iter-rewind base2))))
  (define (rl-cartesian-iter-name cartesian-iter)
    (let ([base1 (rl-cartesian-iter-base1 cartesian-iter)]
          [base2 (rl-cartesian-iter-base2 cartesian-iter)])
      (string-append "<" (rl-iter-name base1) " * " (rl-iter-name base2) ">")))
  (define (rl-cartesian-iter-columns cartesian-iter)
    (let ([base1 (rl-cartesian-iter-base1 cartesian-iter)]
          [base2 (rl-cartesian-iter-base2 cartesian-iter)])
      (append (rl-cartesian-iter-columns base1) (rl-cartesian-iter-columns base2))))
  (rl-iter #|repr|#  (rl-cartesian-iter base1 (rl-iter-next base2))
           #|procs|# (rl-iter-procs rl-cartesian-iter-get
                                    rl-cartesian-iter-next
                                    rl-cartesian-iter-test
                                    rl-cartesian-iter-rewind
                                    rl-cartesian-iter-name
                                    rl-cartesian-iter-columns)))

(struct rl-projection-iter (base))

(define (rl-build-projection-iter base column-names)
  (define (rl-build-all-column-selectors table-columns column-names)
    (define (rl-build-column-selector table-columns column-name)
      (let ([column-index (index-of table-columns column-name)])
        (if (equal? column-index false)
            (error "column does not exist")
            (lambda (tuple) (list-ref tuple column-index)))))
    (map (lambda (column-name) (rl-build-column-selector table-columns column-name)) 
         column-names))
  (let* ([base-columns (rl-iter-columns base)]
         [all-column-selectors (rl-build-all-column-selectors base-columns column-names)])
    (define (rl-projection-iter-get projection-iter)
      (let ([cur-tuple (rl-iter-get projection-iter)])
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
    (define (rl-projection-iter-name projection-iter)
      (let ([base (rl-projection-iter-base projection-iter)])
        (string-append "PI(" rl-projection-iter-name "; " (~a) ")")))
    (define (rl-projection-iter-columns projection-iter)
      column-names)
    (rl-iter #|repr|#  (rl-projection-iter base)
             #|procs|# (rl-iter-procs rl-projection-iter-get
                                      rl-projection-iter-next
                                      rl-projection-iter-test
                                      rl-projection-iter-rewind
                                      rl-projection-iter-name
                                      rl-projection-iter-columns))))

(struct rl-select-iter (base))

(struct rl-indexed-select-iter (base))

(struct rl-equiv-join-iter (iter1 table2))
