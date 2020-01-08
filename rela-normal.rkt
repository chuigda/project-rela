#lang racket

(require racket/format)

(define (map-recur proc x)
  (define (map-one item)
    (if (list? item) (map map-one item) (proc item)))
  (map map-one x))

(struct rl-table (name columns tuples))

(struct rl-basic-iter (table cur-tuples))

(struct rl-select-iter (base condition))

(struct rl-equiv-join-iter (iter1 table2 attr1-selector attr2-selector))

(struct rl-cproduct-iter (base1 base2))

(struct rl-pi-iter (base selectors))

(struct rl-iter (repr get next test))

(struct rl-phantom-tuple ())

(define (rl-build-basic-iter table)
  (define (rl-basic-iter-get basic-iter)
    (let ([current-tuple (rl-basic-iter-cur-tuples basic-iter)])
      (cond [(rl-phantom-tuple? current-tuple) (error "iterator not ready")]
            [(null? current-tuple) (error "iterator at end")]
            [else current-tuple])))
  (define (rl-basic-iter-next basic-iter)
    (let ([iter-table (rl-basic-iter-table basic-iter)]
          [current-tuples (rl-basic-iter-cur-tuples basic-iter)])
      (rl-basic-iter iter-table
                     (cond [(rl-phantom-tuple? current-tuples) (rl-table-tuples iter-table)]
                           [(null? current-tuples) (error "iterator at end")]
                           [else (cdr current-tuples)]))))
  (define (rl-basic-iter-at-end? basic-iter)
    (null (cdr (rl-basic-iter-cur-tuples basic-iter))))
  (rl-iter (rl-basic-iter table (rl-phantom-tuple))
           rl-basic-iter-get
           rl-basic-iter-next
           rl-basic-iter-at-end?))

(define (rl-build-select-iter base condition)
  (let ([base-get (rl-iter-get base)]
        [base-next (rl-iter-next base)]
        [base-test (rl-iter-test base)])
    (define (rl-select-iter-get select-iter) (error "unimplemented"))
    (define (rl-select-iter-next select-iter) (error "unimplemented"))
    (define (rl-select-iter-at-end? select-iter) (error "unimplemented"))
    (rl-iter (rl-select-iter base condition)
             rl-select-iter-get
             rl-select-iter-next
             rl-select-iter-at-end?)))

(define (rl-build-cproduct-iter base1 base2)
  (let ([base1-get (rl-iter-get base1)]
        [base1-next (rl-iter-next base1)]
        [base1-test (rl-iter-test base1)]
        [base2-get (rl-iter-get base2)]
        [base2-next (rl-iter-next base2)]
        [base2-test (rl-iter-test base2)])
    (define (rl-cproduct-iter-get cproduct-iter) (error "unimplemented"))
    (define (rl-cproduct-iter-next cproduct-iter) (error "unimplemented"))
    (define (rl-cproduct-iter-at-end? select-iter) (error "unimplemented"))
    (rl-iter (rl-cproduct-iter base1 base2)
             rl-cproduct-iter-get
             rl-cproduct-iter-next
             rl-cproduct-iter-at-end?)))

(define (rl-build-pi-iter base selectors)
  (let ([base-get (rl-iter-get base)]
        [base-next (rl-iter-next base)]
        [base-test (rl-iter-test base)])
    (define (rl-pi-iter-get pi-iter) (error "unimplemented"))
    (define (rl-pi-iter-next pi-iter) (error "unimplemented"))
    (define (rl-pi-iter-at-end? pi-iter) (error "unimplemented"))
    (rl-iter (rl-pi-iter base selectors)
             rl-pi-iter-get
             rl-pi-iter-next
             rl-pi-iter-at-end?)))