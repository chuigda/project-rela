#lang racket

(require racket/format)

(define (map-recur proc x)
  (define (map-one item)
    (if (list? item) (map map-one item) (proc item)))
  (map map-one x))

(struct rl-table (name columns tuples))

(struct rl-basic-iter (table cur-tuples))

(struct rl-select-iter (base condition cur-tuples))

(struct rl-equiv-join-iter (iter1 table2 attr1 attr2))

(struct rl-cproduct-iter (base1 base2 base1-tuples base2-tuples))

(struct rl-pi-iter (base column-selectors cur-tuples))

(struct rl-iter (repr get next test))

(struct rl-phantom-tuple ())

(define (rl-build-basic-iter table)
  (rl-basic-iter table (rl-phantom-tuple)))

(define (rl-basic-iter-get basic-iter)
  (let ([current-tuple (rl-basic-iter-cur-tuples basic-iter)])
    (cond [(rl-phantom-tuple? current-tuple) (error "iterator not ready")]
          [(null? current-tuple) (error "iterator at end")]
          [else current-tuple])))

(define (rl-basic-iter-next basic-iter)
  (let ([iter-table (rl-basic-iter-table basic-iter)]
        [current-tuples (rl-basic-iter-cur-tuples basic-iter)])
    (rl-basic-iter iter-table
                   (cond [(rl-phantom-tuple? current-tuples) (rl-table-tuples iter-table))]
                         [(null? current-tuples) (error "iterator at end")]
                         [else (cdr current-tuples)])))

(define (rl-basic-iter-at-end? basic-iter)
  (null (cdr (rl-basic-iter-cur-tuples basic-iter))))
