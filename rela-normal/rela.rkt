#lang racket

(require racket/format)
(require racket/trace)
(require errortrace)

(require srfi/2)
(require srfi/43)

(require "util.rkt")
(require "svec.rkt")
(require "table.rkt")

(provide rl-build-table
         rl-table-name
         rl-table-columns
         rl-table-tuples
         rl-table-indexed-columns
         rl-table-index-maps
         rl-build-column-selector)

; making eval work
(current-namespace (make-base-namespace))

(provide rl-ref rl-ref? rl-ref-var)

; for building name references in raw expressions
(struct rl-ref (name))
(define rl-ref-var rl-ref-name)

(provide display-table)

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
; 'get, 'next, 'test, 'rewind, 'name, 'columns, 'indexable-check, 'index

(provide rl-iter-get
         rl-iter-next
         rl-iter-test
         rl-iter-rewind
         rl-iter-name
         rl-iter-columns
         rl-iter-indexable?
         rl-iter-index)

; these are shorthand methods
(define (rl-iter-get iter) (iter 'get))
(define (rl-iter-next iter) (iter 'next))
(define (rl-iter-test iter) (iter 'test))
(define (rl-iter-rewind iter) (iter 'rewind))
(define (rl-iter-name iter) (iter 'name))
(define (rl-iter-columns iter) (iter 'columns))
(define (rl-iter-indexable? iter column-name) (iter 'indexable-check column-name))
(define (rl-iter-index iter column-name) (iter 'index column-name))

; phantom tuple, used when implementing basic-iter
(struct rl-phantom-tuple ())

(provide rl-build-basic-iter)

(define (rl-build-basic-iter table)
  (define (rl-build-basic-iter-int cur-tuples)
    (define (rl-basic-iter-get)
      (cond [(rl-phantom-tuple? cur-tuples) (error "iterator not ready")]
            [(null? cur-tuples) (error "iterator at end")]
            [else (car cur-tuples)]))
    (define (rl-basic-iter-next)
      (cond [(rl-phantom-tuple? cur-tuples) (rl-build-basic-iter-int (rl-table-tuples table))]
            [(null? cur-tuples) (error "iterator already at end")]
            [else (rl-build-basic-iter-int (cdr cur-tuples))]))
    (define (rl-basic-iter-test) (null? cur-tuples))
    (define (rl-basic-iter-rewind) (rl-build-basic-iter-int (rl-phantom-tuple)))
    (define (rl-basic-iter-name) (rl-table-name table))
    (define (rl-basic-iter-columns) (rl-table-columns table))
    (define (rl-basic-iter-indexable-check column-name)
      (let ([indexed-columns (rl-table-indexed-columns table)])
        (if (null? indexed-columns)
            false
            (not (false? (index-of indexed-columns column-name))))))
    (define (rl-basic-iter-index column-name)
      (let* ([indexed-columns (rl-table-indexed-columns table)]
             [index-maps (rl-table-index-maps table)]
             [subscript (index-of indexed-columns column-name)])
        (if (false? subscript)
            (error "index does not exist")
            (lambda (column-value) (svec-get (list-ref index-maps subscript) column-value)))))
    (lambda (message . params)
      (case message ['get (rl-basic-iter-get)]
                    ['next (rl-basic-iter-next)]
                    ['test (rl-basic-iter-test)]
                    ['rewind (rl-basic-iter-rewind)]
                    ['name (rl-basic-iter-name)]
                    ['columns (rl-basic-iter-columns)]
                    ['indexable-check (rl-basic-iter-indexable-check (car params))]
                    ['index (rl-basic-iter-index (car params))])))
  (rl-build-basic-iter-int (rl-phantom-tuple)))

(provide rl-build-cartesian-iter)

(define (rl-build-cartesian-iter base1-origin base2-origin)
  (define (rl-build-cartesian-iter-int base1 base2)
    (define (rl-cartesian-iter-get) (append (rl-iter-get base1) (rl-iter-get base2)))
    (define (rl-cartesian-next-post-check)
      (if (rl-iter-test base2)
          (rl-cartesian-iter-next)
          (rl-build-cartesian-iter-int base1 base2)))
    (define (rl-cartesian-iter-next)
      (cond [(rl-iter-test base1) (error "cartesian iterator at end")]
            [(rl-iter-test base2) (rl-build-cartesian-iter-int (rl-iter-next base1)
                                                               (rl-iter-next (rl-iter-rewind base2)))]
            [else ((rl-build-cartesian-iter-int base1 (rl-iter-next base2)) 'post-check)]))
    (define (rl-cartesian-iter-test) (rl-iter-test base1))
    (define (rl-cartesian-iter-rewind) (rl-build-cartesian-iter-int base1-origin base2-origin))
    (define (rl-cartesian-iter-name) 
      (string-append "<" (rl-iter-name base1-origin) " * " (rl-iter-name base2-origin) ">"))
    (define (rl-cartesian-iter-columns)
      (append (rl-iter-columns base1-origin) (rl-iter-columns base2-origin)))
    (lambda (message . params)
      (case message ['get (rl-cartesian-iter-get)]
                    ['next (rl-cartesian-iter-next)]
                    ['test (rl-cartesian-iter-test)]
                    ['rewind (rl-cartesian-iter-rewind)]
                    ['name (rl-cartesian-iter-name)]
                    ['columns (rl-cartesian-iter-columns)]
                    ['post-check (rl-cartesian-next-post-check)])))
  (rl-build-cartesian-iter-int (rl-iter-next base1-origin) base2-origin))

(provide rl-build-projection-iter)

(define (rl-build-projection-iter base-origin column-names)
  (define (rl-build-all-column-selectors table-columns column-names)
    (map (lambda (column-name) (rl-build-column-selector table-columns column-name)) 
         column-names))
  (let* ([base-columns (rl-iter-columns base-origin)]
         [all-column-selectors (rl-build-all-column-selectors base-columns column-names)])
    (define (rl-build-projection-iter-int base)
      (define (rl-projection-iter-get)
        (let ([cur-tuple (rl-iter-get base)])
          (map (lambda (f) (f cur-tuple)) all-column-selectors)))
      (define (rl-projection-iter-next)
        (rl-build-projection-iter-int (rl-iter-next base)))
      (define (rl-projection-iter-test)
        (rl-iter-test base))
      (define (rl-projection-iter-rewind)
        (rl-build-projection-iter-int base-origin))
      (define (rl-projection-iter-name)
        (string-append "PI<" 
                       (rl-iter-name base-origin)
                       "; "
                       (~a column-names)
                       ">"))
      (define (rl-projection-iter-columns) column-names)
      (lambda (message . params)
        (case message ['get (rl-projection-iter-get)]
                      ['next (rl-projection-iter-next)]
                      ['test (rl-projection-iter-test)]
                      ['rewind (rl-projection-iter-rewind)]
                      ['name (rl-projection-iter-name)]
                      ['columns (rl-projection-iter-columns)])))
    (rl-build-projection-iter-int base-origin)))

(provide rl-build-select-iter)

(define (rl-build-select-iter base-origin raw-condition)
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
  (let ([compiled-condition (rl-compile-expr (rl-iter-columns base-origin) raw-condition)])
    (define (rl-build-select-iter-int base)
      (define (rl-select-iter-get) (rl-iter-get base))
      (define (rl-select-iter-next)
        (define (rl-select-iter-next-intern base1)
          (cond [(rl-iter-test base1) base1]
                [(compiled-condition (rl-iter-get base1)) base1]
                [else (rl-select-iter-next-intern (rl-iter-next base1))]))
        (rl-build-select-iter-int (rl-select-iter-next-intern (rl-iter-next base))))
      (define (rl-select-iter-test) (rl-iter-test base))
      (define (rl-select-iter-rewind) (rl-build-select-iter-int (rl-iter-rewind base)))
      (define (rl-select-iter-name)
        (string-append "SIGMA<"
                       (rl-iter-name base-origin)
                       ";"
                       (~a (raw-expr->string raw-condition))
                       ">"))
      (define (rl-select-iter-columns) (rl-iter-columns base-origin))
      (define (rl-select-iter-indexable-check column-name) (rl-iter-indexable? base column-name))
      (define (rl-select-iter-index column-name)
        (lambda (column-value)
          (and-let* ([base-index (rl-iter-index base)]
                     [tuple (base-index column-value)]
                     [check-result (compiled-condition tuple)])
            tuple)))
      (lambda (message . params)
        (case message ['get (rl-select-iter-get)]
                      ['next (rl-select-iter-next)]
                      ['test (rl-select-iter-test)]
                      ['rewind (rl-select-iter-rewind)]
                      ['name (rl-select-iter-name)]
                      ['columns (rl-select-iter-columns)]
                      ['indexable-check (rl-select-iter-indexable-check (car params))]
                      ['index (rl-select-iter-index (car params))])))
    (rl-build-select-iter-int base-origin)))

(provide rl-build-equiv-join-iter)

(define (rl-build-equiv-join-iter iter1-origin iter2 iter1-column iter2-column)
  (let ([iter1-column-selector (rl-build-column-selector (rl-iter-columns iter1-origin) iter1-column)]
        [iter2-index (rl-iter-index iter2 iter2-column)])
    (define (rl-build-equiv-join-iter-int iter1)
      (define (rl-equiv-join-iter-get)
        (let* ([iter1-tuple (rl-iter-get iter1)]
               [iter1-key (iter1-column-selector iter1-tuple)]
               [iter2-tuple (iter2-index iter1-key)])
          (append iter1-tuple iter2-tuple)))
      (define (rl-equiv-join-iter-next)
        (define (rl-equiv-join-iter-next-int prim-iter)
          (if (rl-iter-test prim-iter)
              prim-iter
              (let* ([iter1-tuple (rl-iter-get prim-iter)]
                     [iter1-key (iter1-column-selector iter1-tuple)]
                     [iter2-tuple (iter2-index iter1-key)])
                (if (false? iter2-tuple)
                    (rl-equiv-join-iter-next-int (rl-iter-next prim-iter))
                    prim-iter))))
        (rl-build-equiv-join-iter-int (rl-equiv-join-iter-next-int (rl-iter-next iter1))))
      (define (rl-equiv-join-iter-test) (rl-iter-test iter1))
      (define (rl-equiv-join-iter-rewind) (rl-build-equiv-join-iter-int (rl-iter-rewind iter1)))
      (define (rl-equiv-join-iter-indexable-check column-name)
        (rl-iter-indexable? iter1 column-name))
      (define (rl-equiv-join-iter-name)
        (string-append "<" (rl-iter-name iter1-origin) " |X| " (rl-iter-name iter2) ">"))
      (define (rl-equiv-join-iter-columns) 
        (append (rl-iter-columns iter1-origin) (rl-iter-columns iter2)))
      (define (rl-equiv-join-iter-index column-name)
        (lambda (column-value)
          (and-let* ([iter1-index (rl-iter-index iter1 column-name)]
                     [iter1-tuple (iter1-index column-value)]
                     [iter1-key (iter1-column-selector iter1-tuple)]
                     [iter2-tuple (iter2-index iter1-key)])
            (append iter1-tuple iter2-tuple))))
      (lambda (message . params)
        (case message ['get (rl-equiv-join-iter-get)]
                      ['next (rl-equiv-join-iter-next)]
                      ['test (rl-equiv-join-iter-test)]
                      ['rewind (rl-equiv-join-iter-rewind)]
                      ['name (rl-equiv-join-iter-name)]
                      ['columns (rl-equiv-join-iter-columns)]
                      ['indexable-check (rl-equiv-join-iter-indexable-check (car params))]
                      ['index (rl-equiv-join-iter-index (car params))])))
    (rl-build-equiv-join-iter-int iter1-origin)))

(provide rl-iter-traverse)

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
