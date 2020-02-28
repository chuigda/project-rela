#lang racket

(require "table.rkt")
(require "rela.rkt")
(require "util.rkt")

(require racket/format)
(require racket/trace)
(require errortrace)
(require srfi/1)
(require srfi/2)
(require srfi/43)

(struct rl-cartesian (sub-nodes))

(define (rl-build-cartesian sub-nodes)
  (lambda (message . params)
    (case message ['type 'cartesian]
                  ['sub-nodes sub-nodes]
                  ['indexable-with? false]
                  ['has-field? (any (lambda (sub-node) (sub-node 'has-field? (car params)))
                                    sub-nodes)])))

(define (rl-build-equiv-join lhs rhs lhs-var rhs-var)
  (lambda (message . params)
    (case message ['type 'equiv-join]
                  ['lhs lhs]
                  ['rhs rhs]
                  ['lhs-var lhs-var]
                  ['rhs-var rhs-var]
                  ['indexable-with? (lhs 'indexable-with? (car params))]
                  ['has-field? (or (lhs 'has-field? (car params))
                                   (rhs 'has-field? (car params)))])))

(define (rl-adapt-table table)
  (lambda (message . params)
    (let ([columns (rl-table-columns table)]
          [indexed-columns (rl-table-indexed-columns table)])
      (case message ['type 'table]
                    ['indexable-with? (to-be-or-not-to-be (findf (car params) indexed-columns))]
                    ['has-field? (to-be-or-not-to-be (findf (car params) columns))]))))

(define (rl-optimize cartesian-node conditions)
  (define (maybe-equiv-join-condition? condition)
    (and (= (length condition) 3)
         (or (equal? (car condition) =)
             (equal? (car condition) equal?))
         (rl-ref? (cadr condition))
         (rl-ref? (caddr condition))))
  (define (condition-lhs condition) (cadr condition))
  (define (condition-rhs condition) (cddr condition))
  (define (find-lhs-in-tree cartesian-node lhs) (unimplemented))
  (define (find-rhs-top-level cartesian-node rhs) 
    (findf (lambda (node) (rl-indexable-with? node rhs))
           (cartesian-node 'sub-nodes)))
  (define (remove-from-cartesian cartesian-node removed-node)
    (rl-build-cartesian (remove (cartesian-node 'sub-nodes) removed-node eq?)))
  (define (replace-in-tree tree replaced replacement)
    (unimplemented))
  (define (try-find-equiv-join cartesian-node condition)
    (if (not (maybe-equiv-join-condition? condition))
      cartesian-node
      (let* ([lhs-var (condition-lhs condition)]
             [rhs-var (condition-rhs condition)]
             [lhs-var-name (rl-ref-var lhs-var)]
             [rhs-var-name (rl-ref-var rhs-var)]
             [lhs-node (find-lhs-in-tree cartesian-node lhs-var-name)]
             [rhs-node (find-rhs-top-level cartesian-node rhs-var-name)])
        (if (or (null? lhs-node) (null? rhs-node))
            cartesian-node
            (replace-in-tree (remove-from-cartesian cartesian-node rhs-node)
                             lhs-node
                             (rl-equiv-join lhs-node
                                            rhs-node
                                            lhs-var-name
                                            rhs-var-name))))))
)
