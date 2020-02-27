#lang racket

(require "table.rkt")
(require "rela.rkt")
(require "util.rkt")

(require racket/format)
(require racket/trace)
(require errortrace)
(require srfi/2)
(require srfi/43)

(struct rl-cartesian (sub-nodes))

(define (optimize cartesian-node conditions)
  (define (maybe-equiv-join-condition? condition)
    (and (= (length condition) 3)
         (or (equal? (car condition) =)
             (equal? (car condition) equal?))
         (rl-ref? (cadr condition))
         (rl-ref? (caddr condition))))
  (define (condition-lhs condition) (cadr condition))
  (define (condition-rhs condition) (cddr condition))
  (define (remove-from-cartesian cartesian-node removed-node)
    (rl-cartesian (remove (rl-cartesian-sub-nodes cartesian-node) removed-node eq?)))
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
