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

(define (rl-build-table-info name columns indexed-columns)
  (lambda (message . params)
    (case message ['type 'table]
                  ['name name]
                  ['indexable-with? (any (lambda (column) (equal? column (car params)))
                                         indexed-columns)]
                  ['has-field? (any (lambda (column) (equal? column (car params))
                                    columns))])))

(define (rl-adapt-table table)
  (rl-build-table-info (rl-table-name table)
                       (rl-table-columns table)
                       (rl-table-indexed-columns table)))

(define (rl-optimize cartesian-node conditions)
  (define (equiv-join-condition? condition)
    (and (= (length condition) 3)
         (or (equal? (car condition) =)
             (equal? (car condition) equal?))
         (rl-ref? (cadr condition))
         (rl-ref? (caddr condition))))
  (define (condition-lhs condition) (cadr condition))
  (define (condition-rhs condition) (cddr condition))
  (define (find-lhs-in-tree tree lhs-var)
    (if (tree 'indexable-with lhs-var)
        (case (tree 'type)
              ['cartesian 
                (find not-null?
                      (map (lambda (sub-tree) (find-lhs-in-tree sub-tree lhs-var))
                           (tree 'sub-nodes)))]
              ['equiv-join 
                (let* ([lhs-sub-tree (tree 'lhs)]
                       [rhs-sub-tree (tree 'rhs)]
                       [lhs-result (find-lhs-in-tree lhs-sub-tree lhs-var)]
                       [rhs-result (find-lhs-in-tree rhs-sub-tree lhs-var)])
                  (cond [(not-null? lhs-result) lhs-result]
                        [(not-null? rhs-result) rhs-result]
                        [else null]))]
              ['table tree])
        null))
  (define (find-rhs-top-level cartesian-node rhs-var)
    (findf (lambda (node) (node 'indexable-with? rhs-var))
           (cartesian-node 'sub-nodes)))
  (define (remove-from-cartesian cartesian-node removed-node)
    (rl-build-cartesian (remove (cartesian-node 'sub-nodes) removed-node eq?)))
  (define (replace-in-tree tree replaced replacement)
    (if (eq? tree replacement)
        replacement
        (case (tree 'type)
              ['cartesian 
                (rl-build-cartesian (map (lambda (sub-tree) (replace-in-tree sub-tree replacement))
                                         (tree 'sub-nodes)))]
              ['equiv-join
                (let* ([lhs-sub-tree (tree 'lhs)]
                       [rhs-sub-tree (tree 'rhs)]
                       [lhs-var (tree 'lhs-var)]
                       [rhs-var (tree 'rhs-var)])
                  (rl-build-equiv-join (replace-in-tree lhs-sub-tree replacement)
                                       (replace-in-tree rhs-sub-tree replace-in-tree)
                                       lhs-var
                                       rhs-var))]
              ['table tree])))
  (define (try-find-equiv-join cartesian-node condition)
    (if (not (equiv-join-condition? condition))
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
                             (rl-build-equiv-join lhs-node
                                                  rhs-node
                                                  lhs-var-name
                                                  rhs-var-name))))))
  (define (optimize-int tree equiv-join-conditions)
    (if (null? equiv-join-conditions)
        (optimize-int (try-find-equiv-join tree (car equiv-join-conditions))
                      (cdr equiv-join-conditions))
        tree))
  (let* ([equiv-join-conditions (filter equiv-join-condition? conditions)]
         [permuts (permutations equiv-join-conditions)])
    (map (lambda (equiv-join-conditions) (optimize-int cartesian-node equiv-join-conditions)
                 permuts))))
