#lang racket

(require racket/format)
(require racket/trace)
(require errortrace)
(require srfi/2)
(require srfi/43)

(require "expr.rkt")
(require "table.rkt")
(require "util.rkt")

(provide rl-ref rl-ref? rl-ref-var)

(provide rl-build-cartesian
         rl-build-equiv-join
         rl-build-table-info
         rl-adapt-table)

(define (rl-build-cartesian sub-nodes)
  (lambda (message . params)
    (case message ['type 'cartesian]
                  ['sub-nodes sub-nodes]
                  ['indexable-with? false]
                  ['fields (foldl append (map (lambda (sub-node) (sub-node 'fields)) sub-nodes))]
                  ['has-field? 
                    (any-of (lambda (sub-node) (sub-node 'has-field? (car params))) sub-nodes)]
                  ['disp
                    (string-append "<" (string-join (map (lambda (sub-node) (sub-node 'disp))
                                                         sub-nodes)
                                                    " * ")
                                   ">")])))

(define (rl-build-cartesian-2 lhs rhs)
  (lambda (message . params)
    (case message ['type 'cartesian2]
                  ['lhs lhs]
                  ['rhs rhs]
                  ['fields (append (lhs 'fields) (rhs 'fields))]
                  ['has-field? (or (lhs 'has-field? (car params))
                                   (rhs 'has-field? (car params)))]
                  ['indexable-with? false]
                  ['disp (string-append "<" (lhs 'disp) " * " (rhs 'disp) ">")])))

(define (rl-build-selection selected condition)
  (lambda (message . params)
    (case message ['type 'select]
                  ['selected selected]
                  ['condition condition]
                  ['fields (selected 'fields)]
                  ['has-field? (selected 'has-field (car params))]
                  ['indexable-with? false]
                  ['disp (string-append "SIGMA(" (raw-expr->string condition) ": " (selected 'disp) ")")])))

(define (rl-build-equiv-join lhs rhs lhs-var rhs-var)
  (lambda (message . params)
    (case message ['type 'equiv-join]
                  ['lhs lhs]
                  ['rhs rhs]
                  ['lhs-var lhs-var]
                  ['rhs-var rhs-var]
                  ['fields (append (lhs 'fields) (rhs 'fields))]
                  ['indexable-with? (lhs 'indexable-with? (car params))]
                  ['has-field? 
                    (or (lhs 'has-field? (car params))
                        (rhs 'has-field? (car params)))]
                  ['disp
                    (string-append "JOIN("
                                   (~a lhs-var)
                                   " -> "
                                   (~a rhs-var)
                                   ": "
                                   (lhs 'disp)
                                   ", "
                                   (rhs 'disp)
                                   ")")])))

(define (rl-build-table-info name columns indexed-columns)
  (lambda (message . params)
    (case message ['type 'table]
                  ['name name]
                  ['fields columns]
                  ['indexable-with? (any-of (lambda (column) (equal? column (car params)))
                                            indexed-columns)]
                  ['has-field? (any-of (lambda (column) (equal? column (car params))
                                       columns))]
                  ['disp name])))

(define (rl-adapt-table table)
  (rl-build-table-info (rl-table-name table)
                       (rl-table-columns table)
                       (rl-table-indexed-columns table)))

(provide rl-optimize)

(define (rl-optimize cartesian-node conditions)
  (define (equiv-join-condition? condition)
    (and (= (length condition) 3)
         (or (equal? (first condition) =)
             (equal? (first condition) equal?))
         (rl-ref? (second condition))
         (rl-ref? (third condition))))
  (define (condition-lhs condition) (second condition))
  (define (condition-rhs condition) (third condition))
  (define (find-lhs-in-tree cartesian lhs-var)
    (define (find-lhs-in-tree-int tree lhs-var)
      (if (tree 'indexable-with? lhs-var)
          (case (tree 'type)
                ['cartesian (unreachable)]
                ['cartesian2 (unreachable)]
                ['select (unreachable)]
                ['equiv-join 
                  (let* ([lhs-sub-tree (tree 'lhs)]
                         [rhs-sub-tree (tree 'rhs)]
                         [lhs-result (find-lhs-in-tree-int lhs-sub-tree lhs-var)]
                         [rhs-result (find-lhs-in-tree-int rhs-sub-tree lhs-var)])
                    (cond [(not-null? lhs-result) lhs-result]
                          [(not-null? rhs-result) rhs-result]
                          [else null]))]
                ['table tree])
          null))
    (false2null (findf not-null? (map (lambda (sub-node) (find-lhs-in-tree-int sub-node lhs-var))
                                      (cartesian 'sub-nodes)))))
  (define (find-rhs-top-level cartesian-node rhs-var)
    (false2null (findf (lambda (node) (node 'indexable-with? rhs-var))
                       (cartesian-node 'sub-nodes))))
  (define (remove-from-cartesian cartesian-node removed-node)
    (rl-build-cartesian (remove removed-node (cartesian-node 'sub-nodes) eq?)))
  (define (replace-in-tree tree replaced replacement)
    (if (eq? tree replaced)
        replacement
        (case (tree 'type)
              ['cartesian 
                (rl-build-cartesian (map (lambda (sub-tree) 
                                           (replace-in-tree sub-tree replaced replacement))
                                         (tree 'sub-nodes)))]
              ['cartesian2
                (let ([lhs-sub-tree (tree 'lhs)]
                      [rhs-sub-tree (tree 'rhs)])
                  (rl-build-cartesian-2 (replace-in-tree lhs-sub-tree replaced replacement)
                                        (replace-in-tree rhs-sub-tree replaced replacement)))]
              ['select
                (let ([selected (tree 'selected)]
                      [condition (tree 'condition)])
                  (rl-build-selection (replace-in-tree selected replaced replacement) condition))]
              ['equiv-join
                (let* ([lhs-sub-tree (tree 'lhs)]
                       [rhs-sub-tree (tree 'rhs)]
                       [lhs-var (tree 'lhs-var)]
                       [rhs-var (tree 'rhs-var)])
                  (rl-build-equiv-join (replace-in-tree lhs-sub-tree replaced replacement)
                                       (replace-in-tree rhs-sub-tree replaced replace-in-tree)
                                       lhs-var
                                       rhs-var))]
              ['table tree])))
  (define (try-find-equiv-join cartesian-node condition)
    (let* ([lhs-var (condition-lhs condition)]
           [rhs-var (condition-rhs condition)]
           [lhs-var-name (rl-ref-var lhs-var)]
           [rhs-var-name (rl-ref-var rhs-var)]
           [lhs-node-1 (find-lhs-in-tree cartesian-node lhs-var-name)]
           [rhs-node-1 (find-rhs-top-level cartesian-node rhs-var-name)]
           [lhs-node-2 (find-lhs-in-tree cartesian-node rhs-var-name)]
           [rhs-node-2 (find-rhs-top-level cartesian-node lhs-var-name)])
      (list (if (and (not-null? lhs-node-1) (not-null? rhs-node-1))
                (cons (replace-in-tree (remove-from-cartesian cartesian-node rhs-node-1)
                                       lhs-node-1
                                       (rl-build-equiv-join lhs-node-1
                                                            rhs-node-1
                                                            lhs-var-name
                                                            rhs-var-name))
                      null)
                null)
            (if (and (not-null? lhs-node-2) (not-null? rhs-node-2))
                (cons (replace-in-tree (remove-from-cartesian cartesian-node rhs-node-2)
                                       lhs-node-2
                                       (rl-build-equiv-join lhs-node-2
                                                            rhs-node-2
                                                            rhs-var-name
                                                            lhs-var-name))
                      null)
                null)
            (cons cartesian-node condition))))
  (define (find-node-with-vars tree vars)
    (define (find-binary-node tree vars)
      (let* ([lhs (tree 'lhs)]
             [rhs (tree 'rhs)]
             [lhs-result (find-node-with-vars lhs vars)]
             [rhs-result (find-node-with-vars rhs vars)])
        (cond [(not-null? lhs-result) lhs-result]
              [(not-null? rhs-result) rhs-result]
              [else tree])))
    (let ([tree-vars (tree 'fields)])
      (if (list-contains? tree-vars vars)
          (case (tree 'type)
                ['cartesian (unreachable)]
                ['cartesian2 (find-binary-node tree vars)]
                ['select tree]
                ['equiv-join (find-binary-node tree vars)]
                ['table tree])
          null)))
  (define (pushdown-selection tree condition)
    (let* ([condition-vars (rl-expr-vars condition)]
           [tree-node (find-node-with-vars tree condition-vars)]
           [select-node (rl-build-selection tree-node condition)])
      (replace-in-tree tree tree-node select-node)))
  (define (optimize-int tree-list-pairs equiv-join-conditions)
    (if (null? equiv-join-conditions)
        tree-list-pairs
        (optimize-int
          (foldl append 
                 (list)
                 (map (lambda (tree-list-pair)
                        (map
                          (lambda (tree-condition-pair)
                            (cons (car tree-condition-pair) 
                                  (cons (cdr tree-condition-pair) (cdr tree-list-pair))))
                          (remove null
                            (try-find-equiv-join (car tree-list-pair) (car equiv-join-conditions)))))
                      tree-list-pairs))
          (cdr equiv-join-conditions))))
  (let* ([equiv-join-conditions (filter equiv-join-condition? conditions)]
         [non-join-conditions (filter (negate equiv-join-condition?) conditions)])
    (optimize-int (list (cons cartesian-node null)) equiv-join-conditions)))
