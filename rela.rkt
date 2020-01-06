#lang racket
(current-namespace (make-base-namespace))

(struct rl-table (name columns tuples))

(define (rl-cartesian-product table1 table2)
  (define (rl-cartesian-product-one tuple table)
    (map (lambda (tuple1) (append tuple tuple1)) table))
  (define (rl-cartesian-product-intern table1-tuples table2-tuples)
    (foldl (lambda (x0 x) (append x0 x))
           (list)
           (map (lambda (tuple1) (rl-cartesian-product-one tuple1 table2-tuples)) table1-tuples)))
  (rl-table (string-append "<" (rl-table-name table1) " * " (rl-table-name table2) ">" )
            (append (rl-table-columns table1) (rl-table-columns table2))
            (rl-cartesian-product-intern (rl-table-tuples table1)
                                         (rl-table-tuples table2))))

(define (rl-select-base table pred select-name)
  (define (rl-select-intern tuples) (filter pred tuples))
  (rl-table (string-append "SIGMA(" select-name "; " (rl-table-name table) ")")
            (rl-table-columns table)
            (rl-select-intern (rl-table-tuples table))))

(define (rl-projection table column-names projection-name)
  (define (get-all-column-index table-columns column-names)
    (map (lambda (column-name)
      (let ([column-index (index-of table-columns column-name)])
        (if (equal? column-index false) (error "column does not exist")
                                        column-index)))
      column-names))
  (define (projection-one-tuple tuple column-indexes)
    (map (lambda (column-index) (list-ref tuple column-index)) column-indexes))
  (define (rl-projection-intern tuples column-indexes)
    (map (lambda (tuple) (projection-one-tuple tuple column-indexes)) tuples))
  (rl-table (string-append "PI(" projection-name "; " (rl-table-name table) ")")
            column-names
            (rl-projection-intern (rl-table-tuples table) 
                                  (get-all-column-index (rl-table-columns table) column-names))))

(define (rl-build-column-selector table-columns column-name)
  (let ([column-index (index-of table-columns column-name)])
    (if (equal? column-index false)
      (error "column does not exist")
      (lambda (tuple) (list-ref tuple column-index)))))

(struct rl-ref (name))

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

(define (display-table table)
  (define (display-tuples tuples)
      (if (null? (cdr tuples))
          (begin
            (display "")
            (display (car tuples)))
          (begin
            (display (car tuples))
            (display "\n ")
            (display-tuples (cdr tuples)))))
  (begin (display (rl-table-name table)) (display "\n")
         (display " ") (display (rl-table-columns table)) (display "\n")
         (display " ") (display-tuples (rl-table-tuples table))))

; select columns from tables where conditions
(define (rl-select columns tables conditions)
  (error "unimplemented"))

(define test-table 
  (rl-table "test-table" 
            (list "a" "b" "c") 
            (list (list 1 2 3)
                  (list 4 5 6)
                  (list 2 3 6))))

(define (and-proc x y) (and x y))

(define (or-proc x y) (or x y))

(define compiled-expr 
  (rl-compile-expr (rl-table-columns test-table)
                   (list and-proc (list > (rl-ref "a") 1)
                                  (list < (rl-ref "b") 4))))

(display (compiled-expr (list 2 3 4))) ; #t
(display "\n")
(display (compiled-expr (list 4 5 6))) ; #f
(display "\n")

(define test-table-2
  (rl-table "test-table2"
            (list "a1" "d" "e" )
            (list (list 1 "PaddY" "AK-47")
                  (list 2 "Winter" "SG-553")
                  (list 4 "Auxilior" "XM1014"))))

(define ttable-p (rl-projection test-table '("c" "b") "c, b"))

(define ttable-s (rl-select-base test-table 
                                 (lambda (tuple) (= (list-ref tuple 2) 6))
                                 "c = 6"))

(define cprod (rl-cartesian-product test-table test-table-2))

(define cprod-selected (rl-projection (rl-select-base cprod 
                                                      (lambda (tuple) (= (list-ref tuple 0)
                                                                         (list-ref tuple 3)))
                                                      "a = a1")
                                      '("a" "b" "c" "d" "e")
                                      "a, b, c, d, e"))

(display-table test-table)
(display "\n")
(display-table test-table-2)
(display "\n")
(display-table ttable-p)
(display "\n")
(display-table ttable-s)
(display "\n")
(display-table cprod)
(display "\n")
(display-table cprod-selected)
(display "\n")