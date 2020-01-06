#lang racket

(struct rl-table (name columns tuples))

(define (rl-cartesian-product table1 table2)
  (define (rl-cartesian-product-one tuple table)
    (if (null? table) 
        null
        (cons (append tuple (car table))
              (rl-cartesian-product-one tuple (cdr table)))))
  (define (rl-cartesian-product-intern table1-tuples table2-tuples)
    (if (null? table1-tuples)
        null
        (append (rl-cartesian-product-one (car table1-tuples) table2-tuples)
                (rl-cartesian-product-intern (cdr table1-tuples) table2-tuples))))
  (rl-table (string-append "<" (rl-table-name table1) " * " (rl-table-name table2) ">" )
            (append (rl-table-columns table1) (rl-table-columns table2))
            (rl-cartesian-product-intern (rl-table-tuples table1)
                                         (rl-table-tuples table2))))

(define (rl-select-base table pred select-name)
  (define (rl-select-intern tuples)
    (if (null? tuples)
        null
        (let ([this-tuple (car tuples)]
              [rest-tuples (cdr tuples)])
          (if (pred this-tuple)
              (append (list this-tuple)
                      (rl-select-intern rest-tuples))
              (rl-select-intern rest-tuples)))))
  (rl-table (string-append "SIGMA(" select-name "; " (rl-table-name table) ")")
            (rl-table-columns table)
            (rl-select-intern (rl-table-tuples table))))

(define (rl-projection table column-names projection-name)
  (define (get-all-column-index table-columns column-names)
    (if (null? column-names)
        null
        (let ([this-column-name (car column-names)]
              [rest-column-names (cdr column-names)])
          (let ([column-index (index-of table-columns this-column-name)])
            (if (equal? column-index false)
                (error "column does not exist")
                (append (list column-index) 
                              (get-all-column-index table-columns rest-column-names)))))))
  (define (projection-one-tuple tuple column-indexes)
    (if (null? column-indexes)
        null
        (let ([this-column-index (car column-indexes)]
              [rest-column-indexes (cdr column-indexes)])
          (append (list (list-ref tuple this-column-index))
                  (projection-one-tuple tuple rest-column-indexes)))))
  (define (rl-projection-intern tuples column-indexes)
    (if (null? tuples)
        null
        (append (list (projection-one-tuple (car tuples) column-indexes))
                (rl-projection-intern (cdr tuples) column-indexes))))
  (rl-table (string-append "PI(" projection-name "; " (rl-table-name table) ")")
            column-names
            (rl-projection-intern (rl-table-tuples table) 
                                  (get-all-column-index (rl-table-columns table) column-names))))

(define (rl-build-column-selector table column-name)
  (let ([column-index (index-of (rl-table-columns table) column-name)])
    (if (equal? column-index false)
      (error "column does not exist")
      (lambda (tuple) (list-ref tuple column-index)))))

(define (compile-condition incomplete-condition)
  )

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