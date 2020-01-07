#lang racket

(require racket/format)

(define (map-recur proc x)
  (define (map-one item)
    (if (list? item) (map map-one item) (proc item)))
  (map map-one x))

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
             (if (equal? column-index false) 
                 (error (string-append "column " column-name " does not exist"))
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
          (display " ")
          (display (car tuples)))
        (begin
          (display " ")
          (displayln (car tuples))
          (display-tuples (cdr tuples)))))
  (begin (displayln (rl-table-name table))
         (display " ") (displayln (rl-table-columns table))
         (display-tuples (rl-table-tuples table))))

(define (raw-expr->string raw-expr)
  (define (rl-ref-replace raw-expr)
    (map-recur (lambda (x) (if (rl-ref? x) (rl-ref-name x) x))
               raw-expr))
  (string-replace (~a (rl-ref-replace raw-expr)) "procedure:" ""))

(define (and-proc x y) (and x y))

(define (or-proc x y) (or x y))

(define (rl-select columns tables conditions)
  (define (cartesian-all-tables tables)
    (foldl (lambda (t0 t1) (rl-cartesian-product t0 t1))
           (car tables)
           (cdr tables)))
  (define (rl-select-cascade raw-conditions compiled-conditions table)
    (if (null? raw-conditions)
        table
        (rl-select-cascade (cdr raw-conditions)
                           (cdr compiled-conditions)
                           (rl-select-base table
                                           (car compiled-conditions)
                                           (raw-expr->string (car raw-conditions))))))
  (let ([all-cprod-table (cartesian-all-tables tables)])
    (let ([compiled-conditions (map (lambda (raw-expression) 
                                            (rl-compile-expr (rl-table-columns all-cprod-table)
                                                                               raw-expression))
                                    conditions)])
      (rl-projection (rl-select-cascade conditions compiled-conditions all-cprod-table) 
                     columns 
                     (~a columns)))))

(define players-table
  (rl-table "players-table"
            '("pno" "pname" "pteam")
            '((1 "QDU.Sumoon" "Qingdao University")
              (2 "BUG.Chu1gda" "BUGaming")
              (3 "ICE.1000" "Internal Compiler Error")
              (4 "CHUK-SZ.ZYF" "CHinese University of HongKong (Shenzhen)"))))

(define tools-table
  (rl-table "tools-table"
            '("tno" "tname" "tvendor")
            '((1 "Dev-CPP" "ACM-ICPC")
              (2 "Intellij-IDEA" "Jetbrains")
              (3 "QtCreator" "Digia")
              (4 "CLion" "Jetbrains"))))

(define players-tools-table
  (rl-table "players-tools-table"
            '("pno1" "tno1")
            '((1 1)
              (2 3)
              (3 2)
              (4 4))))

; select pname, tname 
;   from playes-table, tools-table, players-tools-table
;   where pno = pno1 
;     and tno = tno1
(define s1 (rl-select '("pname" "tname")
                      (list players-table tools-table players-tools-table)
                      (list (list = (rl-ref "pno") (rl-ref "pno1"))
                            (list = (rl-ref "tno") (rl-ref "tno1")))))

; select tname, tvendor
;   from tools-table
;   where tvendor = "Jetbrains"
(define s2 (rl-select '("tname" "tvendor")
                      (list tools-table)
                      (list (list string=? (rl-ref "tvendor") "Jetbrains"))))

; select pname
; from players-table, tools-table, players-tools-table
; where pno = pno1 
;   and tno = tno1
;   and tvendor = "Jetbrains"
(define s3 (rl-select '("pname")
                      (list players-table tools-table players-tools-table)
                      (list (list = (rl-ref "pno") (rl-ref "pno1"))
                            (list = (rl-ref "tno") (rl-ref "tno1"))
                            (list string=? (rl-ref "tvendor") "Jetbrains"))))

(display-table s1)

(displayln " ")

(display-table s2)

(displayln " ")

(display-table s3)
