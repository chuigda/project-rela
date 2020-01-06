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
  (rl-table (string-append (rl-table-name table1) " * " (rl-table-name table2))
            (append (rl-table-columns table1) (rl-table-columns table2))
            (rl-cartesian-product-intern (rl-table-tuples table1)
                                         (rl-table-tuples table2))))

