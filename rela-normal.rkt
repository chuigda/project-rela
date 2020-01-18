#lang racket

(require racket/format)
(require racket/trace)
(require errortrace)

; unimplemented stuffs and todos
(define (unimplemented) (error "unimplemented"))

; sometimes useful for debugging
(define (id x) x)

; making eval work
(current-namespace (make-base-namespace))

; a shorthand
(define (not-null? blah) (not (null? blah)))

; this will be useful when constructing hash index for tables
(define (list->hash list-of-pairs)
  (foldl (lambda (the-hash pair) (hash-set (car pair) (cdr pair)))
         (make-hash)
         list-of-pairs))

; we need this for tree structure traversing
(define (map-recur proc x)
  (define (map-one item)
    (if (list? item) (map map-one item) (proc item)))
  (map map-one x))

; basic table
(struct rl-table (name columns tuples indexed-columns index-maps))

; used in many situations
(define (rl-build-column-selector table-columns column-name)
  (let ([column-index (index-of table-columns column-name)])
    (if (equal? column-index false)
        (error "column does not exist")
        (lambda (tuple) (list-ref tuple column-index)))))

(define (rl-build-table name columns tuples . indexed-columns)
  (define (rl-build-one-index indexed-column)
    (let* ([column-selector (rl-build-column-selector columns indexed-column)]
           [tuples-transformer (lambda (tuple) (cons (column-selector tuple) tuple))])
      (list->hash (map tuples-transformer tuples))))
  (if (null? indexed-columns)
      (rl-table name 
                columns
                tuples
                null
                null)
      (rl-table name
                columns
                tuples
                indexed-columns
                (map rl-build-one-index indexed-columns))))

; for building name references in raw expressions
(struct rl-ref (name))

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
(struct rl-iter-procs (get next test rewind name columns indexable-check index))

(struct rl-iter (repr procset))

; only some of iterators supports indexed lookup
(define (rl-iter-indexable? iter field)
  (let* ([iter-procs (rl-iter-procset iter)]
         [iter-indexable-check (rl-iter-procs-indexable-check iter)])
    (and (not-null? iter-indexable-check)
         (iter-indexable-check field))))

; generic methods
(define (rl-iter-get iter)
  (let ([repr (rl-iter-repr iter)]
        [get-proc (rl-iter-procs-get (rl-iter-procset iter))])
    (get-proc repr)))

(define (rl-iter-next iter)
  (let* ([repr (rl-iter-repr iter)]
         [procs (rl-iter-procset iter)]
         [next-proc (rl-iter-procs-next procs)])
    (rl-iter (next-proc repr) procs)))

(define (rl-iter-test iter)
  (let ([repr (rl-iter-repr iter)]
        [test-proc (rl-iter-procs-test (rl-iter-procset iter))])
    (test-proc repr)))

(define (rl-iter-rewind iter)
  (let* ([repr (rl-iter-repr iter)]
         [procs (rl-iter-procset iter)]
         [rewind-proc (rl-iter-procs-rewind procs)])
    (rl-iter (rewind-proc repr) procs)))

(define (rl-iter-name iter)
  (let ([repr (rl-iter-repr iter)]
        [name-proc (rl-iter-procs-name (rl-iter-procset iter))])
    (name-proc repr)))

(define (rl-iter-columns iter)
  (let ([repr (rl-iter-repr iter)]
        [columns-proc (rl-iter-procs-columns (rl-iter-procset iter))])
    (columns-proc repr)))

(define (rl-iter-index iter column-name value)
  (let* ([repr (rl-iter-repr iter)]
         [procs (rl-iter-procset iter)]
         [index (rl-iter-procs-index iter)])
    (index repr column-name value)))

; phantom tuple, used when implementing basic-iter
(struct rl-phantom-tuple ())

(struct rl-basic-iter (table cur-tuples))

(define (rl-build-basic-iter table)
  (define (rl-basic-iter-get basic-iter)
    (let ([cur-tuples (rl-basic-iter-cur-tuples basic-iter)])
      (cond [(rl-phantom-tuple? cur-tuples) (error "iterator not ready")]
            [(null? cur-tuples) (error "iterator at end")]
            [else (car cur-tuples)])))
  (define (rl-basic-iter-next basic-iter)
    (let ([table (rl-basic-iter-table basic-iter)]
          [cur-tuples (rl-basic-iter-cur-tuples basic-iter)])
      (cond [(null? cur-tuples) (error "iterator already at end")]
            [(rl-phantom-tuple? cur-tuples) (rl-basic-iter table (rl-table-tuples table))]
            [(rl-basic-iter table (cdr cur-tuples))])))
  (define (rl-basic-iter-test basic-iter)
    (let ([cur-tuples (rl-basic-iter-cur-tuples basic-iter)])
      (null? cur-tuples)))
  (define (rl-basic-iter-rewind basic-iter)
    (let ([table (rl-basic-iter-table basic-iter)])
      (rl-basic-iter table (rl-phantom-tuple))))
  (define (rl-basic-iter-name basic-iter)
    (let ([table (rl-basic-iter-table basic-iter)])
      (rl-table-name table)))
  (define (rl-basic-iter-columns basic-iter)
    (let ([table (rl-basic-iter-table basic-iter)])
      (rl-table-columns table)))
  (define (rl-basic-iter-indexable-check basic-iter column-name)
    (let* ([table (rl-basic-iter-table basic-iter)]
           [indexed-columns (rl-table-indexed-columns table)])
      (if (null? indexed-columns)
          false
          (not (false? (index-of indexed-columns column-name))))))
  (define (rl-basic-iter-index basic-iter column-name column-value)
    (let* ([table (rl-basic-iter-table basic-iter)]
           [indexed-columns (rl-table-indexed-columns table)]
           [index-maps (rl-table-index-maps table)]
           [subscript (index-of indexed-columns column-name)])
      (if (false? subscript)
           (error "column does not exist")
             (hash-ref (list-ref index-maps subscript) column-value))))
  (rl-iter #|repr|#  (rl-basic-iter table (rl-phantom-tuple))
           #|procs|# (rl-iter-procs rl-basic-iter-get
                                    rl-basic-iter-next
                                    rl-basic-iter-test
                                    rl-basic-iter-rewind
                                    rl-basic-iter-name
                                    rl-basic-iter-columns
                                    rl-basic-iter-indexable-check
                                    rl-basic-iter-index)))

(struct rl-cartesian-iter (base1 base2))

(define (rl-build-cartesian-iter base1 base2)
  (define (rl-cartesian-iter-get cartesian-iter)
    (let ([base1 (rl-cartesian-iter-base1 cartesian-iter)]
          [base2 (rl-cartesian-iter-base2 cartesian-iter)])
      ; (displayln (rl-iter-name base1))
      ; (displayln (rl-iter-name base2))
      (append (rl-iter-get base1) (rl-iter-get base2))))
  (define (rl-cartesian-iter-next cartesian-iter)
    (define (rl-cartesian-next-post-check cartesian-iter)
      (let ([base1 (rl-cartesian-iter-base1 cartesian-iter)]
            [base2 (rl-cartesian-iter-base2 cartesian-iter)])
        (if (rl-iter-test base2)
            (rl-cartesian-iter-next cartesian-iter)
            cartesian-iter)))
    (let ([base1 (rl-cartesian-iter-base1 cartesian-iter)]
          [base2 (rl-cartesian-iter-base2 cartesian-iter)])
      (cond [(rl-iter-test base1) (error "cartesian iterator at end")]
            [(rl-iter-test base2) (rl-cartesian-iter (rl-iter-next base1)
                                                     (rl-iter-next (rl-iter-rewind base2)))]
            [else (rl-cartesian-next-post-check (rl-cartesian-iter base1 (rl-iter-next base2)))])))
  (define (rl-cartesian-iter-test cartesian-iter)
    (let ([base1 (rl-cartesian-iter-base1 cartesian-iter)])
      (rl-iter-test base1)))
  (define (rl-cartesian-iter-rewind cartesian-iter)
    (let ([base1 (rl-cartesian-iter-base1 cartesian-iter)]
          [base2 (rl-cartesian-iter-base2 cartesian-iter)])
      (rl-cartesian-iter (rl-iter-next (rl-iter-rewind base1))
                         (rl-iter-rewind base2))))
  (define (rl-cartesian-iter-name cartesian-iter)
    (let ([base1 (rl-cartesian-iter-base1 cartesian-iter)]
          [base2 (rl-cartesian-iter-base2 cartesian-iter)])
      (string-append "<" (rl-iter-name base1) " * " (rl-iter-name base2) ">")))
  (define (rl-cartesian-iter-columns cartesian-iter)
    (let ([base1 (rl-cartesian-iter-base1 cartesian-iter)]
          [base2 (rl-cartesian-iter-base2 cartesian-iter)])
      (append (rl-iter-columns base1) (rl-iter-columns base2))))
  (rl-iter #|repr|#  (rl-cartesian-iter (rl-iter-next base1) base2)
           #|procs|# (rl-iter-procs rl-cartesian-iter-get
                                    rl-cartesian-iter-next
                                    rl-cartesian-iter-test
                                    rl-cartesian-iter-rewind
                                    rl-cartesian-iter-name
                                    rl-cartesian-iter-columns
                                    null
                                    null)))

(struct rl-projection-iter (base))

(define (rl-build-projection-iter base column-names)
  (define (rl-build-all-column-selectors table-columns column-names)
    (map (lambda (column-name) (rl-build-column-selector table-columns column-name)) 
         column-names))
  (let* ([base-columns (rl-iter-columns base)]
         [all-column-selectors (rl-build-all-column-selectors base-columns column-names)])
    (define (rl-projection-iter-get projection-iter)
      (let ([cur-tuple (rl-iter-get (rl-projection-iter-base projection-iter))])
        (map (lambda (f) (f cur-tuple))
             all-column-selectors)))
    (define (rl-projection-iter-next projection-iter)
      (let ([base (rl-projection-iter-base projection-iter)])
        (rl-projection-iter (rl-iter-next base))))
    (define (rl-projection-iter-test projection-iter)
      (let ([base (rl-projection-iter-base projection-iter)])
        (rl-iter-test base)))
    (define (rl-projection-iter-rewind projection-iter)
      (let ([base (rl-projection-iter-base projection-iter)])
        (rl-projection-iter (rl-iter-rewind base))))
    (define (rl-projection-iter-name projection-iter)
      (let ([base (rl-projection-iter-base projection-iter)])
        (string-append "PI<" 
                       (rl-iter-name base)
                       "; "
                       (~a (rl-projection-iter-columns projection-iter)) 
                       ">")))
    (define (rl-projection-iter-columns projection-iter)
      column-names)
    (rl-iter #|repr|#  (rl-projection-iter base)
             #|procs|# (rl-iter-procs rl-projection-iter-get
                                      rl-projection-iter-next
                                      rl-projection-iter-test
                                      rl-projection-iter-rewind
                                      rl-projection-iter-name
                                      rl-projection-iter-columns
                                      null
                                      null))))

(struct rl-select-iter (base))

(define (rl-build-select-iter base raw-condition)
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
  (let ([compiled-condition (rl-compile-expr (rl-iter-columns base) raw-condition)])
    (define (rl-select-iter-get select-iter)
      (let ([base (rl-select-iter-base select-iter)])
        (rl-iter-get base)))
    (define (rl-select-iter-next select-iter)
      (define (rl-select-iter-next-intern base)
        (cond [(rl-iter-test base) base]
              [(compiled-condition (rl-iter-get base)) base]
              [else (rl-select-iter-next-intern (rl-iter-next base))]))
      (rl-select-iter (rl-select-iter-next-intern (rl-iter-next (rl-select-iter-base select-iter)))))
    (define (rl-select-iter-test select-iter)
      (rl-iter-test (rl-select-iter-base select-iter)))
    (define (rl-select-iter-rewind select-iter)
      (rl-select-iter (rl-iter-rewind (rl-select-iter-base select-iter))))
    (define (rl-select-iter-name select-iter)
      (string-append "SIGMA<"
                     (rl-iter-name (rl-select-iter-base select-iter))
                     ";"
                     (~a (raw-expr->string raw-condition))
                     ">"))
    (define (rl-select-iter-columns select-iter)
      (rl-iter-columns (rl-select-iter-base select-iter)))
    (rl-iter #|repr|#  (rl-select-iter base)
             #|procs|# (rl-iter-procs rl-select-iter-get
                                      rl-select-iter-next
                                      rl-select-iter-test
                                      rl-select-iter-rewind
                                      rl-select-iter-name
                                      rl-select-iter-columns
                                      null
                                      null))))

(define (rl-iter-traverse iter)
  (displayln (rl-iter-name iter))
  (displayln (~a (rl-iter-columns iter)))
  (define (rl-iter-traverse-int iter)
    (if (rl-iter-test iter)
        (displayln "")
        (begin (writeln (rl-iter-get iter))
               (rl-iter-traverse-int (rl-iter-next iter)))))
  (rl-iter-traverse-int (rl-iter-next iter)))

(struct rl-indexed-select-iter (base))

(struct rl-equiv-join-iter (iter1 table2))

(define players-table
  (rl-build-table "players-table"
                  '("pno" "pname" "pteam")
                  '((1 "QDU.Sumoon" "Qingdao University")
                    (2 "BUG.Chu1gda" "BUGaming")
                    (3 "ICE.1000" "Internal Compiler Error")
                    (4 "CHUK-SZ.ZYF" "CHinese University of HongKong (Shenzhen)"))))

(define tools-table
  (rl-build-table "tools-table"
                  '("tno" "tname" "tvendor")
                  '((1 "Dev-CPP" "ACM-ICPC")
                    (2 "Intellij-IDEA" "Jetbrains")
                    (3 "QtCreator" "Digia")
                    (4 "CLion" "Jetbrains"))))

(define players-tools-table
  (rl-build-table "players-tools-table"
                  '("pno1" "tno1")
                  '((1 1)
                    (2 3)
                    (3 2)
                    (4 4))))

(define players-table-iter (rl-build-basic-iter players-table))

(define tools-table-iter (rl-build-basic-iter tools-table))

(define players-tools-table-iter (rl-build-basic-iter players-tools-table))

(define jetbrains-tools-table-iter
  (rl-build-select-iter tools-table-iter (list equal? (rl-ref "tvendor") "Jetbrains")))

(define three-cartesian-iter
  (rl-build-cartesian-iter (rl-build-cartesian-iter players-table-iter 
                                                    tools-table-iter) 
                           players-tools-table-iter))

(define selected-iter1
  (rl-build-select-iter three-cartesian-iter (list = (rl-ref "tno") (rl-ref "tno1"))))

(define selected-iter2
  (rl-build-select-iter selected-iter1 (list = (rl-ref "pno") (rl-ref "pno1"))))

(define final-projection-iter
  (rl-build-projection-iter selected-iter2 (list "pno" "pname" "pteam" "tname" "tvendor")))

(define final-selected-iter
  (rl-build-select-iter final-projection-iter (list equal? (rl-ref "tvendor") "Jetbrains")))

(rl-iter-traverse players-table-iter)

(rl-iter-traverse jetbrains-tools-table-iter)

(rl-iter-traverse final-projection-iter)

(rl-iter-traverse final-selected-iter)