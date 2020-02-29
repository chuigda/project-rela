#lang racket

(require "svec.rkt")
(require "util.rkt")

(provide rl-build-table
         rl-table-name
         rl-table-columns
         rl-table-tuples
         rl-table-indexed-columns
         rl-table-index-maps
         rl-build-column-selector)

; basic table
(struct rl-table (name columns tuples indexed-columns index-maps))

; used in many procedures
(define (rl-build-column-selector table-columns column-name)
  (let ([column-index (index-of table-columns column-name)])
    (if (equal? column-index false)
        (error (string-append "column " column-name " does not exist"))
        (lambda (tuple) (list-ref tuple column-index)))))

(define (rl-build-table name columns tuples . indexed-columns)
  (define (rl-build-one-index indexed-column)
    (let* ([column-selector (rl-build-column-selector columns indexed-column)]
           [tuples-transformer (lambda (tuple) (cons (column-selector tuple) tuple))])
      (list->svec (map tuples-transformer tuples))))
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
