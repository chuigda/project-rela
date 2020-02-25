#lang racket

(require "util.rkt")

(require racket/format)
(require racket/trace)
(require errortrace)

(require srfi/2)
(require srfi/43)

(provide list->svec
         svec-get
         svec-ref)

; ordered indexing mechanism implemented with sorted vectors
(define (list->svec list-of-pairs)
  (vector-sort (list->vector list-of-pairs)
               (lambda (p1 p2) (less? (car p1) (car p2)))))

(define (svec-get svec key)
  (define (cmp x1 x2)
    (cond [(pair? x1) (cmp (car x1) x2)]
          [(pair? x2) (cmp x1 (car x2))]
          [(equal? x1 x2) 0]
          [(less? x1 x2) -1]
          [(greater? x1 x2) 1]))
  (let ([subscript (vector-binary-search svec key cmp)])
    (if (false? subscript)
        false
        (cdr (vector-ref svec subscript)))))

(define (svec-ref svec idx)
  (if (>= idx (vector-length svec))
      #f
      (vector-ref svec idx)))
