#lang racket

(require "util.rkt")
(require "table.rkt")

; making eval work
(current-namespace (make-base-namespace))

; for building name references in raw expressions
(provide rl-ref rl-ref? rl-ref-var)

(struct rl-ref (name))
(define rl-ref-var rl-ref-name)

; raw expression compiler
(provide raw-expr->string
         rl-compile-expr
         rl-expr-vars)

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

(define (rl-expr-vars raw-expr) (unimplemented))
