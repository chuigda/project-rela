#lang racket

(require "util.rkt")
(require "table.rkt")

; making eval work
(current-namespace (make-base-namespace))

; for building name references in raw expressions
(provide rl-ref rl-ref? rl-ref-var)
(provide rl-input rl-input? rl-input-var)

(struct rl-ref (var))
(struct rl-input (var type))

; raw expression compiler
(provide raw-expr->string
         rl-compile-expr
         rl-expr-vars)

(define (raw-expr->string raw-expr)
  (define (rl-ref-replace raw-expr)
    (map-recur (lambda (x) 
                 (cond [(rl-ref? x) (rl-ref-var x)]
                       [(rl-input? x) (string-append "#!" (rl-input-var x) 
                                                     "@" (~a (rl-input-type x)))]
                       [else x]))
               raw-expr))
  (string-replace (~a (rl-ref-replace raw-expr)) "procedure:" ""))

(define (rl-compile-expr table-columns raw-expr)
  (define (rl-compile-item item tuple)
    (cond [(rl-ref? item)
           (let ([column-name (rl-ref-var item)])
             (list (rl-build-column-selector table-columns column-name) 
                   (list (lambda () tuple))))]
          [(rl-input? item) (unimplemented)]
          [(list? item) (rl-compile-list item tuple)]
          [else item]))
  (define (rl-compile-list the-list tuple)
    (map (lambda (item) (rl-compile-item item tuple)) the-list))
  (lambda (tuple) (eval (map (lambda (item) (rl-compile-item item tuple))
                             raw-expr))))

(define (rl-expr-vars raw-expr)
  (flatten
    (remove* (list false)
             (map (lambda (expr-item)
                    (cond [(list? expr-item) (rl-expr-vars expr-item)]
                          [(rl-ref? expr-item) (rl-ref-var expr-item)]
                          [else null]))
                  raw-expr))))
