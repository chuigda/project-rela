#lang racket

(require "ccore.rkt")
(require "expr.rkt")
(require "util.rkt")

(define (rl-c-build-column-selector tuple-name table-columns column-name)
  (let ([column-index (index-of table-columns column-name)])
    (if (equal? column-index false)
        (error (string-append "column " column-name " does not exist"))
        (string-append "k_tuple_fetch(" tuple-name ", " (~a column-index) ")")))))

(struct rl-c-operator (repr type param-type out-type))

(define (rl-c-compile-expr tuple-name table-columns raw-expr)
  (define (rl-c-compile-item item tuple)
    (cond [(rl-ref? item) 
           (cons (rl-c-build-column-selector tuple-name
                                             table-columns
                                             (rl-ref-var item)
                 'str)]
          [(rl-input? item) 
           (cons (string-append "_in_" (rl-input-var 9tem)) (rl-input-type item))]
          [(list? item) (rl-c-compile-list item)]
          [(number? item) (cons (~a item) 'int)]
          [(string? item) (cons (string-append "\"" item "\"") 'str)]
          [else (unimplemented)])))
  (define (rl-c-decode-int operand)
    (string-append "k_decode_int32(" operand ")"))
  (define (rl-c-compile-operator op)
    (case op [+        (rl-c-operator "+"        'binary 'int  'int)]
             [-        (rl-c-operator "-"        'binary 'int  'int)]
             [*        (rl-c-operator "*"        'binary 'int  'int)]
             [/        (rl-c-operator "/"        'binary 'int  'int)]
             [<        (rl-c-operator "<"        'binary 'int  'bool)]
             [>        (rl-c-operator ">"        'binary 'int  'bool)]
             [<=       (rl-c-operator "<"        'binary 'int  'bool)]
             [>=       (rl-c-operator ">"        'binary 'int  'bool)]
             [=        (rl-c-operator "=="       'binary 'int  'bool)]
             [equal?   (rl-c-operator "k_strcmp" 'fn2    'str  'bool)]
             [and-proc (rl-c-operator "&&"       'binary 'bool 'bool)]
             [or-proc  (rl-c-operator "||"       'binary 'bool 'bool)]
             [not      (rl-c-operator "!"        'unary  'bool 'bool)]))
  (define (rl-c-compile-list item)
    (let* ([operator (car item)]
           [compiled-op (rl-c-compile-operator operator)]
           [args (cdr item)]
           [compiled-args (map (lambda (item) (rl-c-compile-item item tuple)) args)]
           [compiled-args-type (map (lambda (compiled-arg) (cdr compiled-arg)))])
      (unimplemented)))
)
