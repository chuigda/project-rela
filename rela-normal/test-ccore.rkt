#lang racket

(require racket/trace)
(require errortrace)

(require "ccore.rkt")
(require "rela.rkt")
(require "util.rkt")

(define students-table (rl-build-table-info "students" '("sno" "sname" "sdept") '("sno")))
(define courses-table (rl-build-table-info "courses" '("cno" "cname" "cdesc") '("cno")))
(define sc-table (rl-build-table-info "sc" '("sno1" "cno1" "score") '("sno1" "cno1")))

(define three-cartesian (rl-build-cartesian (list students-table courses-table sc-table)))

(for-each (lambda (tree) (displayln (tree 'disp)))
          (rl-optimize three-cartesian
                       (list (list = (rl-ref "sno") (rl-ref "sno1"))
                             (list = (rl-ref "cno") (rl-ref "cno1"))
                             (list >= (rl-ref "score") 60))))
