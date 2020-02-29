#lang racket

(require "ccore.rkt")
(require "rela.rkt")

(require racket/trace)
(require errortrace)

(define students-table (rl-build-table-info "students" '("sno" "sname" "sdept") '("sno")))
(define courses-table (rl-build-table-info "courses" '("cno" "cname" "cdesc") '("cno")))
(define sc-table (rl-build-table-info "sc" '("sno1" "cno1") '("sno1" "cno1")))

(define three-cartesian (rl-build-cartesian (list students-table courses-table sc-table)))

(three-cartesian 'disp)

(for-each (lambda (tree) (displayln (tree 'disp)))
          (rl-optimize three-cartesian
                       (list (list = (rl-ref "sno") (rl-ref "sno1"))
                             (list = (rl-ref "cno") (rl-ref "cno1")))))
