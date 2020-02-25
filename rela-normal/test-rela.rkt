#lang racket

(require rackunit)
(require errortrace)

(require "util.rkt")
(require "rela.rkt")

(define players-table
  (rl-build-table "players-table"
                  '("pno" "pname" "pteam")
                  '((1 "QDU.Sumoon" "Qingdao University")
                    (2 "BUG.Chu1gda" "BUGaming")
                    (3 "ICE.1000" "Internal Compiler Error")
                    (4 "CHUK-SZ.ZYF" "CHinese University of HongKong (Shenzhen)")
                    (5 "ICE.Hoshino" "Internal Compilter Error"))
                  "pno" "pname"))

(define tools-table
  (rl-build-table "tools-table"
                  '("tno" "tname" "tvendor")
                  '((1 "Dev-CPP" "ACM-ICPC")
                    (2 "Intellij-IDEA" "Jetbrains")
                    (3 "QtCreator" "Digia")
                    (4 "CLion" "Jetbrains"))
                  "tno" "tname"))
  
(define players-tools-table
  (rl-build-table "players-tools-table"
                  '("pno1" "tno1")
                  '((1 1)
                    (2 3)
                    (3 2)
                    (4 4)
                    (5 2))
                  "pno1" "tno1"))

(define players-table-iter (rl-build-basic-iter players-table))

(writeln ((rl-iter-index players-table-iter "pno") 2))
(writeln ((rl-iter-index players-table-iter "pname") "ICE.1000"))
(displayln "")

(define tools-table-iter (rl-build-basic-iter tools-table))

(define players-tools-table-iter (rl-build-basic-iter players-tools-table))

(define pt-iter players-tools-table-iter)

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

(define players-tools-equiv-join-iter
  (rl-build-equiv-join-iter
   (rl-build-equiv-join-iter pt-iter players-table-iter "pno1" "pno")
   tools-table-iter "tno1" "tno"))

(rl-iter-traverse players-tools-equiv-join-iter)
