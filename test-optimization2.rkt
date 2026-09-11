#lang racket

(require "circuit.rkt"
         "universe.rkt"
         "optimization.rkt")

;; Temporary circuit for testing path-delay optimization.
;;
;; Path 1:
;;   a -> n1 -> n3 -> out
;;   delay = 2 + 3 + 4 = 9
;;
;; Path 2:
;;   c -> n2 -> n3 -> out
;;   delay = 1 + 3 + 4 = 8

(define sample-3g
  (circuit
   (list
    (input-node 'a)
    (input-node 'b)
    (input-node 'c)
    (input-node 'd))

   (list
    (gate-node 'n1 'and '(a b) 2)
    (gate-node 'n2 'nand '(c d) 1)
    (gate-node 'n3 'or '(n1 n2) 3)
    (gate-node 'out 'not '(n3) 4))

   (list
    (output-node 'OUT 'out))))

;; Generate the soft clauses.
(define soft-clauses
  (generate-path-delay-soft-clauses
   sample-3g))

(displayln soft-clauses)
