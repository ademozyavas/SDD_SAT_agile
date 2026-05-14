#lang racket



(struct input-node (name)
  #:transparent)

(struct gate-node (name
                   gate-type
                   fanins
                   delay)
  #:transparent)

(struct output-node (name
                     source)
  #:transparent)

(struct circuit (inputs
                 gates
                 outputs)
  #:transparent)

;; ------------------------------------------------------------
;; Tiny hard-coded combinational circuit
;; Inspired by c17.bench style
;;
;; n1 = AND(a,b)
;; n2 = NAND(c,d)
;; n3 = OR(n1,n2)
;; out = NOT(n3)
;;
;; Delays:
;; AND  = 1
;; NAND = 1
;; OR   = 1
;; NOT  = 1
;;
;; Longest path to out = 3
;; ------------------------------------------------------------

(define sample-c17
  (circuit
   (list
    (input-node 'a)
    (input-node 'b)
    (input-node 'c)
    (input-node 'd))

   (list
    (gate-node 'n1 'and '(a b) 1)
    (gate-node 'n2 'nand '(c d) 1)
    (gate-node 'n3 'or '(n1 n2) 1)
    (gate-node 'out 'not '(n3) 1))

   (list
    (output-node 'OUT 'out))))


(provide
 ;;Export all bindings related to the struct input-node
 (struct-out input-node)
 (struct-out gate-node)
 (struct-out output-node)
 (struct-out circuit)
 sample-c17)
