#lang racket

(require "circuit.rkt"
         "sat-vars.rkt"
         "sat-encoder.rkt"
         "optimization.rkt"
         "write-wcnf.rkt"
         "fault-model.rkt")

;; ============================================================
;; FINAL 3G TEST CIRCUIT
;; ============================================================

(define sample-3g-final
  (circuit
   (list
    (input-node 'a)
    (input-node 'b)
    (input-node 'c)
    (input-node 'd))

   (list
    (gate-node 'n1 'and '(a b) 1)
    (gate-node 'n2 'nand '(c d) 4)
    (gate-node 'n3 'or '(n1 n2) 2)
    (gate-node 'out 'not '(n3) 1))

   (list
    (output-node 'OUT 'out))))

;; ============================================================
;; Fault site
;; ============================================================

(define fault
  (sdd-fault 'n2))

;; ============================================================
;; Maximum possible path delay
;;
;; n2 branch:
;;
;;     4 + 2 + 1 = 7
;; ============================================================

(define Tmax 7)

;; ============================================================
;; Generate hard clauses
;; ============================================================

(define hard-clauses
  (generate-sat-instance
   sample-3g-final
   Tmax
   fault))

;; ============================================================
;; Generate weighted objective
;; ============================================================

(define soft-clauses
  (generate-path-delay-soft-clauses
   sample-3g-final))

;; ============================================================
;; Number of variables
;; ============================================================

(define num-vars
  (sat-var-count))

;; ============================================================
;; Write WCNF and variable metadata
;; ============================================================

(write-wcnf
 "3g-final.wcnf"
 hard-clauses
 soft-clauses
 num-vars)

(write-sat-vars
 "3g-final.vars")

;; ============================================================
;; Report
;; ============================================================

(printf "\n========================================\n")
(printf "FINAL 3G TEST INSTANCE\n")
(printf "========================================\n")
(printf "Variables: ~a\n" num-vars)
(printf "Hard clauses: ~a\n" (length hard-clauses))
(printf "Soft clauses: ~a\n" (length soft-clauses))
(printf "Tmax: ~a\n" Tmax)
(printf "Fault site: ~a\n"
        (sdd-fault-site fault))

(printf "\nExpected optimal paths:\n")
(printf "  c -> n2 -> n3 -> out : delay = 7\n")
(printf "  d -> n2 -> n3 -> out : delay = 7\n")

(printf "\nExpected non-optimal paths:\n")
(printf "  a -> n1 -> n3 -> out : delay = 4\n")
(printf "  b -> n1 -> n3 -> out : delay = 4\n")

(printf "\nWCNF written to 3g-final.wcnf\n")
(printf "Variable map written to 3g-final.vars\n")
