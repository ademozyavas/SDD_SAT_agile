#lang racket

(require "circuit.rkt"
         "sat-vars.rkt"
         "sat-encoder.rkt"
         "optimization.rkt"
         "write-wcnf.rkt"
         "fault-model.rkt")

;; ============================================================
;; Temporary circuit for 3G optimization testing
;; ============================================================
;;
;; Path 1:
;;
;;   a -> n1 -> n3 -> out
;;   2     3      4
;;
;;   total delay = 9
;;
;; Path 2:
;;
;;   c -> n2 -> n3 -> out
;;   1     3      4
;;
;;   total delay = 8
;;
;; ============================================================

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

;; Fault site.
;;
;; This means the selected path must contain n1.
;; Therefore the optimizer should select:
;;
;;   a -> n1 -> n3 -> out
;;
;; with delay 9.
(define fault
  (sdd-fault 'n1))

;; Reachability universe.
;;
;; Longest path delay is 9.
(define Tmax 9)

;; ============================================================
;; Generate hard clauses
;; ============================================================

(define hard-clauses
  (generate-sat-instance
   sample-3g
   Tmax
   fault))

;; ============================================================
;; Generate weighted soft clauses
;; ============================================================

(define soft-clauses
  (generate-path-delay-soft-clauses
   sample-3g))

;; ==========================================test-3g-wcnf.rkt==================
;; Number of variables
;; ============================================================

(define num-vars
  (sat-var-count))

;; ============================================================
;; Write WCNF
;; ============================================================

(write-wcnf
 "3g.wcnf"
 hard-clauses
 soft-clauses
 num-vars)

(write-sat-vars "3g.vars")

;; ============================================================
;; Report
;; ============================================================

(printf "\n========================================\n")
(printf "3G WCNF INSTANCE\n")
(printf "========================================\n")
(printf "Variables:       ~a\n" num-vars)
(printf "Hard clauses:   ~a\n" (length hard-clauses))
(printf "Soft clauses:   ~a\n" (length soft-clauses))
(printf "Total clauses:  ~a\n"
        (+ (length hard-clauses)
           (length soft-clauses)))
(printf "========================================\n")

(printf "\nSoft clauses:\n")
(for-each
 (lambda (sc)
   (printf "  weight = ~a, literal = ~a\n"
           (car sc)
           (cdr sc)))
 soft-clauses)

(printf "\nWCNF written to 3g.wcnf\n")
