#lang racket

(require "circuit.rkt"
         "fault-model.rkt"
         "sat-encoder.rkt"
         "sat-vars.rkt"
         "dimacs.rkt")

;; ------------------------------------------------------------
;; 3F-4
;; Generate complete SAT instance and write DIMACS CNF
;; ------------------------------------------------------------

(define circuit sample-c17)

;; Fault site
(define fault
  (sdd-fault 'n1))

;; Maximum reachability depth.
;;
;; sample-c17:
;;   a/b -> n1/n2 -> n3 -> out
;; Longest path = 3 gates.
(define Tmax 3)

;; Generate complete SAT instance
(define clauses
  (generate-sat-instance
   circuit
   Tmax
   fault))

;; Number of SAT variables
(define num-vars
  (sat-var-count))

;; Write DIMACS
(write-dimacs
 "3f.cnf"
 clauses
 num-vars)

(printf "========================================\n")
(printf "3F-4 CNF GENERATION\n")
(printf "========================================\n")
(printf "Fault site: ~a\n"
        (sdd-fault-site fault))
(printf "Tmax: ~a\n"
        Tmax)
(printf "SAT variables: ~a\n"
        num-vars)
(printf "Clauses: ~a\n"
        (length clauses))
(printf "DIMACS file: 3f.cnf\n")
(printf "[PASS] CNF file generated.\n")
