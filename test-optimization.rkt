#lang racket

(require "circuit.rkt"
         "optimization.rkt")

(define soft-clauses
  (generate-path-delay-soft-clauses
   sample-c17))

(printf "\n========================================\n")
(printf "3G PATH DELAY OBJECTIVE\n")
(printf "========================================\n")

(for-each
 (lambda (weighted-clause)
   (printf "weight = ~a, variable = ~a\n"
           (car weighted-clause)
           (first (cdr weighted-clause))))
 soft-clauses)

(printf "\nTotal soft clauses: ~a\n"
        (length soft-clauses))
