#lang racket

(require
 "circuit.rkt"
 "timing.rkt"
 "sat-vars.rkt"
 "sat-encoder.rkt"
 "dimacs.rkt")

;; ------------------------------------------------------------
;; Circuit
;; ------------------------------------------------------------

(define ckt sample-c17)

;; ------------------------------------------------------------
;; Timing Analysis
;; ------------------------------------------------------------

(displayln "=== Arrival Times ===")

(define arrivals
  (compute-arrival-times ckt))

(for-each
 (lambda (kv)
   (printf "~a -> ~a~n"
           (car kv)
           (cdr kv)))

 (hash->list arrivals))

;; ------------------------------------------------------------
;; SAT Encoding
;; ------------------------------------------------------------

(define Tmax 5)

(displayln "")
(displayln "=== Generating SAT Encoding ===")

(define clauses
  (generate-sat-instance ckt Tmax))

;; ------------------------------------------------------------
;; Count variables
;; ------------------------------------------------------------

(define num-vars
  (apply max
         (map abs
              (apply append clauses))))

(printf "Variables: ~a~n" num-vars)
(printf "Clauses:   ~a~n"
        (length clauses))

;; ------------------------------------------------------------
;; Write DIMACS
;; ------------------------------------------------------------

(write-dimacs
 "timing.cnf"
 clauses
 num-vars)

(displayln "")
(displayln "DIMACS written to timing.cnf")

(displayln "")
(displayln "You can now run:")
(displayln "minisat timing.cnf result.out")

