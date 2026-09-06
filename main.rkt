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
(define rt
  (compute-required-times sample-c17 4))

(for-each
 (lambda (kv)
   (printf "~a -> ~a~n"
           (car kv)
           (cdr kv)))

 (hash->list arrivals))
;;hash->list is a builti-in function that converts a hash to
;;a list of pairs (key-value pairs, (key.value)), in an arbitrary
;;order. You can sort by key: (hash->list my-hash #:try-order? #t)
;;in Racket #: denotes a keyword to pass optional or named args
;;to functions.
(displayln "=== Required Times ===")(for-each
 (lambda (kv)
   (printf "~a -> ~a~n"
           (car kv)
           (cdr kv)))

 (hash->list rt))

;; ------------------------------------------------------------
;; SAT Encoding
;; ------------------------------------------------------------

;; desired maximum delay bound
(define Tmax 5)

(displayln "")
(displayln "=== Generating SAT Encoding ===")

(define clauses
  (generate-sat-instance ckt Tmax))

;; ------------------------------------------------------------
;; Count variables
;; ------------------------------------------------------------
;; DIMAC file's first line reports the number of variables
(define num-vars
  (apply max
         (map abs
              (apply append clauses))))

(printf "Variables: ~a~n" num-vars)
(printf "Clauses:   ~a~n"
        (length clauses))

(dump-sat-vars)
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

