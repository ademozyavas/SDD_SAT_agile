#lang racket

(require "circuit.rkt"
         "sat-vars.rkt"
         "universe.rkt"
         "fault-model.rkt"
         "path-constraints.rkt")

;; ============================================================
;; TEST CONFIGURATION
;; ============================================================

(define circuit
  sample-c17)

(define fault
  (sdd-fault 'n1))

(define Tmax
  3)

(define frames
  '(0 1))


;; ============================================================
;; BUILD SAT UNIVERSE
;; ============================================================

(build-sat-universe
 circuit
 Tmax
 frames)


;; ============================================================
;; GENERATE 3F CLAUSES
;; ============================================================

(define clauses
  (generate-3f-clauses
   circuit
   fault))


;; ============================================================
;; BASIC TESTS
;; ============================================================

(define (check condition message)
  (if condition
      (printf "[PASS] ~a\n" message)
      (printf "[FAIL] ~a\n" message)))


(printf "\n")
(printf "========================================\n")
(printf "3F VALIDATION\n")
(printf "========================================\n")


;; ------------------------------------------------------------
;; Test 1: Clauses were generated
;; ------------------------------------------------------------

(check
 (> (length clauses) 0)
 "3F clauses were generated")


;; ------------------------------------------------------------
;; Test 2: Fault site is valid
;; ------------------------------------------------------------

(check
 (valid-fault-site?
  circuit
  fault)
 "Fault site n1 is a valid gate")


;; ------------------------------------------------------------
;; Test 3: Fault-site variable exists
;; ------------------------------------------------------------

(define n1-path-node
  (path-node-var-id 'n1))

(check
 (number? n1-path-node)
 "PATH_NODE(n1) variable exists")


;; ------------------------------------------------------------
;; Test 4: Transition variable exists
;; ------------------------------------------------------------

(define n1-transition
  (transition-var-id 'n1))

(check
 (number? n1-transition)
 "TRANSITION(n1) variable exists")


;; ------------------------------------------------------------
;; Test 5: Path-edge variables exist
;; ------------------------------------------------------------

(define a-n1
  (path-var-id 'a 'n1))

(define n1-n3
  (path-var-id 'n1 'n3))

(define n3-out
  (path-var-id 'n3 'out))

(check
 (and (number? a-n1)
      (number? n1-n3)
      (number? n3-out))
 "Path-edge variables exist")


;; ------------------------------------------------------------
;; Test 6: Path-output variable exists
;; ------------------------------------------------------------

(define output-var
  (path-output-var-id 'out))

(check
 (number? output-var)
 "PATH_OUTPUT(out) variable exists")


;; ------------------------------------------------------------
;; Test 7: Different SAT variables have different IDs
;; ------------------------------------------------------------

(check
 (and (not (= a-n1 n1-n3))
      (not (= n1-n3 n3-out))
      (not (= n1-path-node n1-transition)))
 "Different propositions have different SAT IDs")


;; ------------------------------------------------------------
;; Test 8: Same proposition gets same ID
;; ------------------------------------------------------------

(check
 (= (path-var-id 'a 'n1)
    (path-var-id 'a 'n1))
 "SAT variable allocation is stable")


;; ------------------------------------------------------------
;; Dump useful information
;; ------------------------------------------------------------

(printf "\n")
(printf "Generated clauses: ~a\n"
        (length clauses))

(printf "\nSelected variable IDs:\n")
(printf "PATH(a,n1)      = ~a\n" a-n1)
(printf "PATH(n1,n3)     = ~a\n" n1-n3)
(printf "PATH(n3,out)    = ~a\n" n3-out)
(printf "PATH_NODE(n1)   = ~a\n" n1-path-node)
(printf "TRANSITION(n1)  = ~a\n" n1-transition)
(printf "PATH_OUTPUT(out)= ~a\n" output-var)

(printf "\n")

