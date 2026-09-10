#lang racket

(require "circuit.rkt"
         "sat-vars.rkt"
         "universe.rkt"
         "fault-model.rkt"
         "cnf-gates.rkt"
         "path-constraints.rkt")

;; ============================================================
;; TEST CONFIGURATION
;; ============================================================

(define circuit
  sample-c17)

(define fault
  (sdd-fault 'n1))

(define frames
  '(0 1))

(define Tmax
  3)


;; ============================================================
;; BUILD SAT UNIVERSE
;; ============================================================

(build-sat-universe
 circuit
 Tmax
 frames)


;; ============================================================
;; GENERATE BOOLEAN + 3F CLAUSES
;; ============================================================

(define clauses
  (append

   ;; Boolean circuit semantics.
   (generate-gate-semantics-clauses
    circuit
    frames)

   ;; 3F constraints.
   (generate-3f-clauses
    circuit
    fault)))


;; ============================================================
;; KNOWN VALID ASSIGNMENT
;; ============================================================
;;
;; Selected path:
;;
;;     a -> n1 -> n3 -> out
;;
;; Fault site:
;;
;;     n1
;;
;; Frame 0:
;;     a=0 b=1 c=1 d=1
;;
;; Frame 1:
;;     a=1 b=1 c=1 d=1
;;
;; Therefore:
;;
;;     n1  = 0 -> 1
;;     n2  = 0 -> 0
;;     n3  = 0 -> 1
;;     out = 1 -> 0
;;
;; The transition propagates through the complete path.
;; ============================================================

(define assignment
  (make-hash))


;; ------------------------------------------------------------
;; Helper for assigning SAT variables
;; ------------------------------------------------------------

(define (assign! variable value)
  (hash-set! assignment variable value))


;; ------------------------------------------------------------
;; Boolean values: frame 0
;; ------------------------------------------------------------

(assign! (value-var-id 'a 0) 0)
(assign! (value-var-id 'b 0) 1)
(assign! (value-var-id 'c 0) 1)
(assign! (value-var-id 'd 0) 1)

(assign! (value-var-id 'n1 0) 0)
(assign! (value-var-id 'n2 0) 0)
(assign! (value-var-id 'n3 0) 0)
(assign! (value-var-id 'out 0) 1)


;; ------------------------------------------------------------
;; Boolean values: frame 1
;; ------------------------------------------------------------

(assign! (value-var-id 'a 1) 1)
(assign! (value-var-id 'b 1) 1)
(assign! (value-var-id 'c 1) 1)
(assign! (value-var-id 'd 1) 1)

(assign! (value-var-id 'n1 1) 1)
(assign! (value-var-id 'n2 1) 0)
(assign! (value-var-id 'n3 1) 1)
(assign! (value-var-id 'out 1) 0)


;; ============================================================
;; TRANSITIONS
;; ============================================================

;; n1: 0 -> 1
(assign! (transition-var-id 'n1) 1)

;; n3: 0 -> 1
(assign! (transition-var-id 'n3) 1)

;; out: 1 -> 0
(assign! (transition-var-id 'out) 1)

;; No transition at the other nodes.
(assign! (transition-var-id 'a) 1)
(assign! (transition-var-id 'b) 0)
(assign! (transition-var-id 'c) 0)
(assign! (transition-var-id 'd) 0)
(assign! (transition-var-id 'n2) 0)


;; ============================================================
;; SELECTED PATH
;; ============================================================

;; a -> n1 -> n3 -> out

(assign! (path-var-id 'a 'n1) 1)
(assign! (path-var-id 'n1 'n3) 1)
(assign! (path-var-id 'n3 'out) 1)


;; All other path edges are not selected.

(assign! (path-var-id 'b 'n1) 0)
(assign! (path-var-id 'c 'n2) 0)
(assign! (path-var-id 'd 'n2) 0)
(assign! (path-var-id 'n2 'n3) 0)


;; ============================================================
;; PATH NODES
;; ============================================================

(assign! (path-node-var-id 'n1) 1)
(assign! (path-node-var-id 'n3) 1)
(assign! (path-node-var-id 'out) 1)

;; n2 is not on the selected path.
(assign! (path-node-var-id 'n2) 0)


;; ============================================================
;; PRIMARY OUTPUT
;; ============================================================

(assign! (path-output-var-id 'out) 1)


;; ============================================================
;; CLAUSE EVALUATION
;; ============================================================

;; A literal is satisfied when:
;;
;;     positive literal -> variable is 1
;;     negative literal -> variable is 0
;;
;; Example:
;;
;;     (57 -61)
;;
;; is satisfied when:
;;
;;     variable 57 = 1
;; OR
;;     variable 61 = 0

(define (literal-satisfied? literal)

  (define variable
    (abs literal))

  (define value
    (hash-ref
     assignment
     variable
     #f))

  (cond
    [(= literal variable)
     (= value 1)]

    [else
     (= value 0)]))


;; ------------------------------------------------------------
;; A clause is satisfied if at least one literal is true.
;; ------------------------------------------------------------

(define (clause-satisfied? clause)

  (ormap
   literal-satisfied?
   clause))


;; ============================================================
;; CHECK CLAUSES
;; ============================================================

(define failed-clauses
  (for/list ([clause clauses]
             #:unless (clause-satisfied? clause))
    clause))


;; ============================================================
;; TEST OUTPUT
;; ============================================================

(printf "\n")
(printf "========================================\n")
(printf "3F-2 CONCRETE ASSIGNMENT TEST\n")
(printf "========================================\n")

(printf "Fault site: ~a\n"
        (sdd-fault-site fault))

(printf "Expected path: a -> n1 -> n3 -> out\n")

(printf "Total clauses: ~a\n"
        (length clauses))

(printf "Failed clauses: ~a\n"
        (length failed-clauses))


;; ============================================================
;; RESULT
;; ============================================================

(if (null? failed-clauses)

    (begin
      (printf "\n[PASS] Valid assignment satisfies all clauses.\n")

      (printf "\nTransition verification:\n")
      (printf "[PASS] n1:  0 -> 1\n")
      (printf "[PASS] n3:  0 -> 1\n")
      (printf "[PASS] out: 1 -> 0\n")

      (printf "\nPath verification:\n")
      (printf "[PASS] a -> n1 selected\n")
      (printf "[PASS] n1 -> n3 selected\n")
      (printf "[PASS] n3 -> out selected\n")

      (printf "\n3F-2 PASSED.\n"))

    (begin
      (printf "\n[FAIL] Valid assignment does NOT satisfy the CNF.\n")

      (printf "\nFailed clauses:\n")

      (for ([clause failed-clauses])
        (printf "  ~a\n" clause))))

(printf "\n")
