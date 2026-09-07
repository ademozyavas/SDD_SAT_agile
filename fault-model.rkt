#lang racket

(require "circuit.rkt")
;;3E does not yet answer whether the fault is detectable.
;;It only establishes:
;;"The SDD is located at this valid gate."

;; ------------------------------------------------------------
;; Small Delay Defect
;;
;; The fault is identified by its site.
;;
;; The additional delay δ is intentionally not fixed here.
;; During ATPG we want to find a test vector that sensitizes
;; a minimum-slack path through this site.
;; ------------------------------------------------------------

;;An SDD fault is currently represented simply by the gate at
;;which the small delay defect occurs.Why only site?
;;Because we deliberately decided not to assign a fixed delay
;;defect δ at this stage. Because our ATPG objective is
;;"Can I generate a test that sensitizes a path through this
;;fault site with the smallest possible slack?"
(struct sdd-fault
        (site)
        #:transparent)


;; ------------------------------------------------------------
;; Validate that the fault site is a gate in the circuit.
;; ------------------------------------------------------------
;;This defines a function that checks whether the specified
;;fault site actually exists as a gate in the circuit.
;;Why require a fault site to be a gate?
;;The SDD is modeled as an additional delay assoicated with
;;a gate (not a wire).That's a possible future extension if
;;we want defects on interconnects rather than gate delays,
;;but it isn't part of our current 3E model.
(define (valid-fault-site? ckt fault)
  (define site
    (sdd-fault-site fault))
  ;;searh the gates in the ckt (circuit)
  ;;(ormap even? '(1 3 5 6)) returns true
  (if (ormap
       (lambda (g)
         (eq? (gate-node-name g) site))
       (circuit-gates ckt)) ;;returns list of gates in circuit
      #t
      (error
       "Fault site ~a is not a gate in the circuit"
       site)))


;; ------------------------------------------------------------
;; Return the nominal delay of the fault site.
;; ------------------------------------------------------------
;;This function returns the nominal delay of the gate containing
;;the fault.
;;Why do we need the nominal delay? The nominal delay is useful
;;later when constructing the timing/path model. 
(define (fault-site-delay ckt fault)
  (valid-fault-site? ckt fault)

  (define site
    (sdd-fault-site fault))

  (define gate
    (findf
     (lambda (g)
       (eq? (gate-node-name g) site))
     (circuit-gates ckt)))

  (gate-node-delay gate))


(provide
 (struct-out sdd-fault)
 valid-fault-site?
 fault-site-delay)
