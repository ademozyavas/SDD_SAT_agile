#lang racket

(require "sat-vars.rkt"
         "circuit.rkt"
         "universe.rkt"
         "reachability.rkt"
         "cnf-gates.rkt")

;; ------------------------------------------------------------
;; CNF clause helpers
;; ------------------------------------------------------------

;;(define (implies a b)
;;  (list (- a) b)) ;;not(A) or B

;; ------------------------------------------------------------
;; Generate SAT encoding
;;
;; X_pred_t => X_gate_(t+d)
;; ------------------------------------------------------------
;; do we have any logical constraints on the input value (genrally no)
;; for ATPG, the SAT solver is supposed to choose:
;; a = 0 or 1
;; b = 0 or 1
;; freely, therefore, (v 'value 'a 0) often needs no clauses at all.
;; (generate-sat-instance circuit 5 '(0 1)) where '(0 1) is frames
;; if you tried to pass a single time (0 or 1), you would lose:
;;
;; -> launch/capture distinction
;; -> flexibility for multi-cycle ATPG later
;; -> ability to extend to transition faults cleanly
;; For pure logical functionality (Increment 2 gate semantics),
;; Tmax is not inherently needed.
;; Gate semantics are purely:
;; value(out, t + delay) ↔ f(value(inputs, t))
;; The CNF for a gate needs only needs (1) node identities,
;; (2) fanins, (3) delay, and (4) chosen time frame t
;; Tmax ONLY belongs to Reachability/timing unrolling (increment 1)
;; Gate semantics needs circuit and frame (t), no Tmax.
;; Reachability used Tmax and needs circuit, no frame concept needed
;; (generate-sat-instance circuit Tmax frames) combines both

(define (generate-sat-instance circuit Tmax)

  (build-sat-universe
   circuit
   Tmax
   '(0 1))

  (append

   (generate-reachability-clauses
    circuit
    Tmax)

   (generate-gate-semantics-clauses
    circuit
    '(0 1))))


;;your Increment 1 reachability times and the future ATPG time frames
;;are not necessarily the same thing (time in the following function)
;;for example (reach-var-id 'n1 7) might mean "signal propagatio at
;; depth 7" while (value-var-id 'n1 0) might mean "launch time frame".
;; while (value-var-id 'n1 1) might mean "capture time frame".
;; so 'reach time index and 'value time index represent different concepts.
;;Tmax was used only to prevent:
;; -> generating variables outside the model
;; -> exploding SAT variable space
;; ->inconsistent unrolling
;; There are two importnat questions:
;; (1) Is this gate encoding correct (independent of Tmax)
;; (2) Should I create this SAT variable at all (depends on Tmax)









(provide generate-sat-instance)