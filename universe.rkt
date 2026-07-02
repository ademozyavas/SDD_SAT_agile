#lang racket

(require "sat-vars.rkt"
         "circuit.rkt")


;; ------------------------------------------------------------
;; Collect all node names in the circuit
;; ------------------------------------------------------------

(define (circuit-nodes circuit)

  (define input-nodes
    (circuit-inputs circuit))

  (define gate-output-nodes
    (map gate-node-name
         (circuit-gates circuit)))

  (append input-nodes gate-output-nodes))

;; ------------------------------------------------------------
;; Build SAT variable universe
;; ------------------------------------------------------------

(define (build-sat-universe circuit Tmax frames)

  (reset-vars!)

  (define nodes
    (circuit-nodes circuit))

  ;; Reachability variables

  (for* ([n nodes]
         [t (in-range (+ Tmax 1))])

    (reach-var-id n t))

  ;; Logic value variables

  (for* ([n nodes]
         [frame frames])

    (value-var-id n frame))

  (void))



(provide build-sat-universe)