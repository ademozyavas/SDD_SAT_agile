#lang racket

(require "sat-vars.rkt"
         "circuit.rkt")

;;One thing to think about before moving to SDD ATPG: this
;;encoding is conservative. It says "if all fanins are reachable
;;at time t, then the output is reachable at t+d." For realistic
;;delay ATPG you may eventually want additional clauses that
;;define reachability more precisely (especially for reconvergent
;;fanout and sensitized paths). For a first implementation, however,
;;this is a clean and reasonable reachability model.
;; ------------------------------------------------------------
;; Utility
;; ------------------------------------------------------------

;;(define (input-reachability-clauses circuit)
;;
;;  (for/list ([pi (circuit-inputs circuit)])
;;
;;    (list (reach-var-id pi 0))))
(define (input-reachability-clauses circuit)
  (for/list ([pi (circuit-inputs circuit)])
    (list
     (reach-var-id
      (input-node-name pi)
      0))))

(define (gate-reachability-clauses gate Tmax)

  (define output
    (gate-node-name gate))

  (define fanins
    (gate-node-fanins gate))

  (define delay
    (gate-node-delay gate))

  (apply append

         (for/list ([t (in-range (+ Tmax 1))])

           (define target-time
             (+ t delay))

           (if (> target-time Tmax)

               '()

               (list

                (append

                 (map
                  (lambda (fanin)
                    (- (reach-var-id fanin t)))
                  fanins)

                 (list
                  (reach-var-id output
                                target-time))))))))


(define (generate-reachability-clauses circuit Tmax)

  (append

   ;; inputs reachable at depth 0
   (input-reachability-clauses circuit)

   ;; gate propagation clauses
   (apply append

          (for/list ([g (circuit-gates circuit)])

            (gate-reachability-clauses g Tmax)))))


(provide generate-reachability-clauses)