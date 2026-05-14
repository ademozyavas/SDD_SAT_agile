#lang racket

(require "sat-vars.rkt"
         "circuit.rkt")

;; ------------------------------------------------------------
;; Helper:
;; create SAT variable for (node,time)
;; ------------------------------------------------------------

(define (v node time)
  (allocate-var!
   (sat-var node time)))

;; ------------------------------------------------------------
;; CNF clause helpers
;; ------------------------------------------------------------

(define (implies a b)
  (list (- a) b))

;; ------------------------------------------------------------
;; Generate SAT encoding
;;
;; X_pred_t => X_gate_(t+d)
;; ------------------------------------------------------------

(define (generate-sat-instance ckt Tmax)

  (reset-vars!)

  (define clauses '())

  ;; --------------------------------------------------------
  ;; Input signals exist at time 0
  ;; --------------------------------------------------------

  (for-each
   (lambda (i)

     (define var
       (v (input-node-name i) 0))

     ;; unit clause
     (set! clauses
           (cons (list var)
                 clauses)))

   (circuit-inputs ckt))

  ;; --------------------------------------------------------
  ;; Propagation constraints
  ;; --------------------------------------------------------

  (for-each
   (lambda (g)

     (define gname (gate-node-name g))
     (define delay (gate-node-delay g))

     (for-each
      (lambda (fanin)

        ;; time-unrolling
        (for ([t (in-range 0 (+ 1 Tmax))])

          (define next-time (+ t delay))

          (when (<= next-time Tmax)

            (define a (v fanin t))
            (define b (v gname next-time))

            ;; X_fanin_t => X_gate_(t+d)
            (set! clauses
                  (cons
                   (implies a b)
                   clauses)))))

      (gate-node-fanins g)))

   (circuit-gates ckt))

  clauses)

(provide generate-sat-instance)