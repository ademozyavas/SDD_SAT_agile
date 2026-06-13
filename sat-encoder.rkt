#lang racket

(require "sat-vars.rkt"
         "circuit.rkt")

;; ------------------------------------------------------------
;; Helper:
;; create SAT variable for (node,time)
;; ------------------------------------------------------------

(define (v node time)
  (allocate-var!  ;;returns an ID (new or already existing)
                  ;; for (node,time) pair
   (sat-var node time)))

;; ------------------------------------------------------------
;; CNF clause helpers
;; ------------------------------------------------------------

(define (implies a b)
  (list (- a) b)) ;;not(A) or B

;; ------------------------------------------------------------
;; Generate SAT encoding
;;
;; X_pred_t => X_gate_(t+d)
;; ------------------------------------------------------------

(define (generate-sat-instance ckt Tmax)

  (reset-vars!) ;;counter to 0, empty both hashes (var->id, id->var)

  (define clauses '())

  ;; --------------------------------------------------------
  ;; Input signals exist at time 0
  ;; --------------------------------------------------------
  ;;(for-each function list1 list2 ...) return <void>
  ;; SAT variables are forced to be TRUE at time 0.
  ;; suppose ID is 1 for a, then there is a unit clause for a
  ;; at time 0 (always true) which is "1 0" which means
  ;; variable a is true.
  ;; Suppose n1 = AND(a,b).then you may have X_a_0 = var 1
  ;; X_b_0 = var 2, X_n1_1 = var 3. Input clauses
  ;; "1 0" and "2 0". Propagation clauses: -1 
  (for-each
   (lambda (i)

     (define var  ;; var is bound to a number for SAT
       (v (input-node-name i) 0))

     ;; unit clause
     (set! clauses
           (cons (list var)
                 clauses)))

   (circuit-inputs ckt)) ;;inputs (input-node 'a),b,c,and d 

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

  clauses) ;;return a list of CNF clauses

(provide generate-sat-instance)