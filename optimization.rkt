#lang racket

(require  "circuit.rkt"
         "universe.rkt"
         "path-constraints.rkt")

;; ============================================================
;; 3G PATH-DELAY OPTIMIZATION
;; ============================================================
;;
;; The 3F model guarantees that the selected PATH_EDGE
;; variables form one valid PI -> PO path.
;;
;; For every selected path edge:
;;
;;     PATH(from,to)
;;
;; the contribution to path delay is the delay of the
;; destination gate:
;;
;;     weight = delay(to)
;;
;; Therefore:
;;
;;     total objective
;;       = sum(weight(edge) * PATH_EDGE(edge))
;;
;; Maximizing this objective is equivalent to maximizing
;; path delay.
;;
;; For a fixed capture time:
;;
;;     Slack(P) = Tcapture - Delay(P)
;;
;; so maximizing path delay is equivalent to minimizing
;; path slack.
;; ============================================================


;; ------------------------------------------------------------
;; Find a gate by name
;; ------------------------------------------------------------

(define (find-gate circuit node-name)

  (findf
   (lambda (g)
     (eq? (gate-node-name g)
          node-name))
   (circuit-gates circuit)))


;; ------------------------------------------------------------
;; Weight of a path edge
;; ------------------------------------------------------------
;;
;; An edge:
;;
;;     from -> to
;;
;; contributes the delay of the destination gate.
;;
;; Example:
;;
;;     a -> n1
;;
;; contributes delay(n1).
;;
;; ------------------------------------------------------------

(define (path-edge-weight circuit edge)

  (define from
    (first edge))

  (define to
    (second edge))

  (define gate
    (find-gate circuit to))

  (unless gate
    (error
     "PATH edge ~a -> ~a does not terminate at a gate"
     from
     to))

  (gate-node-delay gate))


;; ------------------------------------------------------------
;; Generate weighted PATH_EDGE soft clauses
;; ------------------------------------------------------------
;;
;; Each result has the form:
;;
;;     (cons weight (list variable))
;;
;; which is the representation expected by write-wcnf.
;;
;; Example:
;;
;;     (cons 3 (list 57))
;;
;; becomes:
;;
;;     3 57 0
;;
;; ------------------------------------------------------------

(define (generate-path-delay-soft-clauses circuit)

  (map
   (lambda (edge)

     (define variable
       (path-var-id
        (first edge)
        (second edge)))

     (define weight
       (path-edge-weight
        circuit
        edge))

     (cons
      weight
      (list variable)))

   (circuit-edges circuit)))


(provide
 find-gate
 path-edge-weight
 generate-path-delay-soft-clauses)
