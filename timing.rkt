#lang racket

(require "circuit.rkt")

;; ------------------------------------------------------------
;; Build hash: node-name -> node-struct
;; ------------------------------------------------------------

(define (build-node-table ckt)
  (define h (make-hash))

  ;; Inputs
  (for-each
   (lambda (i)
     (hash-set! h
                (input-node-name i)
                i))
   (circuit-inputs ckt))

  ;; Gates
  (for-each
   (lambda (g)
     (hash-set! h
                (gate-node-name g)
                g))
   (circuit-gates ckt))

  h)


;; ------------------------------------------------------------
;; Compute arrival times
;; ------------------------------------------------------------
;;
;; Primary inputs have arrival time 0.
;;
;; For a gate:
;;
;;   AT(g) = max(AT(fanin)) + delay(g)
;;
;; Returns:
;;
;;   node-name -> arrival-time
;;
;; ------------------------------------------------------------

(define (compute-arrival-times ckt)

  (define table
    (build-node-table ckt))

  (define memo
    (make-hash))

  (define (arrival name)

    (cond
      [(hash-has-key? memo name)
       (hash-ref memo name)]

      [else

       (define node
         (hash-ref table name))

       (define result
         (cond

           ;; Primary input
           [(input-node? node)
            0]

           ;; Gate
           [(gate-node? node)
            (+ (apply
                max
                (map arrival
                     (gate-node-fanins node)))
               (gate-node-delay node))]

           [else
            (error "Unknown node")]))

       (hash-set! memo name result)

       result]))

  ;; ----------------------------------------------------------
  ;; Include BOTH primary inputs and gates.
  ;; ----------------------------------------------------------

  (define result
    (make-hash))

  ;; Primary inputs
  (for-each
   (lambda (i)
     (hash-set!
      result
      (input-node-name i)
      (arrival (input-node-name i))))
   (circuit-inputs ckt))

  ;; Gates
  (for-each
   (lambda (g)
     (hash-set!
      result
      (gate-node-name g)
      (arrival (gate-node-name g))))
   (circuit-gates ckt))

  result)
;; ------------------------------------------------------------
;; Compute required times
;; ------------------------------------------------------------
;;
;; capture-time:
;;   Latest acceptable arrival time at the primary outputs.
;;
;; For a primary output:
;;
;;   RT(output) = capture-time
;;
;; For a gate:
;;
;;   RT(fanin) = RT(gate) - delay(gate)
;;
;; If a node feeds multiple gates, the minimum required time
;; is selected.
;;
;; ------------------------------------------------------------

;; ------------------------------------------------------------
;; Compute required times
;; ------------------------------------------------------------
;;
;; capture-time:
;;   Latest acceptable arrival time at the primary outputs.
;;
;; For a primary output:
;;
;;   RT(output) = capture-time
;;
;; For a gate:
;;
;;   RT(fanin) = RT(gate) - delay(gate)
;;
;; If a node feeds multiple gates, the minimum required time
;; is selected.
;;
;; ------------------------------------------------------------

(define (compute-required-times ckt capture-time)

  (define table
    (build-node-table ckt))

  (define required
    (make-hash))

  ;; ----------------------------------------------------------
  ;; Initialize primary output source nodes.
  ;; ----------------------------------------------------------

  (for-each
   (lambda (o)

     (define source
       (output-node-source o))

     (hash-set!
      required
      source
      capture-time))

   (circuit-outputs ckt))

  ;; ----------------------------------------------------------
  ;; Propagate required times backward.
  ;; ----------------------------------------------------------

  (for-each
   (lambda (gate)

     (define gate-name
       (gate-node-name gate))

     ;; A gate should already have a required time from
     ;; a downstream gate. If it does not, something is
     ;; structurally wrong with the circuit.
     (unless (hash-has-key? required gate-name)
       (error
        "No required time available for gate ~a"
        gate-name))

     (define gate-required
       (hash-ref required gate-name))

     (define fanin-required
       (- gate-required
          (gate-node-delay gate)))

     ;; Propagate the requirement to every fanin.
     (for-each
      (lambda (fanin)

        (if (hash-has-key? required fanin)

            ;; Multiple fanout gates:
            ;; choose the most restrictive requirement.
            (hash-set!
             required
             fanin
             (min
              (hash-ref required fanin)
              fanin-required))

            ;; First requirement encountered.
            (hash-set!
             required
             fanin
             fanin-required)))

      (gate-node-fanins gate)))

   ;; Gates must be processed from outputs toward inputs.
   (reverse (circuit-gates ckt)))

  required)

(define (compute-timing-table ckt capture-time)

  (define arrival-times
    (compute-arrival-times ckt))

  (define required-times
    (compute-required-times ckt capture-time))

  (define timing
    (make-hash))

  ;; Add all nodes appearing in arrival-times.
  (for-each
   (lambda (name)
     (hash-set!
      timing
      name
      (hash
       'arrival
       (hash-ref arrival-times name)

       'required
       (hash-ref required-times name))))
   (hash-keys arrival-times))

  timing)
(provide
 build-node-table
 compute-arrival-times
 compute-required-times
 compute-timing-table)