#lang racket

(require "circuit.rkt")

;; ------------------------------------------------------------
;; Build hash: node-name -> node-struct
;; ------------------------------------------------------------

(define (build-node-table ckt)
  (define h (make-hash))

  ;; inputs
  (for-each
   (lambda (i)
     (hash-set! h
                (input-node-name i)
                i))
   (circuit-inputs ckt))

  ;; gates
  (for-each
   (lambda (g)
     (hash-set! h
                (gate-node-name g)
                g))
   (circuit-gates ckt))

  h)

;; ------------------------------------------------------------
;; Recursive arrival time computation
;; ------------------------------------------------------------

(define (compute-arrival-times ckt)

  (define table (build-node-table ckt))

  (define memo (make-hash))

  (define (arrival name)

    (cond
      [(hash-has-key? memo name)
       (hash-ref memo name)]

      [else

       (define node (hash-ref table name))

       (define result
         (cond

           ;; input arrival = 0
           [(input-node? node)
            0]

           ;; gate arrival
           [(gate-node? node)
            (+
             (apply
              max
              (map arrival
                   (gate-node-fanins node)))

             (gate-node-delay node))]

           [else
            (error "Unknown node")]))

       (hash-set! memo name result)

       result]))

  ;; return all gate arrival times
  (for/hash ([g (circuit-gates ckt)])
    (values
     (gate-node-name g)
     (arrival (gate-node-name g)))))


(provide
 build-node-table
 compute-arrival-times)