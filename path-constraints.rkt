#lang racket

(require "sat-vars.rkt"
         "circuit.rkt"
         "fault-model.rkt"
         "universe.rkt")
;;The current 3F model says:
;;Find a Boolean transition that propagates through a selected,
;;sensitized PI→PO path containing the SDD fault site.
;;It does not yet say that the path is the most timing-critical path.
;; ============================================================
;; PATH VARIABLES
;; ============================================================

;; PATH_EDGE(from,to)
(define (path-var-id from to)
  (allocate-var!
   (sat-var 'path
            (list from to)
            0)))

;; PATH_NODE(node)
(define (path-node-var-id node)
  (allocate-var!
   (sat-var 'path-node
            node
            0)))

;; PATH_OUTPUT(node)
;;
;; True when the selected path terminates at a primary
;; output whose source is node.
(define (path-output-var-id node)
  (allocate-var!
   (sat-var 'path-output
            node
            0)))


;; ============================================================
;; TRANSITION VARIABLES
;; ============================================================

;; transition(node) <-> value(node,0) XOR value(node,1)

(define (transition-clauses circuit)

  (define nodes
    (append
     (map input-node-name
          (circuit-inputs circuit))
     (map gate-node-name
          (circuit-gates circuit))))

  (apply append

         (for/list ([node nodes])

           (define v0
             (value-var-id node 0))

           (define v1
             (value-var-id node 1))

           (define tr
             (transition-var-id node))

           ;; tr <-> (v0 XOR v1)
           ;;
           ;; (v0 OR v1 OR NOT tr)
           ;; (NOT v0 OR NOT v1 OR NOT tr)
           ;; (v0 OR NOT v1 OR tr)
           ;; (NOT v0 OR v1 OR tr)

           (list
            (list v0 v1 (- tr))
            (list (- v0) (- v1) (- tr))
            (list v0 (- v1) tr)
            (list (- v0) v1 tr)))))



;; ============================================================
;; EDGE HELPERS
;; ============================================================

(define (incoming-edges circuit node)

  (filter
   (lambda (edge)
     (eq? (second edge) node))
   (circuit-edges circuit)))


(define (outgoing-edges circuit node)

  (filter
   (lambda (edge)
     (eq? (first edge) node))
   (circuit-edges circuit)))


;; ============================================================
;; EXACTLY-ONE
;; ============================================================

;; Given SAT variables:
;;
;;   x1 x2 ... xn
;;
;; generate:
;;
;;   x1 OR x2 OR ... OR xn
;;
;; and pairwise:
;;
;;   NOT xi OR NOT xj
;;
;; for every i != j.
;;
;; This is appropriate for the small research circuits
;; currently being used.

(define (exactly-one vars)

  (cond

    ;; No possible choice -> impossible.
    [(null? vars)
     (list '())]

    [else

     (append

      ;; At least one.
      (list vars)

      ;; At most one.
      (apply append

             (for/list ([i
                         (in-range
                          (length vars))])

               (apply append

                      (for/list ([j
                                  (in-range
                                   (+ i 1)
                                   (length vars))])

                        (list
                         (list
                          (- (list-ref vars i))
                          (- (list-ref vars j)))))))))]))


;; ============================================================
;; PATH-EDGE -> PATH-NODE
;; ============================================================

;; If an edge is selected, both its endpoints must be
;; selected path nodes.

(define (edge-node-consistency-clauses circuit)

  (apply append

         (for/list ([edge (circuit-edges circuit)])

           (define from
             (first edge))

           (define to
             (second edge))

           (define p
             (apply path-var-id edge))

           (define to-node
             (path-node-var-id to))

           (define clauses
             (list
              ;; PATH_EDGE(from,to) ->
              ;; PATH_NODE(to)
              (list (- p) to-node)))

           ;; If FROM is a gate, its PATH_NODE must also
           ;; be selected.
           (when
               (ormap
                (lambda (g)
                  (eq? (gate-node-name g) from))
                (circuit-gates circuit))

             (set! clauses
                   (append
                    clauses
                    (list
                     ;; PATH_EDGE(from,to) ->
                     ;; PATH_NODE(from)
                     (list (- p)
                           (path-node-var-id from))))))

           clauses)))
;; ============================================================
;; PATH-NODE <-> INCOMING PATH EDGE
;; ============================================================

;; For every gate:
;;
;; PATH_NODE(g)
;;      ->
;; exactly one incoming path edge
;;
;; and every selected incoming edge implies PATH_NODE(g).
;;
;; Therefore:
;;
;; PATH_NODE(g) iff one incoming edge is selected.

(define (path-incoming-clauses circuit)

  (apply append

         (for/list ([g (circuit-gates circuit)])

           (define node
             (gate-node-name g))

           (define pn
             (path-node-var-id node))

           (define incoming-vars

             (map
              (lambda (edge)
                (apply path-var-id edge))
              (incoming-edges circuit node)))

           (append

            ;; PATH_NODE(g) ->
            ;; at least one incoming edge.
            (list
             (append
              (list (- pn))
              incoming-vars))

            ;; PATH_NODE(g) ->
            ;; at most one incoming edge.
            (apply append

                   (for/list ([i
                               (in-range
                                (length incoming-vars))])

                     (apply append

                            (for/list ([j
                                        (in-range
                                         (+ i 1)
                                         (length incoming-vars))])

                              (list
                               (list
                                (- pn)
                                (- (list-ref
                                    incoming-vars i))
                                (- (list-ref
                                    incoming-vars j))))))))))))


;; ============================================================
;; PATH-NODE -> OUTGOING PATH
;; ============================================================

;; A selected internal gate must continue toward exactly one
;; destination.
;;
;; If the gate is also an output source, terminating at the
;; primary output is one possible continuation.

(define (path-outgoing-clauses circuit)

  (define output-sources
    (map output-node-source
         (circuit-outputs circuit)))

  (apply append

         (for/list ([g (circuit-gates circuit)])

           (define node
             (gate-node-name g))

           (define pn
             (path-node-var-id node))

           (define internal-outgoing
             (map
              (lambda (edge)
                (apply path-var-id edge))
              (outgoing-edges circuit node)))

           (define output-var
             (if (member node output-sources)
                 (path-output-var-id node)
                 #f))

           (define choices
             (if output-var
                 (append
                  internal-outgoing
                  (list output-var))
                 internal-outgoing))

           ;; Selected node -> exactly one continuation.
           (append
            (list
             (append
              (list (- pn))
              choices))

            ;; At most one continuation.
            (apply append

                   (for/list ([i
                               (in-range
                                (length choices))])

                     (apply append

                            (for/list ([j
                                        (in-range
                                         (+ i 1)
                                         (length choices))])

                              (list
                               (list
                                (- pn)
                                (- (list-ref choices i))
                                (- (list-ref choices j))))))))))))


;; ============================================================
;; PRIMARY INPUT START
;; ============================================================

;; Exactly one path edge may leave the primary inputs.
;;
;; This gives the path a unique starting point.

(define (path-start-clauses circuit)

  (define starts

    (apply append

           (for/list ([pi (circuit-inputs circuit)])

             (define name
               (input-node-name pi))

             (map
              (lambda (edge)
                (apply path-var-id edge))
              (outgoing-edges circuit name)))))

  (exactly-one starts))


;; ============================================================
;; PRIMARY OUTPUT
;; ============================================================

;; Exactly one output source terminates the selected path.

(define (path-output-selection-clauses circuit)

  (define output-sources
    (map output-node-source
         (circuit-outputs circuit)))

  (define output-vars
    (map path-output-var-id
         output-sources))

  (exactly-one output-vars))


;; ============================================================
;; PATH OUTPUT -> PATH NODE
;; ============================================================

(define (path-output-node-clauses circuit)

  (apply append

         (for/list ([o (circuit-outputs circuit)])

           (define source
             (output-node-source o))

           (define po
             (path-output-var-id source))

           (define pn
             (path-node-var-id source))

           ;; PATH_OUTPUT(source) ->
           ;; PATH_NODE(source)
           (list
            (list (- po) pn)))))


;; ============================================================
;; FAULT SITE
;; ============================================================

;; The fault gate must belong to the selected path.

(define (fault-site-clauses circuit fault)

  (valid-fault-site? circuit fault)

  (define site
    (sdd-fault-site fault))

  (define pn
    (path-node-var-id site))

  ;; PATH_NODE(fault-site)
  (list
   (list pn)))


;; ============================================================
;; TRANSITION PROPAGATION ALONG SELECTED EDGES
;; ============================================================

;; A selected path edge requires a transition at its source.

(define (path-source-transition-clauses circuit)

  (apply append

         (for/list ([edge (circuit-edges circuit)])

           (define p
             (apply path-var-id edge))

           (define from
             (first edge))

           (define tr
             (transition-var-id from))

           ;; PATH_EDGE(from,to) ->
           ;; TRANSITION(from)
           (list
            (list (- p) tr)))))


;; A selected path edge requires a transition at its
;; destination.

(define (path-destination-transition-clauses circuit)

  (apply append

         (for/list ([edge (circuit-edges circuit)])

           (define p
             (apply path-var-id edge))

           (define to
             (second edge))

           (define tr
             (transition-var-id to))

           ;; PATH_EDGE(from,to) ->
           ;; TRANSITION(to)
           (list
            (list (- p) tr)))))


;; ============================================================
;; GATE SENSITIZATION
;; ============================================================

;; For the selected input of a gate:
;;
;; AND:
;;     side inputs = 1 in both frames
;;
;; NAND:
;;     side inputs = 1 in both frames
;;
;; OR:
;;     side inputs = 0 in both frames
;;
;; NOT:
;;     no side-input constraints.

(define (sensitization-clauses circuit)

  (apply append

         (for/list ([g (circuit-gates circuit)])

           (define gate-type
             (gate-node-gate-type g))

           (define output
             (gate-node-name g))

           (define fanins
             (gate-node-fanins g))

           (apply append

                  (for/list ([selected-input fanins])

                    (define p
                      (path-var-id
                       selected-input
                       output))

                    (define side-inputs
                      (filter
                       (lambda (fanin)
                         (not
                          (eq? fanin selected-input)))
                       fanins))

                    (cond

                      ;; AND and NAND.
                      [(or (eq? gate-type 'and)
                           (eq? gate-type 'nand))

                       (apply append

                              (for/list ([side side-inputs])

                                (define v0
                                  (value-var-id side 0))

                                (define v1
                                  (value-var-id side 1))

                                (list

                                 ;; P ->
                                 ;; side(frame 0) = 1
                                 (list (- p) v0)

                                 ;; P ->
                                 ;; side(frame 1) = 1
                                 (list (- p) v1))))]

                      ;; OR.
                      [(eq? gate-type 'or)

                       (apply append

                              (for/list ([side side-inputs])

                                (define v0
                                  (value-var-id side 0))

                                (define v1
                                  (value-var-id side 1))

                                (list

                                 ;; P ->
                                 ;; side(frame 0) = 0
                                 (list (- p) (- v0))

                                 ;; P ->
                                 ;; side(frame 1) = 0
                                 (list (- p) (- v1)))))]

                      ;; NOT.
                      [(eq? gate-type 'not)

                       '()]

                      [else

                       (error
                        "Unsupported gate type for sensitization: ~a"
                        gate-type)]))))))


;; ============================================================
;; OBSERVATION
;; ============================================================

;; The selected primary-output source must transition.

(define (observation-clauses circuit)

  (apply append

         (for/list ([o (circuit-outputs circuit)])

           (define source
             (output-node-source o))

           (define po
             (path-output-var-id source))

           (define tr
             (transition-var-id source))

           ;; PATH_OUTPUT(source) ->
           ;; TRANSITION(source)
           (list
            (list (- po) tr)))))


;; ============================================================
;; COMPLETE 3F MODEL
;; ============================================================

(define (generate-3f-clauses circuit fault)

  (append

   ;; Transition variables.
   (transition-clauses circuit)

   ;; Path structure.
   (edge-node-consistency-clauses circuit)
   (path-incoming-clauses circuit)
   (path-outgoing-clauses circuit)
   (path-start-clauses circuit)

   ;; Primary output.
   (path-output-selection-clauses circuit)
   (path-output-node-clauses circuit)

   ;; Fault.
   (fault-site-clauses circuit fault)

   ;; Transition propagation.
   (path-source-transition-clauses circuit)
   (path-destination-transition-clauses circuit)

   ;; Sensitization.
   (sensitization-clauses circuit)

   ;; Observation.
   (observation-clauses circuit)))


(provide
 path-var-id
 path-node-var-id
 path-output-var-id
 generate-3f-clauses)