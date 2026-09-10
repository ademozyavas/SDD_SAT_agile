#lang racket

(require "sat-vars.rkt"
         "circuit.rkt")

;;the universe defines the variables, the encoder later
;;genarates constraints over those variables

;; ------------------------------------------------------------
;; Circuit nodes
;; ------------------------------------------------------------

;; Return node names, not node structs.
;;
;; Example:
;;   '(a b c d n1 n2 n3 out)

(define (circuit-nodes circuit)

  (define input-nodes
    (map input-node-name
         (circuit-inputs circuit)))

  (define gate-output-nodes
    (map gate-node-name
         (circuit-gates circuit)))

  (append
   input-nodes
   gate-output-nodes))


;; ------------------------------------------------------------
;; Circuit edges
;; ------------------------------------------------------------

;; Return all actual circuit connections.
;;
;; Example for sample-c17:
;;
;;   ((a n1)
;;    (b n1)
;;    (c n2)
;;    (d n2)
;;    (n1 n3)
;;    (n2 n3)
;;    (n3 out))

(define (circuit-edges circuit)

  (apply append

         (for/list ([g (circuit-gates circuit)])

           (define to
             (gate-node-name g))

           (for/list ([from (gate-node-fanins g)])

             (list from to)))))


;; ------------------------------------------------------------
;; 3F path variables
;; ------------------------------------------------------------

;; PATH_EDGE(from,to)

(define (path-var-id-from-universe from to)
  (allocate-var!
   (sat-var 'path
            (list from to)
            0)))


;; PATH_NODE(node)

(define (path-node-var-id-from-universe node)
  (allocate-var!
   (sat-var 'path-node
            node
            0)))


;; PATH_OUTPUT(node)

(define (path-output-var-id-from-universe node)
  (allocate-var!
   (sat-var 'path-output
            node
            0)))


;; ------------------------------------------------------------
;; Build complete SAT universe
;; ------------------------------------------------------------

(define (build-sat-universe circuit Tmax frames)

  ;; Start variable numbering from 1.
  (reset-vars!)

  (define nodes
    (circuit-nodes circuit))

  (define edges
    (circuit-edges circuit))


  ;; ----------------------------------------------------------
  ;; 1. Reachability variables
  ;; ----------------------------------------------------------

  ;; REACH(node,t)
  ;;
  ;; t = 0 ... Tmax

  (for* ([node nodes]
         [t (in-range (+ Tmax 1))])

    (reach-var-id
     node
     t))


  ;; ----------------------------------------------------------
  ;; 2. Boolean value variables
  ;; ----------------------------------------------------------

  ;; VALUE(node,frame)
  ;;
  ;; frame = 0,1

  (for* ([node nodes]
         [frame frames])

    (value-var-id
     node
     frame))


  ;; ----------------------------------------------------------
  ;; 3. Transition variables
  ;; ----------------------------------------------------------

  ;; TRANSITION(node)

  (for ([node nodes])

    (transition-var-id
     node))


  ;; ----------------------------------------------------------
  ;; 4. Path-edge variables
  ;; ----------------------------------------------------------

  ;; PATH_EDGE(from,to)

  (for ([edge edges])

    (path-var-id-from-universe
     (first edge)
     (second edge)))


  ;; ----------------------------------------------------------
  ;; 5. Path-node variables
  ;; ----------------------------------------------------------

  ;; PATH_NODE(node)
  ;;
  ;; Only gate nodes need to be path nodes.
  ;; Primary inputs are path starts and don't need PATH_NODE.
  
  (for ([gate (circuit-gates circuit)])

    (path-node-var-id-from-universe
     (gate-node-name gate)))


  ;; ----------------------------------------------------------
  ;; 6. Path-output variables
  ;; ----------------------------------------------------------

  ;; PATH_OUTPUT(source)

  (for ([output (circuit-outputs circuit)])

    (path-output-var-id-from-universe
     (output-node-source output)))


  (void))


(provide
 circuit-nodes
 circuit-edges
 build-sat-universe)