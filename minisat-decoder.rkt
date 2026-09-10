#lang racket

(require "circuit.rkt"
         "fault-model.rkt"
         "sat-encoder.rkt"
         "sat-vars.rkt")

;; ------------------------------------------------------------
;; 3F-4
;;
;; Read a MiniSat model and reconstruct/verify the
;; solver-discovered SDD path.
;;
;; Expected input:
;;     3f.out
;;
;; produced by:
;;     minisat 3f.cnf 3f.out
;; ------------------------------------------------------------


;; ------------------------------------------------------------
;; Configuration
;; ------------------------------------------------------------

(define circuit
  sample-c17)

(define fault
  (sdd-fault 'n1))

(define Tmax
  3)

(define model-file
  "3f.out")


;; ------------------------------------------------------------
;; Rebuild the SAT universe
;;
;; This is important because sat-vars.rkt uses deterministic
;; allocation of SAT variable IDs.
;; ------------------------------------------------------------

(define clauses
  (generate-sat-instance
   circuit
   Tmax
   fault))

;; Number of SAT variables
(define num-vars
  (sat-var-count))


;; ------------------------------------------------------------
;; Read MiniSat output
;; ------------------------------------------------------------

(define (read-minisat-model filename)

  (define lines
    (file->lines filename))

  (when (null? lines)
    (error "MiniSat output file is empty"))

  (define status
    (string-trim
     (first lines)))

  (cond

    [(string=? status "SAT")
     (define tokens
       (apply append
              (map string-split
                   (rest lines))))

     (define literals
       (for/list ([token tokens]
                  #:when (not (string=? token "0")))
         (string->number token)))

     (define model
       (make-hash))

     (for ([lit literals]
           #:when lit)

       (define id
         (abs lit))

       (hash-set!
        model
        id
        (> lit 0)))

     model]

    [(string=? status "UNSAT")
     (error
      "MiniSat reports UNSAT")]

    [else
     (error
      "Unknown MiniSat result: ~a"
      status)]))


;; ------------------------------------------------------------
;; Decode one SAT variable
;; ------------------------------------------------------------

(define (model-true? model id)
  (hash-ref
   model
   id
   #f))


;; ------------------------------------------------------------
;; Collect true PATH variables
;; ------------------------------------------------------------

(define (selected-path-edges model)

  (for/list ([id (in-range 1 (+ num-vars 1))]
             #:when
             (and
              (model-true? model id)
              (let ([sv (lookup-var id)])
                (eq? (sat-var-kind sv)
                     'path))))

    (define sv
      (lookup-var id))

    (sat-var-node sv)))


;; ------------------------------------------------------------
;; Collect true PATH_NODE variables
;; ------------------------------------------------------------

(define (selected-path-nodes model)

  (for/list ([id (in-range 1 (+ num-vars 1))]
             #:when
             (and
              (model-true? model id)
              (let ([sv (lookup-var id)])
                (eq? (sat-var-kind sv)
                     'path-node))))

    (sat-var-node
     (lookup-var id))))


;; ------------------------------------------------------------
;; Collect true PATH_OUTPUT variables
;; ------------------------------------------------------------

(define (selected-path-outputs model)

  (for/list ([id (in-range 1 (+ num-vars 1))]
             #:when
             (and
              (model-true? model id)
              (let ([sv (lookup-var id)])
                (eq? (sat-var-kind sv)
                     'path-output))))

    (sat-var-node
     (lookup-var id))))


;; ------------------------------------------------------------
;; Collect true TRANSITION variables
;; ------------------------------------------------------------

(define (selected-transitions model)

  (for/list ([id (in-range 1 (+ num-vars 1))]
             #:when
             (and
              (model-true? model id)
              (let ([sv (lookup-var id)])
                (eq? (sat-var-kind sv)
                     'transition))))

    (sat-var-node
     (lookup-var id))))


;; ------------------------------------------------------------
;; Reconstruct ordered path
;; ------------------------------------------------------------

(define (reconstruct-path edges circuit)

  (define input-names
    (map input-node-name
         (circuit-inputs circuit)))

  (define output-sources
    (map output-node-source
         (circuit-outputs circuit)))

  ;; Find the selected edge beginning at a primary input.
  (define start-edge
    (findf
     (lambda (edge)
       (member (first edge)
               input-names))
     edges))

  (unless start-edge
    (error
     "No selected path starts at a primary input"))

  ;; Recursively follow selected edges.
  (define (follow current path)

    (if (member current output-sources)

        ;; We have reached the output.
        path

        ;; Otherwise find the next selected edge.
        (let ([next-edge
               (findf
                (lambda (edge)
                  (eq? (first edge)
                       current))
                edges)])

          (unless next-edge
            (error
             "Selected path terminates at ~a before reaching output"
             current))

          (follow
           (second next-edge)
           (append path
                   (list (second next-edge)))))))

  ;; Start with the primary-input node.
  (follow
   (first start-edge)
   (list (first start-edge))))
;; ------------------------------------------------------------
;; Print a path
;; ------------------------------------------------------------

(define (print-path path)

  (printf "\nDiscovered path:\n\n")

  (printf "  ")

  (for ([node path]
        [i (in-naturals)])

    (when (> i 0)
      (printf " -> "))

    (printf "~a" node))

  (printf "\n"))


;; ------------------------------------------------------------
;; Verify path structure
;; ------------------------------------------------------------

(define (verify-path-structure
         path
         selected-edges
         selected-path-nodes
         selected-path-outputs)

  (define inputs
    (map input-node-name
         (circuit-inputs circuit)))

  (define output-sources
    (map output-node-source
         (circuit-outputs circuit)))

  (define start
    (first path))

  (define finish
    (last path))

  ;; Check PI
  (unless (member start inputs)
    (error
     "Path does not start at a primary input: ~a"
     start))

  ;; Check PO
  (unless (member finish output-sources)
    (error
     "Path does not end at a primary output source: ~a"
     finish))

  ;; Check every path edge exists
  (for ([i (in-range (- (length path) 1))])

    (define from
      (list-ref path i))

    (define to
      (list-ref path (+ i 1)))

    (unless (member (list from to)
                    selected-edges)

      (error
       "Path edge (~a,~a) was not selected"
       from
       to)))

  ;; Check path nodes
  (for ([node (rest path)])

    (unless
        (member node selected-path-nodes)

      (error
       "Path node ~a is missing"
       node)))

  ;; Check output selection
  (unless
      (member finish selected-path-outputs)

    (error
     "Output ~a was not selected"
     finish))

  #t)


;; ------------------------------------------------------------
;; Verify fault site
;; ------------------------------------------------------------

(define (verify-fault-site path)

  (define site
    (sdd-fault-site fault))

  (unless
      (member site path)

    (error
     "Fault site ~a is NOT on the selected path"
     site))

  (printf
   "[PASS] Fault site ~a is on the selected path.\n"
   site))


;; ------------------------------------------------------------
;; Verify transitions
;; ------------------------------------------------------------

(define (verify-transitions
         path
         transitions)

  (for ([node path])

    (unless
        (member node transitions)

      (error
       "Path node ~a has no transition"
       node)))

  (printf
   "[PASS] All nodes on the selected path transition.\n"))


;; ------------------------------------------------------------
;; Find gate by name
;; ------------------------------------------------------------

(define (find-gate circuit name)

  (findf
   (lambda (g)
     (eq? (gate-node-name g)
          name))
   (circuit-gates circuit)))


;; ------------------------------------------------------------
;; Get Boolean value from model
;; ------------------------------------------------------------

(define (node-value model node frame)

  (define wanted-id
    #f)

  (for ([id (in-range 1 (+ num-vars 1))])

    (define sv
      (lookup-var id))

    (when
        (and
         (eq? (sat-var-kind sv)
              'value)
         (eq? (sat-var-node sv)
              node)
         (= (sat-var-time sv)
            frame))

      (set! wanted-id id)))

  (unless wanted-id
    (error
     "No VALUE variable found for ~a frame ~a"
     node
     frame))

  (model-true? model wanted-id))


;; ------------------------------------------------------------
;; Verify sensitization of selected gate input
;; ------------------------------------------------------------

(define (verify-gate-sensitization
         model
         from
         to)

  (define gate
    (find-gate circuit to))

  (if (not gate)

      ;; Destination is not a gate.
      #t

      (let* ([gate-type
              (gate-node-gate-type gate)]
             [fanins
              (gate-node-fanins gate)]
             [side-inputs
              (filter
               (lambda (fanin)
                 (not (eq? fanin from)))
               fanins)])

        (for ([side side-inputs])

          (define v0
            (node-value model side 0))

          (define v1
            (node-value model side 1))

          (case gate-type

            [(and nand)
             (unless
                 (and v0 v1)

               (error
                "Sensitization failure at ~a: side input ~a is not 1 in both frames"
                to
                side))]

            [(or)
             (unless
                 (and (not v0)
                      (not v1))

               (error
                "Sensitization failure at ~a: side input ~a is not 0 in both frames"
                to
                side))]

            [(not)
             (void)]

            [else
             (error
              "Unsupported gate type: ~a"
              gate-type)]))

        #t)))

;; ------------------------------------------------------------
;; Verify sensitization along complete path
;; ------------------------------------------------------------

(define (verify-sensitization
         model
         path)

  (for ([i (in-range (- (length path) 1))])

    (define from
      (list-ref path i))

    (define to
      (list-ref path (+ i 1)))

    (verify-gate-sensitization
     model
     from
     to))

  (printf
   "[PASS] Selected path satisfies gate sensitization conditions.\n"))


;; ------------------------------------------------------------
;; Verify observation at output
;; ------------------------------------------------------------

(define (verify-observation
         path
         transitions
         selected-path-outputs)

  (define output
    (last path))

  (unless
      (member output selected-path-outputs)

    (error
     "Selected path does not reach an output"))

  (unless
      (member output transitions)

    (error
     "Output source ~a does not transition"
     output))

  (printf
   "[PASS] Transition is observed at output ~a.\n"
   output))


;; ------------------------------------------------------------
;; Print transitions
;; ------------------------------------------------------------

(define (print-transitions transitions)

  (printf "\nTransitions:\n\n")

  (for ([node transitions])
    (printf "  ~a\n" node)))


;; ------------------------------------------------------------
;; Main test
;; ------------------------------------------------------------

(define model
  (read-minisat-model
   model-file))

(define selected-edges
  (selected-path-edges model))

(define selected-nodes
  (selected-path-nodes model))

(define selected-outputs
  (selected-path-outputs model))

(define transitions
  (selected-transitions model))

(define path
  (reconstruct-path
   selected-edges
   circuit))


;; ------------------------------------------------------------
;; Report
;; ------------------------------------------------------------

(printf "\n")
(printf "========================================\n")
(printf "3F-4 SAT MODEL DECODING\n")
(printf "========================================\n")

(printf "Fault site: ~a\n"
        (sdd-fault-site fault))

(printf "SAT variables: ~a\n"
        num-vars)

(printf "Clauses: ~a\n"
        (length clauses))

(printf "\nSelected PATH edges:\n")

(for ([edge selected-edges])
  (printf "  ~a -> ~a\n"
          (first edge)
          (second edge)))

(printf "\nSelected PATH nodes:\n")

(for ([node selected-nodes])
  (printf "  ~a\n"
          node))

(printf "\nSelected PATH_OUTPUT variables:\n")

(for ([node selected-outputs])
  (printf "  ~a\n"
          node))

(print-path path)

(print-transitions transitions)


;; ------------------------------------------------------------
;; Verification
;; ------------------------------------------------------------

(verify-path-structure
 path
 selected-edges
 selected-nodes
 selected-outputs)

(verify-fault-site
 path)

(verify-transitions
 path
 transitions)

(verify-sensitization
 model
 path)

(verify-observation
 path
 transitions
 selected-outputs)


(printf "\n")
(printf "========================================\n")
(printf "3F-4 PASSED\n")
(printf "========================================\n")
