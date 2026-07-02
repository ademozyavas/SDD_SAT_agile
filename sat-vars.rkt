#lang racket


;;represents a single Boolean SAT proposition related to timing
;;it encodes: "signal can be present at "node" at "time".
;;for example: (sat-var 'n3 2) means signal reaches at node n3 at time 2
;;this is NOT the SAT solver variable itself yet.
;;each (node,time) pair becomes one Boolean variable.
;;(sat-var 'n3 2) is the logical proposition X_n3_2 which can be TRUE
;;reachable at time 2, FALSE not reachable at time 2.
;; (sat-var 'n3 2) may be mapped to a ID (integer) for SAT 
;(struct sat-var (node time)
;  #:transparent)
;;(struct sat-var (kind node time) #:transparent)
(struct sat-var (kind node time) #:transparent)
;;the field named time will simply mean:
;; a. reach variables -> propagation time/depth
;; b. value variables -> frame number

;;(define PHASE-REACH 'reach-phase) ;;depth(0..Tmax)
;;(define PHASE-VALUE 'value-phase) ;;frame(0/1/launch/capture)

(define var->id (make-hash))
(define id->var (make-hash))
(define next-id 1)

;;assign a unique SAT integer ID to a (kind,node,time) triple
;;return value ID is used in CNF clauses
(define (allocate-var! v)
  (cond
    [(hash-has-key? var->id v) ;;if SAT var has already an ID, return it
     (hash-ref var->id v)] ;;structs are given structural equality support
                           ;;two structs created separatly with same contents
    ;;#transparent structs. same struct type, same field values  => equal
    ;; if #transparent was missing, two separately created structs would
    ;; NOT necessarily match.
    [else
     (set! next-id (+ next-id 1));;otherwise create a new ID

     (hash-set! var->id v next-id) ;;store (sat-var kind node time)->int ID
     (hash-set! id->var next-id v) ;;store (int ID)->(sat-var kind node time)

     next-id]))

(define (v kind node time)
  (allocate-var!
   (sat-var kind node time)))

;; t ranges over 0...Tmax
(define (reach-var-id node t)
  (v 'reach node t))

;; t ranges over {0,1}, that is, frames
(define (value-var-id node t)
  (v 'value node t)) 

;;start new fresh SAT encoding with no earlier variables/mappings
;;set counter back to 0 and clear the hashes
(define (reset-vars!)
  (set! next-id 0) ;; 0 or 1
  (set! var->id (make-hash))
  (set! id->var (make-hash)))

;;reach(a,0) means "a signal can arrive at a at time 0"
;;value(a,0) means "the logic value of a at time 0 is 1"
;; reach(node, t)  -> t: arrival depth which grows along the circuit
;; “Can a signal arrive at this node after t propagation steps?”
;; reach(node, t)  -> typically t is either 0(launch) or 1 (capture)
;; “What is the logic value of this node in time frame t?”


(define (lookup-var id)  ;;returns the value associated with that key
  (hash-ref id->var id)) ;; if key is missing, it will throw an error

(define (lookup-id v)
  (hash-ref var->id v))

;;(define (dump-sat-vars)
;;  (for ([id (sort (hash-keys id->var) <)])
    ;;(define v (hash-ref id->var id))
    ;;(printf "~a -> ~a\n" id v)))
    ;;(displayln id)))
;;    (display id)
;;    (display " -> ")
;;    (displayln (hash-ref id->var id))))


(define (dump-sat-vars)
  (printf "\n========================================\n")
  (printf "SAT VARIABLE TABLE\n")
  (printf "========================================\n")
  (printf "Total SAT variables: ~a\n\n"
          (hash-count id->var))

  (for ([id (sort (hash-keys id->var) <)])
    (match (hash-ref id->var id)
      [(sat-var 'reach node time)
       (printf "~a : (~a, ~a, ~a)\n"
               id
               'reach
               node
               time)]
      [other
       (printf "~a : ~a\n"
        (~a id #:width 4 #:align 'right)
        other)]))

  (printf "========================================\n\n"))

(provide
 struct-out sat-var
 allocate-var!
 lookup-var
 lookup-id
 reset-vars!
 value-var-id
 reach-var-id
 dump-sat-vars)
