#lang racket

(require "circuit.rkt")

;; ------------------------------------------------------------
;; Build hash: node-name -> node-struct
;; ------------------------------------------------------------
;; hash table from "node-names to node structs"
;; (1)input node, (2) gate node, and (3) output node.
;; So, we can get full node definition later (hash-ref table 'n3)
(define (build-node-table ckt)
  (define h (make-hash)) ;;new mutable empty hash

  ;; inputs. (for-each used for side effects)
  ;;(for-each function list1 list2 ...) return <void>
  (for-each
   (lambda (i)
     (hash-set! h
                (input-node-name i)
                i))
   (circuit-inputs ckt));;circuit-inputs gives you a list of input-node
                        ;; structs

  ;; gates (do the same for gate nodes), for each gate-node name
  (for-each    ;; add a key.value (name.get-node-struct) pair
   (lambda (g)
     (hash-set! h
                (gate-node-name g)
                g))
   (circuit-gates ckt))

  h) ;;return value is h (hash from node-name to node struct)
     ;;no pair in the hash for output?

;; ------------------------------------------------------------
;; Recursive arrival time computation
;; ------------------------------------------------------------
;; ckt is the (circuit) value created from the circuit struct.
(define (compute-arrival-times ckt)
  ;;hash from node-name to node struct
  (define table (build-node-table ckt))
  ;;empty memo hash (node, arrival-time) pairs
  (define memo (make-hash)) ;;new mutable empty hash

  (define (arrival name)

    (cond ;;if key exists, return its (name's) value
      [(hash-has-key? memo name)
       (hash-ref memo name)] 

      [else

       (define node (hash-ref table name));;"node" is struct node

       (define result  ;;if and cond are expressions that return a value
         (cond

           ;; input arrival = 0, input nodes has arrival time 0
           [(input-node? node)
            0]

           ;; gate arrival. Add (+) the max(latest) arrival time of the
           ;; fanins and the nominal delay of the gate
           [(gate-node? node)
            (+
             (apply
              max ;;map retuns a list of arrival times but max expects
              ;;separate values. (apply max '(1 2 3)) = (map 1 2 3)
              (map arrival  ;; fanins is a list of inputs to gate
                   (gate-node-fanins node)))

             (gate-node-delay node))]

           [else
            (error "Unknown node")]))

       (hash-set! memo name result)

       result]))

  ;; return all gate arrival times. for/hash is a comprehensive loop
  ;; that builds and returns an immutable hash from the elements of a list
  ;; each iteration should return a key-value pair collected in the hash
  ;; key=gate name, value=arrival time
  ;; for/hash requires exactly two separate values at the end of each
  ;; iteration. The first value is used as the key, and the second
  ;; value is used as the value. Immediately inserts them into the table.
  (for/hash ([g (circuit-gates ckt)])
    (values    ;;returns multiple distinct values from a function/expression
     (gate-node-name g)
     (arrival (gate-node-name g)))))


(provide
 build-node-table
 compute-arrival-times)