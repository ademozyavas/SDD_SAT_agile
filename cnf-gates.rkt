#lang racket

(require "sat-vars.rkt"
         "circuit.rkt")

(define (generate-gate-semantics-clauses circuit frames)

  (apply append

         (for/list ([frame frames])

           (apply append

                  (for/list ([g (circuit-gates circuit)])

                    (case (gate-node-gate-type g)

                      [(and)
                       (encode-and
                        (gate-node-fanins g)
                        (gate-node-name g)
                        frame)]

                      [(or)
                       (encode-or
                        (gate-node-fanins g)
                        (gate-node-name g)
                        frame)]

                      [(not)
                       (encode-not
                        (gate-node-fanins g)
                        (gate-node-name g)
                        frame)]

                      [(nand)
                       (encode-nand
                        (gate-node-fanins g)
                        (gate-node-name g)
                        frame)]

                      [else
                       (error "unknown gate")]))))))

;;generates the cnf clauses for n-input AND
;; same frame on inputs and outputs
;;fanins: a list of the input nodes of the gate
;; z: the output node of the gate
;; frame: either 0 (launch) or 1 (capture)
(define (encode-and fanins z frame)

  (let* ([A (map (lambda (n)
                   (value-var-id n frame))
                 fanins)]

         [Z (value-var-id z frame)])
    ;;circuit node names converted into SAT variables IDs
    ;;suppose the SAT universe contains value(a,0) -> 7 and
    ;;value(b,0) -> 8 and value(z,0)->9. Then A becomes '(7 8)
    ;; Z becomes 9. After this point, the function always uses
    ;; numbers (no more node names)

   
    (append

     ;; z -> all inputs
      ;;z -> (a and b) is equivalent to (z->a) and (z->b)
     (map (lambda (a)
            (list (- Z) a))
          A)

     ;; all inputs -> z

     (list
      (cons Z  ;; (9 -7 -8)
            (map - A))))));;negative all inputs (-7 -8)


;;change this function
(define (encode-or fanins z frame)

    (let* ([A (map (lambda (n)
                   (value-var-id n frame))
                 fanins)]

         [Z (value-var-id z frame)])

    (append

     ;; not z or all inputs
     ;; z -> (a or b or ...)
     (list (cons (- Z) A))

     ;; each input -> z
     (map (lambda (a)
            (list (- a) Z))
          A)
     )))

;;Since a NOT gate has exactly one fanin and
;; z <-> not a (a is input to NOT and z is output)
;; which implies z -> not a and not a -> z
;; which is (not z or not a) and (a or z)
(define (encode-not fanins z frame)

  (define A
    (value-var-id (first fanins) frame))

  (define Z
    (value-var-id z frame))

  (list
   (list (- Z) (- A))
   (list A Z)))

(define (encode-nand fanins z frame)

  (let* ([A (map (λ (n)
                   (value-var-id n frame))
                 fanins)]
         [Z (value-var-id z frame)])

    (append

     ;; z -> NOT(all inputs)
     (list
      (cons (- Z)
            (map - A)))

     ;; NOT(all inputs) -> z
     (map (λ (a)
            (list a Z))
          A))))

(provide generate-gate-semantics-clauses)