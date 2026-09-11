#lang racket

(require "circuit.rkt")

;; ============================================================
;; Increment 4B
;; Independent test-vector verification
;; ============================================================
;;
;; This module deliberately does NOT use:
;;
;;   sat-vars.rkt
;;   sat-encoder.rkt
;;   path-constraints.rkt
;;   the SAT model
;;
;; It independently evaluates the circuit for the two input
;; patterns produced by Increment 4A.
;; ============================================================


;; ============================================================
;; Find a gate
;; ============================================================

(define (find-gate circuit name)
  (findf
   (lambda (g)
     (eq? (gate-node-name g) name))
   (circuit-gates circuit)))


;; ============================================================
;; Evaluate one gate
;; ============================================================

(define (evaluate-gate gate values)

  (define type
    (gate-node-gate-type gate))

  (define inputs
    (map
     (lambda (name)
       (hash-ref values name))
     (gate-node-fanins gate)))

  (case type

    [(and)
     (if (andmap
          (lambda (x) (= x 1))
          inputs)
         1
         0)]

    [(or)
     (if (ormap
          (lambda (x) (= x 1))
          inputs)
         1
         0)]

    [(nand)
     (if (andmap
          (lambda (x) (= x 1))
          inputs)
         0
         1)]

    [(not)
     (if (= (first inputs) 1)
         0
         1)]

    [else
     (error
      "Unsupported gate type: ~a"
      type)]))


;; ============================================================
;; Simulate circuit for one frame
;; ============================================================

(define (simulate-circuit circuit input-vector)

  (define values
    (make-hash))

  ;; Primary inputs.
  (for-each
   (lambda (entry)
     (hash-set!
      values
      (car entry)
      (cdr entry)))
   input-vector)

  ;; Gates are topologically sorted.
  (for-each
   (lambda (gate)
     (hash-set!
      values
      (gate-node-name gate)
      (evaluate-gate
       gate
       values)))
   (circuit-gates circuit))

  values)


;; ============================================================
;; Print circuit values
;; ============================================================

(define (print-values label values circuit)

  (printf "~a:\n" label)

  ;; Inputs.
  (for-each
   (lambda (input)
     (define name
       (input-node-name input))

     (printf
      "  ~a = ~a\n"
      name
      (hash-ref values name)))
   (circuit-inputs circuit))

  ;; Gates.
  (for-each
   (lambda (gate)
     (define name
       (gate-node-name gate))

     (printf
      "  ~a = ~a\n"
      name
      (hash-ref values name)))
   (circuit-gates circuit))

  ;; Outputs.
  (for-each
   (lambda (output)
     (define source
       (output-node-source output))

     (printf
      "  ~a = ~a\n"
      (output-node-name output)
      (hash-ref values source)))
   (circuit-outputs circuit)))


;; ============================================================
;; Verify selected transition
;; ============================================================

(define (verify-transition
         values0
         values1
         node)

  (define v0
    (hash-ref values0 node))

  (define v1
    (hash-ref values1 node))

  (printf
   "  ~a: ~a -> ~a"
   node
   v0
   v1)

  (if (= v0 v1)
      (begin
        (printf "  NO TRANSITION\n")
        #f)
      (begin
        (printf "  TRANSITION\n")
        #t)))


;; ============================================================
;; Main verification
;; ============================================================

(define (verify-test-vector circuit
                            frame-0
                            frame-1
                            fault-site
                            path)

  (define values0
    (simulate-circuit
     circuit
     frame-0))

  (define values1
    (simulate-circuit
     circuit
     frame-1))

  (printf "\n========================================\n")
  (printf "INCREMENT 4B - TEST VECTOR VERIFICATION\n")
  (printf "========================================\n")

  (print-values
   "Frame 0"
   values0
   circuit)

  (printf "\n")

  (print-values
   "Frame 1"
   values1
   circuit)

  (printf "\n========================================\n")
  (printf "TRANSITION CHECK\n")
  (printf "========================================\n")

  ;; Fault-site transition.
  (define fault-transition?
    (verify-transition
     values0
     values1
     fault-site))

  ;; Path transitions.
  (define path-transition?
    (andmap
     (lambda (edge)
       (verify-transition
        values0
        values1
        (second edge)))
     path))

  ;; Output transition.
  (define output-name
    (output-node-source
     (first (circuit-outputs circuit))))

  (define output-transition?
    (verify-transition
     values0
     values1
     output-name))

  (printf "\n========================================\n")
  (printf "VERIFICATION RESULT\n")
  (printf "========================================\n")

  (if (and fault-transition?
           path-transition?
           output-transition?)
      (printf "TEST VECTOR: VALID\n")
      (printf "TEST VECTOR: INVALID\n"))

  (printf "========================================\n"))


;; ============================================================
;; Final 3G circuit
;; ============================================================

(define sample-3g-final
  (circuit
   (list
    (input-node 'a)
    (input-node 'b)
    (input-node 'c)
    (input-node 'd))

   (list
    (gate-node 'n1 'and '(a b) 1)
    (gate-node 'n2 'nand '(c d) 4)
    (gate-node 'n3 'or '(n1 n2) 2)
    (gate-node 'out 'not '(n3) 1))

   (list
    (output-node 'OUT 'out))))


;; ============================================================
;; Test vector obtained from Increment 4A
;; ============================================================

(define frame-0
  '((a . 0)
    (b . 0)
    (c . 1)
    (d . 1)))

(define frame-1
  '((a . 0)
    (b . 0)
    (c . 0)
    (d . 1)))


;; ============================================================
;; Selected path from Increment 3G
;; ============================================================

(define selected-path
  '((c n2)
    (n2 n3)
    (n3 out)))


;; ============================================================
;; Run verification
;; ============================================================

(verify-test-vector
 sample-3g-final
 frame-0
 frame-1
 'n2
 selected-path)
