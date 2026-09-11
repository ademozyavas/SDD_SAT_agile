#lang racket

(require "circuit.rkt"
         "fault-model.rkt")

;; ============================================================
;; Read SAT variable mapping
;; ============================================================
;;
;; Each line of 3g.vars is a Racket datum:
;;
;;   (57 path (a n1) 0)
;;   (61 path (n1 n3) 0)
;;   (63 path (n3 out) 0)
;;
;; The resulting hash maps:
;;
;;   SAT ID -> variable description
;; ============================================================

(define (read-var-map filename)
  (call-with-input-file
   filename
   (lambda (in)
     (define table
       (make-hash))

     (let loop ()
       (define entry
         (read in))

       (unless (eof-object? entry)
         (hash-set!
          table
          (first entry)
          entry)
         (loop)))

     table)))

;; ============================================================
;; Read RC2 model
;; ============================================================
;;
;; The model is a sequence of signed SAT literals ending in 0.
;;
;; Example:
;;
;;   1 -2 3 ... 57 ... 61 ... 0
;;
;; Only positive literals represent TRUE variables.
;; ============================================================

(define (read-model filename)
  (call-with-input-file
   filename
   (lambda (in)
     (define model
       '())

     (let loop ()
       (define x
         (read in))

       (cond
         [(eof-object? x)
          (reverse model)]

         [(= x 0)
          (reverse model)]

         [else
          (set! model
                (cons x model))
          (loop)])))))

;; ============================================================
;; Decode selected PATH edges
;; ============================================================
;;
;; IMPORTANT:
;;
;; This function does NOT call path-var-id.
;;
;; Instead, it uses the exact variable mapping saved in
;; 3g.vars.
;; ============================================================

(define (decode-selected-edges model var-map)
  (for/list ([id model]
             #:when (> id 0)
             #:do
             [(define entry
                (hash-ref var-map id #f))]
             #:when entry
             #:when (eq? (second entry) 'path))
    (third entry)))

;; ============================================================
;; Order path edges
;; ============================================================

(define (order-path edges)
  (if (null? edges)
      '()
      (let* ([starts
              (filter
               (lambda (edge)
                 (not
                  (ormap
                   (lambda (other)
                     (eq? (second other)
                          (first edge)))
                   edges)))
               edges)]

             [start
              (if (= (length starts) 1)
                  (first starts)
                  (error
                   "Could not identify unique path start"))])

        (define (follow current remaining)
          (if (null? remaining)
              (list current)

              (let ([next
                     (findf
                      (lambda (edge)
                        (eq? (first edge)
                             (second current)))
                      remaining)])

                (if next
                    (cons
                     current
                     (follow
                      next
                      (remove next remaining)))
                    (list current)))))

        (follow start
                (remove start edges)))))

;; ============================================================
;; Find gate
;; ============================================================

(define (find-gate circuit name)
  (findf
   (lambda (g)
     (eq? (gate-node-name g)
          name))
   (circuit-gates circuit)))

;; ============================================================
;; Compute path delay
;; ============================================================

(define (path-delay circuit ordered-edges)
  (apply
   +
   (map
    (lambda (edge)
      (define gate
        (find-gate
         circuit
         (second edge)))

      (unless gate
        (error
         "Path edge terminates at unknown gate: ~a"
         (second edge)))

      (gate-node-delay gate))
    ordered-edges)))

;; ============================================================
;; Print path
;; ============================================================

(define (print-path ordered-edges)
  (printf "\nSelected path:\n")

  (if (null? ordered-edges)
      (printf "  <empty>\n")
      (begin
        (printf "  ~a"
                (first
                 (first ordered-edges)))

        (for-each
         (lambda (edge)
           (printf " -> ~a"
                   (second edge)))
         ordered-edges)

        (printf "\n"))))

;; ============================================================
;; Print path delay
;; ============================================================

(define (print-path-delay circuit ordered-edges)
  (printf "\nPath delay:\n")

  (if (null? ordered-edges)

      (printf "  0\n")

      (begin
        (for-each
         (lambda (edge)
           (define gate
             (find-gate
              circuit
              (second edge)))

           (printf
            "  ~a -> ~a : delay = ~a\n"
            (first edge)
            (second edge)
            (gate-node-delay gate)))

         ordered-edges)

        (printf
         "  -------------------------\n")

        (printf
         "  Total delay = ~a\n"
         (path-delay
          circuit
          ordered-edges)))))

;; ============================================================
;; Print fault information
;; ============================================================

(define (print-fault circuit
                     fault
                     ordered-edges)

  (define site
    (sdd-fault-site fault))

  (printf "\nFault site:\n")
  (printf "  ~a\n" site)

  (if (ormap
       (lambda (edge)
         (eq? (second edge) site))
       ordered-edges)

      (printf
       "  Fault site is ON selected path: YES\n")

      (printf
       "  Fault site is ON selected path: NO\n")))

;; ============================================================
;; Complete 3G decoder
;; ============================================================

(define (decode-3g-result circuit
                          fault
                          model-file
                          vars-file)

  ;; Read exact solver model.
  (define model
    (read-model model-file))

  ;; Read exact variable mapping used to construct WCNF.
  (define var-map
    (read-var-map vars-file))

  ;; Decode PATH variables.
  (define selected-edges
    (decode-selected-edges
     model
     var-map))

  ;; Order them into PI -> PO path.
  (define ordered-edges
    (order-path selected-edges))

  ;; ----------------------------------------------------------
  ;; Report
  ;; ----------------------------------------------------------

  (printf "\n========================================\n")
  (printf "3G DECODED RESULT\n")
  (printf "========================================\n")

  (print-path
   ordered-edges)

  (print-path-delay
   circuit
   ordered-edges)

  (print-fault
   circuit
   fault
   ordered-edges)

  (printf "\n========================================\n"))
;; ============================================================
;; Test
;; ============================================================

(define sample-3g
  (circuit
   (list
    (input-node 'a)
    (input-node 'b)
    (input-node 'c)
    (input-node 'd))

   (list
    (gate-node 'n1 'and '(a b) 2)
    (gate-node 'n2 'nand '(c d) 1)
    (gate-node 'n3 'or '(n1 n2) 3)
    (gate-node 'out 'not '(n3) 4))

   (list
    (output-node 'OUT 'out))))
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

(define fault
  (sdd-fault 'n1))

(define fault2
  (sdd-fault 'n2))

;;(decode-3g-result
;; sample-3g
;; fault
;; "3g.model"
;; "3g.vars")

(decode-3g-result
 sample-3g-final
 fault2
 "3g-final.model"
 "3g-final.vars")


(provide
 read-var-map
 read-model
 decode-selected-edges
 order-path
 path-delay
 decode-3g-result)