#lang racket

;; ============================================================
;; Increment 4A
;; Decode primary-input test vector
;; ============================================================

;; This decoder uses the exact SAT-variable mapping saved in
;; 3g-final.vars.
;;
;; It does NOT recreate SAT variables and does NOT call
;; value-var-id.
;;
;; The model and variable map therefore remain an exact pair:
;;
;;     3g-final.model
;;     3g-final.vars
;;
;; ============================================================


;; ============================================================
;; Read SAT variable mapping
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
;; Read SAT model
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
;; Determine whether a SAT variable is TRUE
;; ============================================================

(define (model-contains-true? model id)
  (member id model))


;; ============================================================
;; Decode one value variable
;; ============================================================

;; entry format:
;;
;;   (id value node frame)
;;
;; For example:
;;
;;   (25 value c 0)
;;
;; means:
;;
;;   value(c,0)
;;
;; ============================================================

(define (decode-value model
                      var-map
                      node
                      frame)

  (define result
    (for/first ([id (in-hash-keys var-map)]
                #:when
                (let ([entry (hash-ref var-map id)])
                  (and
                   (eq? (second entry) 'value)
                   (eq? (third entry) node)
                   (= (fourth entry) frame))))
      id))

  (unless result
    (error
     "Could not find value variable for ~a frame ~a"
     node
     frame))

  (if (model-contains-true? model result)
      1
      0))


;; ============================================================
;; Decode primary-input vector
;; ============================================================

(define (decode-input-vector model
                             var-map
                             inputs
                             frame)

  (map
   (lambda (input)
     (cons
      input
      (decode-value
       model
       var-map
       input
       frame)))
   inputs))


;; ============================================================
;; Print vector
;; ============================================================

(define (print-vector label vector)

  (printf "~a:\n" label)

  (for-each
   (lambda (entry)
     (printf
      "  ~a = ~a\n"
      (car entry)
      (cdr entry)))
   vector))


;; ============================================================
;; Complete Increment 4A decoder
;; ============================================================

(define (decode-4a model-file
                   vars-file
                   inputs)

  (define model
    (read-model model-file))

  (define var-map
    (read-var-map vars-file))

  (define frame-0
    (decode-input-vector
     model
     var-map
     inputs
     0))

  (define frame-1
    (decode-input-vector
     model
     var-map
     inputs
     1))

  (printf "\n========================================\n")
  (printf "INCREMENT 4A - TEST VECTOR\n")
  (printf "========================================\n")

  (print-vector
   "Frame 0"
   frame-0)

  (printf "\n")

  (print-vector
   "Frame 1"
   frame-1)

  (printf "\n========================================\n")

  (values frame-0
          frame-1))


;; ============================================================
;; Test
;; ============================================================

(define inputs
  '(a b c d))

(decode-4a
 "3g-final.model"
 "3g-final.vars"
 inputs)


(provide
 read-var-map
 read-model
 decode-value
 decode-input-vector
 decode-4a)
