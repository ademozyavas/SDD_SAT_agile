#lang racket


(struct sat-var (node time)
  #:transparent)

(define next-id 0)

(define var->id (make-hash))
(define id->var (make-hash))

(define (reset-vars!)
  (set! next-id 0)
  (set! var->id (make-hash))
  (set! id->var (make-hash)))

(define (allocate-var! v)
  (cond
    [(hash-has-key? var->id v)
     (hash-ref var->id v)]

    [else
     (set! next-id (+ next-id 1))

     (hash-set! var->id v next-id)
     (hash-set! id->var next-id v)

     next-id]))

(define (lookup-var id)
  (hash-ref id->var id))

(define (lookup-id v)
  (hash-ref var->id v))

(provide
 struct-out sat-var
 allocate-var!
 lookup-var
 lookup-id
 reset-vars!)
