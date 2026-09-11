#lang racket

(require "write-wcnf.rkt")

(define hard-clauses
  (list
   (list 1 2)
   (list -1 3)))

(define soft-clauses
  (list
   (cons 2 (list 1))
   (cons 1 (list 2))
   (cons 3 (list 3))))

(write-wcnf
 "test.wcnf"
 hard-clauses
 soft-clauses
 3)

(printf "WCNF written.\n")