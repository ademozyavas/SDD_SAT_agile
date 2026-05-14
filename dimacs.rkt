#lang racket



;; ------------------------------------------------------------
;; Write DIMACS CNF
;; ------------------------------------------------------------

(define (write-dimacs filename clauses num-vars)

  (call-with-output-file
   filename

   (lambda (out)

     ;; header
     (fprintf out
              "p cnf ~a ~a~n"
              num-vars
              (length clauses))

     ;; clauses
     (for-each
      (lambda (clause)

        (for-each
         (lambda (lit)
           (fprintf out "~a " lit))
         clause)

        (fprintf out "0~n"))

      clauses))

   #:exists 'replace))


(provide write-dimacs)