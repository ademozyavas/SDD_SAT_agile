#lang racket

;; ============================================================
;; Write Weighted CNF (WCNF)
;; ============================================================
;;
;; Each clause is represented as:
;;
;;   (cons weight clause)
;;
;; where:
;;
;;   weight  = positive integer
;;   clause  = list of SAT literals
;;
;; Example:
;;
;;   (cons 10 '(1 -2 3))
;;
;; represents:
;;
;;   10 1 -2 3 0
;;
;; Hard clauses use TOP as their weight.
;; Soft clauses use their actual objective weight.
;;
;; WCNF header:
;;
;;   p wcnf <number-of-variables>
;;          <number-of-clauses>
;;          <TOP>
;;
;; TOP must be greater than the sum of all soft weights.
;; ============================================================


;; ------------------------------------------------------------
;; Compute TOP
;; ------------------------------------------------------------

(define (compute-top soft-clauses)

  (+ 1
     (apply +
            (map car soft-clauses))))


;; ------------------------------------------------------------
;; Write WCNF
;; ------------------------------------------------------------

(define (write-wcnf filename
                     hard-clauses
                     soft-clauses
                     num-vars)

  (define top
    (compute-top soft-clauses))

  (define all-clauses
    (append

     ;; Hard clauses.
     (map (lambda (clause)
            (cons top clause))
          hard-clauses)

     ;; Soft clauses.
     soft-clauses))

  (call-with-output-file
   filename

   (lambda (out)

     ;; Header.
     (fprintf out
              "p wcnf ~a ~a ~a~n"
              num-vars
              (length all-clauses)
              top)

     ;; Clauses.
     (for-each
      (lambda (weighted-clause)

        (define weight
          (car weighted-clause))

        (define clause
          (cdr weighted-clause))

        (fprintf out "~a " weight)

        (for-each
         (lambda (lit)
           (fprintf out "~a " lit))
         clause)

        (fprintf out "0~n"))

      all-clauses))

   #:exists 'replace))


(provide
 write-wcnf
 compute-top)
