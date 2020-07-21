#lang scheme
;; LAMBDA: THE ULTIMATE IMPERATIVE

(define x 0)
;; example: sequencing.
;; begin: set x=5; x+1; end;
((lambda (dummy) (+ x 1)) (set! x 5))
;; multiple statements can be expressed by repetition of this pattern

;; example: expressing GOTO with lambda
(define n 1)
(letrec ((L1 (lambda ()
                 (if (= n 5) (L2)
                     (begin (display "block S1: adding 1\n")
                            (set! n (+ n 1))
                            (if (= n 5) (L2)
                                (begin (display "block S2\n") (L1)))))))
           (L2 (lambda () (display "block S3: n==5\n"))))
    (L1))

;; assignment: variable assignment within block scope can be modelled with lambda
;; eg.    begin: x=2; print x; end;    is represented as...
((lambda (x)
   (display x)
   (display "\n"))
 2)
;; This technique can handle all such simple assignment, even when GOTOs occur.
(define N 7)
(letrec
    ((L1 (lambda (n parity)
           (if (= n 0) (L2 n 0)
               (L3 (- n 1) parity))))
     (L3 (lambda (n parity)
           (if (= n 0) (L2 n 1)
               (L1 (- n 1) parity))))
     (L2 (lambda (n parity)
           (display parity))))
  (L1 N 0))
;; prints parity of N by counting down to 0, looping back and forth between 2 blocks
;; The trick to using lambda for assignments here is to pass around the variables which
;; may be assigned as arguments to the label functions.

