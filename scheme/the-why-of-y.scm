#lang scheme

(let ((g (lambda (h n)
             (if (< n 2) 1 (* n (h h (- n 1)))))))
    (g g 5))

(let ((g (lambda (h)
             (lambda (n)
               (if (< n 2) 1 (* n ((h h) (- n 1))))))))
    ((g g) 5))

(let ((g (lambda (h)
             (lambda (n)
               (let ((f (lambda (q n)
                       (if (= n 1) 1 (* n (q (- n 1)))))))
                 (f (h h) n))))))
    ((g g) 5))

(let ((g (lambda (h)
             (lambda (n)
               (let ((f (lambda (q)
                          (lambda (n)
                            (if (= n 1) 1 (* n (q (- n 1))))))))
                 ((f (h h)) n))))))
    ((g g) 5))

(let ((f (lambda (q) (lambda (n) (if (= n 1) 1 (* n (q (- n 1))))))))
    (let ((g (lambda (h) (lambda (n) ((f (h h)) n)))))
      ((g g) 5)))
; here, the first line is essentially the factorial function "generator",
; and the second line is the Y-combinator

; abstracting over f...
(define Y
  (lambda (f)
    (let ((g (lambda (h)
               (lambda (x) ((f (h h)) x)))))
      (g g))))

; factorial generator (non-tail-recursive)
(define (fact-recurse f)
  (lambda (n)
    (if (= n 0)
        1
        (* n (f (- n 1))))))
; factorial generator (tail-recursive form)
(define (fact-iter f)
  (lambda (n)
    (define (iter n accumulator)
      (if (= n 0)
          accumulator
          (iter (- n 1)
                (* n accumulator))))
    (iter n 1)))
; factorial function generated from fact-iter
(define factorial
  (Y fact-iter))
