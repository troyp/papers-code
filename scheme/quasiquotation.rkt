#lang racket
(require srfi/1)

(define expr1
  (let ((array-name 'a)
        (array-size 20)
        (init-val 1))
    `(do ((i 0 (+ 1 i)))
       ((>= i ,array-size))
       (vector-set! ,array-name
                    i
                    ,init-val))))

(define expr2
  (let ((var 'a)
        (val 'b)
        (expr '(list 1 2))
        (more-clauses (list '((eq? c #F) #F)
                            '(else (list 2 3)))))
    `(cond ((eq? ,var ',val) ,expr)
           . ,more-clauses)))

