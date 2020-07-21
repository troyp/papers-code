#lang scheme
(define hukairs {})
(define add1
  (lambda (n) (+ n 1)))
(define length
  (lambda (l)
    (if (null? l)
        0
        (add1 (length (cdr l))))))

(define length0
  (lambda (l)
    (if (null? l) 0
        (add1 (hukairs (cdr l))))))

(define length1
  (lambda (l)
    (if (null? l) 0
        (add1 ((lambda (l)
                (if (null? l) 0
                    (add1 (hukairs (cdr l)))))
               (cdr l))))))

(define length2
  (lambda (l)
    (if (null? l) 0
        (add1 ((lambda (l)
                (if (null? l) 0
                    (add1 ((lambda (l)
                            (if (null? l) 0
                                (add1 (hukairs (cdr l)))))
                           (cdr l)))))
               (cdr l))))))

(define new-length0
  ((lambda (length)
     (if (null? l) 0
         (add1 (length (cdr l)))))
   hukairs))

(define new-length1
  ((lambda (length)
     (if (null? 0) 0
         (add1 (length (cdr l)))))
   ((lambda (length)
      (if (null? l) 0
          (add1 (length (cdr l)))))
    hukairs)))

(define new-length2
  ((lambda (length)
     (if (null? l) 0
         (add1 (length (cdr l)))))
   ((lambda (length)
      (if (null? l) 0
          (add1 (length (cdr l)))))
    ((lambda (length)
       (if (null? l) 0
           (add1 (length (cdr l)))))
     hukairs))))

(define mk-length
  (lambda (length)
    (if (null? l) 0
        (add1 (length (cdr l))))))

(define final-length0
  ((lambda (mk-length)
     (mk-length hukairs))
   (lambda {length}
     {if (null? l) 0
         (add1 (length (cdr l)))})))

(define final-length1
  ((lambda (mk-length)
     (mk-length
      (mk-length hukairs)))
   (lambda {length}
     {if (null? l) 0
         (add1 (length (cdr l)))})))

(define final-length2
  ((lambda (mk-length)
     (mk-length
      (mk-length
       (mk-length hukairs))))
   (lambda {length}
     {if (null? l) 0
         (add1 (length (cdr l)))})))

