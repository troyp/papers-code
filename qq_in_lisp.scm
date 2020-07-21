#lang scheme

;; ****************************
;; *                          *
;; *  QUASIQUOTATION IN LISP  *
;; *       Alan Bawden        *
;; *                          *
;; ****************************

(define n 10)
(define v (make-vector n))
(define a 'v)
(define initval 3)

(define (initarray1-expr array-name array-size init-val)
  (list 'do '((i 0 (+ 1 i)))
        (list (list  '>= 'i 'array-size))
        (list 'vector-set! array-name
              'i
              init-val)))

;; using quasiquotation...
(define (initarray-expr array-name array-size init-val)
  `(do ((i 0 (+ 1 i)))
     ((>= i ,array-size))
     (vector-set! ,array-name
                  i
                  ,init-val)))
;; initarray: args should be passed in quoted,
;; eg. (initarray 'a 'n 'initval)
(define (initarray . args)
  (eval (eval (cons initarray-expr args))))


(define (compare-to var val)
  `(cond ((not (and (real? ,var) (real? ',val))) #f)
         ((< ,var ',val)  1)
         ((= ,var ',val)  0)
         ((> ,var ',val) -1)
         (else 'error)))

;; ===================
;;   s3.1: Splicing.
;; ===================





;; Utility Procs...

;; call-with-names: calls function on quoted arguments,
;; eg. (call-with-names initarray a n initval)  ==>  (initarray 'a 'n 'initarray)
(define-syntax call-with-names
  (syntax-rules ()
    ((_ f arg1 arg2 ...)
     (f (quote arg1) (quote arg2) ...))))
