#lang scheme


;; TERMINOLOGY USED
;; object: any Scheme value
;; instance: a value returned by a constructor in the object system
;; operation / generic function: a procedure whose definition is distributed amongst
;;                               the various objects it operates on.
;; method: one of the distributed pieces of a generic function's definition.

;; 2.1 SIMPLE OBJECTS
;; First approximation to OOP: an instance is represented as a proc that maps ops to 
;; methods. Methods are represented as procs that take the args to the op and performs
;; the op on the instance. eg. constructor for cells...
(define (make-simple-cell value)
  (lambda (selector)
    (cond ((eq? selector 'fetch) (lambda () value))
          ((eq? selector 'store!) (lambda (new-value) (set! value new-value)))
          ((eq? selector 'cell?) (lambda () #t))
          (else not-handled))))
(define (not-handled) (list 'not-handled))
; (define a-cell (make-simple-cell 13))
; ((a-cell 'fetch))
; ((a-cell 'store) 21)

;; nicer operation invocation
(define (operate selector the-instance . args)
  (apply (the-instance selector) args))
; (operate 'store! a-cell 34)

;; 2.2 INHERITANCE
;; "Named" cell that inherits behaviour from a simple cell
(define (make-named-cell value the-name)
  (let ((s-cell (make-simple-cell value)))
    (lambda (selector)
      (cond ((eq? selector 'name) (lambda () the-name))
            (else (s-cell selector))))))
;; objects returned by make-named-cell have 2 components:
;; one given by the expression (make-simple-cell ...)
;; one given by the expression (lambda (selector) ...)
;; The programmer controls shat state is shared by choosing the args passed to the constructors
;; and by choosing the expressions used to create the components.
;; In this style of inheritance, only behaviour is shared.
;; An instance can name its components, but can assume nothing about how they are implemented

;; We have single inheritance if the instance consults one additional component (beyound itself)
;; for behaviour.
;; We have multiple inheritance if the instance consults multiple additional components, eg...
(define (make-named-cell-2 value the-name)
  (let ((s-cell (make-simple-cell value)))
    (lambda (selector)
      (cond ((eq? selector 'name) (lambda () the-name))
            (else (let ((method (s-cell selector)))
                    (cond ((eq? method not-handled) (another-component selector))
                          (else method))))))))
(define (another-component selector) (lambda () 'not-implemnted))  ; stub
