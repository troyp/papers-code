#lang scheme
(define call/cc call-with-current-continuation)

;;  ***********************************
;;  *                                 *
;;  *  APPLICATIONS OF CONTINUATIONS  *
;;  *        Daniel P Friedman        *
;;  *                                 *
;;  ***********************************

;;  ==========================
;;  Key Terms and Definitions.
;;  ==========================

;; Properties of Scheme.
;; ---------------------
;; * call-by-value, lexically scoped dialect of Lisp.
;; * tail-call optimisation
;; * contains a purely functional subset
;; * supports imperative concepts: IO, assignment to lexical vars, 1st-class continuations

;; Escape Procedures.
;; ------------------
;; Consider the simple expr:  (* (/ 24 (f 0)) 3).  Possible outcomes...
;; 1. (f 0) is, say, 4, so the value is 18
;; 2. f is undefined at 0 -> computation aborts with error message
;; 3. f is undefined at 0 -> infinite loop - eg. (define (f n) (if (zero? n) (f n) n))
;; 4. (f 0) is 3, but f is an *escape procedure*, so the value is 4

;; Notation for Characterising Escape Procedures.
;; ----------------------------------------------
;; if f is a proc, f^ does exactly what f does, but (f^ ...) escapes as the answer and is
;; printed at the REPL. We call this "escaping to the top level".
;; The simplest escape proc is the identity, I^. If f=I^ above, the result is 0 and no
;; division occurs. A more powerful escape proc is +^...
;; Consider:    (* 3 (+^ 4 5))   ==>   9
;; But note that this is similar to   (* 3 (I^ (+ 4 5)))   ==>   9

;; Invoking Escape Procs is Replacing the Control Stack.
;; -----------------------------------------------------
;; In (* 3 (+^ 4 5)), at them time of invoking (+^ 4 5), the control stack is <3,*>
;; Invoking the escape proc causes the contents of the control stack to be forgotten - only the
;; +^ and its args are processed.
;; Note: although (+^ 4 5) and (I^ (+ 4 5)) appear the same, they are *not* operationally
;; identical - there is more control stack growth with (I^ (+ 4 5)).
;; Understanding of control stack growth will be important later when looking at sophisticated
;; uses of escape procs.

;; lambda^: creator of general escape procedures.
;; ----------------------------------------------
;; Given a lambda expr (lambda (x) ...), we assume we can form its escape counterpart
;; (lambda^ (x) ....). Later we'll clarify how such objects are built.

;; call/cc: a creator of escape procedures.
;; ----------------------------------------
