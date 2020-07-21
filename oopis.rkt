;; OOP in Scheme
;; Adams & Rees.

;; 2. OOP USING PROCEDURES.
;;=========================
;; 2.1. Simple Objects.
;;---------------------
;; Instance = Procedure Mapping Operations -> Methods
;; Method   = Procedure Taking args of the op and performing it on the instance
;; eg. a constructor for cells...
(define (make-simple-cell value)
  (lambda (selector)
    (case selector
      ['fetch
       (lambda (self) value)]
      ['store!
       (lambda (self new-value)
	 (set! value new-value))]
      ['cell?
       (lambda (self) #t)]
      [else not-handled])))
(define not-handled (list 'not-handled))
;; example...
(define a-cell (make-simple-cell 13))
((a-cell 'fetch) a-cell)
((a-cell 'store!) a-cell 21)
((a-cell 'cell?) a-cell)
;; to improve readability of operator invocation, we define...
;(define (operate selector the-instance . args)
;  (apply (the-instance selector) args))
(define (operate selector the-instance . args)
  (apply (the-instance selector) (cons the-instance args)))
(operate 'store! a-cell 34)
;; operate is analogous to send (Flavors) or => (Common Objects)

;; 2.2. Inheritance.
;;------------------
;; a named cell inheriting behaviour from a simple cell...
(define make-named-cell
  (lambda (value the-name)
    (let ((s-cell (make-simple-cell value)))
      (lambda (selector)
	(if (eq? selector 'name)
	    (lambda () the-name)
	    (s-cell selector))))))
;; we say these objects have 2 components: one yielded by (make-simple-cell ...)
;; and one by (lambda (selector) ...)
;; We could provide MI by expanding (s-cell selector) as follows..
;; ... (else (let ((method (s-cell selector)))
;;              (cond ((eq? method not-handled)
;;                     (another-component selector))
;;                    (else method))))
;; Only methods are inherited, but distinct instances may share components,
;;   and hence, the components' state.
;; When behaviouur for an op is defined by multiple components, the one defined
;;   first shadows the others.
;; This approach could be called delegation rather than inheritance.

;; Operations on self.
;;--------------------
;; In simple cases, we could wrap the proc in (letrec ((self ...)), but this
;;   won't cope with inheritance.
;; We need to add a self arg to every op, and change operate accordingly.

;; Operations on Components.
;;--------------------------
;; components can be given names, and so the comopsite's method for a given op
;;   may be defined in terms of one of its components.
;; eg..
(define make-filtered-cell
  (lambda (value filter)
    (let ((s-cell (make-simple-cell value)))
      (lambda (selector)
	(cond ((eq? selector 'store!)
	       (lambda (self new-value)
		 (when (filter new-value)
		   (operate 'store! s-cell new-value))))
	      (else (s-cell selector)))))))
;; analogous to 'sending to super' in Smalltalk or call-next-method in CLOS
;; note: the self arg is the component, not the composite!
;; what if we need the composite? we use a variant of operate...
(define (operate-as component selector composite . args)
  (apply (component selector) (cons composite args)))


;; 3. INTEGRATION WITH THE REST OF THE LANGUAGE.
;;==============================================
;; 3.1. Operations on Non-Instances.
;;----------------------------------
