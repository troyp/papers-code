(require r5rs)
;(require racket/mpair)
;; *********************************************
;; *                                           *
;; *  CALL WITH CURRENT CONTINUATION PATTERNS  *
;; *             Ferguson & Duego.             *
;; *                                           *
;; *********************************************

;;====================
;; 1.1. CONTINUATIONS.
;;====================

;; 1.1.1. Contexts.
;;-----------------
;; A "context" is a procedure of one variable, □
;; To obtain the context of an expression we follow these 2 steps...
;; 1. replace the expression with □
;; 2. form a proc be wrapping the expression in (lambda (□) ... )
;; eg. In (+ 3 (* 4 (+ 5 6))), the context of (+ 5 6) is
(define example1
  (lambda (□) (+ 3 (* 4 □))))
;; We can extend this by expanding the first step to include simplification of the expression
;; eg. the context of (* 3 4) in (if (zero?  5) (+ 3 (* 4 (+ 5 6))) (* (+ (* 3 4) 5) 2)) is
(define example2
  (lambda (□) (* (+ □ 5) 2)))

;; 1.1.2. Escape Procedures.
;;--------------------------
;; The escape procedure is a new type of proc.
;; When an escape procedure is invoked, its result becomess the result of the entire computation -
;;   anything awaiting the result is ignored.
;; An example is the "error" proc: everyting waiting for the computation is discarded and an
;;   error message is immediately reported to the user.

;; Suppose we have a proc "escaper" which takes any proc as argument and returns a similarly
;;   defined escape proc. eg. (+ ((escaper *) 5 2) 3)  --->   10
;; We will define escaper later.

;; 1.1.3. Defining Continuations.
;;-------------------------------
;; call/cc is a proc of one argument, called the "receiver".
;; The receiver must be a proc of one argument, which is called a "continuation".
;; call/cc forms a continuation by...
;; 1. finding the context of (call/cc recevier) in the expression
;; 2. Applying escaper to the context, resulting in the continuation.
;; 3. This continuation is then supplied to the receiver as its argument.
;; eg. In  (+ 3 (* 4 (call/cc r)))  ,
;; the context of (call/cc r) is  (lambda (□) (+ 3 (* 4 □)))
;; So the original expr can be expanded to...
;; (+ 3 (* 4 (r (escaper (lambda (□) (+ 3 (* 4 □)))))))
;; If r is (lambda (continuation) 6), then the continuation and escape proc are never used -
;;   (r (escaper ...)) evaluates to 6 and the whole expression evaluates to 24.
;; If r is (lambda (continuation) (continuation 6)), then we have...
;;   (+ 3 (* 4 ((lambda (continuation) (continuation 6))
;;              (escaper (lambda (□) (+ 3 (* 4 □)))))))
;;   Since we invoke the continuation on 6, this becomes...
;;   ((escaper (lambda (□) (+ 3 (* 4 □)))) 6)
;;   ...which still evaluates to 24, but with a different sequence of computations.



;;=================
;; 2. THE PATTERNS.
;;=================


;; Escape From A Loop.
;;--------------------
;; Demonstates separation of code into control structure and action.
;; Continuations are used to break out of the control structure.
;; We use call/cc and a receiver func to get the escape proc before entering the loop.
;; The body of the loop then has access to a func that escape its context, breaking the loop.

;; Context: We have a system that can be seen as a combination of 2 constructs: contol, action.
;; Problem: How do we break out of the looping mechanism?
;; Forces:  * Delegation of looping to a single reusable construct allows for code reuse, but
;;            stopping could be difficult.
;;          * Merging the looping mechanism with each action results in code duplication.
;;          * Using a sentinel value that the looping mechanism can check restricts what values
;;            the function can return. Also, if we change the sentinel, we must change every
;;            function passed into the looping function.
;;          * We want to keep knowledge of how the looping works away from the action
;; Solution: Use a continuation to store the context before entering the loop and use this as an
;;           escape procedure when the exit condition is met.
;;           We begin by making a receiver function for the continuation.
;;           The receiver invokes the proc containing the looping mechanism with the action
;;           proc passed as arg. This action proc will be defined within the receiver to enable
;;           access to the exit-procedure.
;;           Terminating a loop is acieved by invoking the escape proc (ie. the continuation)
;;           within the action procedure, with whatever value is to be returned.
;; Template code...
(define infinite-loop
  (lambda (procedure)
    (letrec ((loop (lambda ()
                     (display "code executed before each action\n")
                     (procedure)
                     (display "code executed after each action\n")
                     (loop))))
      (loop))))
(define exit-condition-met #f)
(define action-procedure
  (lambda (args)
    (let ((receiver (lambda (exit-procedure)
                      (infinite-loop
                       (lambda ()
                         (exit-condition-met
                          (exit-procedure exit-value)
                          (display "Action to be performed.\n")))))))
      (call/cc receiver))))
;; Example of an action procedure...
(define count-to-n
  (lambda (n)
    (let ((receiver (lambda (exit-procedure)
                     (let ((count 0))
                       (infinite-loop
                        (lambda ()
                          (if (= count n)
                              (exit-procedure count)
                              (begin
                                (display "The count is: ")
                                (display count)
                                (display "\n")
                                (set! count (+ count 1))))))))))
    (call/cc receiver))))
;; Rationale: We've broken our loop into two parts: the control (loop) and the action proc.
;;            We can break the infinite loop using call/cc.
;;            Since the loop and action are separate, we can use multiple action procedures with
;;            the same looping mechanism, and write default behaviour to be performed each
;;            iteration without having to modify every action procedure.



;; Escape From Recursion.
;;-----------------------
;; A solution to escaping from a recursive procedure, similar to 'Escape From A Loop'.
;; We create an escape proc in the context before entering the recursive computation, which is
;; called when the exit condition is met to break out of the recursion.

;; Context: We're writing a recursive procedure.
;;          It's possible we may know the final result before the procedure is complete
;; Problem: How do we escape from a recursive procedure?
;; Forces:  * Exiting the computation (and discarding any intermediate results) will make the
;;            proc extremely efficient for special cases (possible avoiding all computation).
;;          * We may lose readability by adding complexity to the procedure.
;;          * Building a recursive computation may temporarily build up stack space.
;;          * This can usually be avoided if TCO is supported.
;; Solution: We use call/cc and a receiver function to create an escape function before we begin
;;           recursively building up the compuatation. We then perform the recursion using a
;;           recursive helper function defined within the receiver func (to give access to the
;;           continuation). When the exit condition is met, we can invoke the continuation with
;;           the exit value.
(define recursive-function
  (lambda (args)
    (let ((receiver (lambda (exit-procedure)
                      (letrec ((helper-function
                                (lambda (args)
                                  (cond
                                   (break-condition (exit-function 'exit-value))
                                   ; other cases and recursive calls
                                   ))))
                        (helper-function args)))))
      (call/cc receiver))))
;; Example: Suppose we wish to find the product of a nonempty list of numbers.
;;          If 0 occurs in the list, then the answer is 0.
(define product-list
  (lambda (nums)
    (let ((receiver (lambda (exit-on-zero)
                      (letrec ((product (lambda (nums)
                                          (cond ((null? nums) 1)
                                                ((zero? (car nums)) (exit-on-zero 0))
                                                (else (* (car nums) (product (cdr nums))))))))
                        (product nums)))))
      (call/cc receiver))))
;; Example: We can also apply this pattern to deep (ie. tree) recursion...
(define (product-list nums)
  (let ((receiver (lambda (exit-on-zero)
                    (letrec ((product (lambda (nums)
                                        (cond ((null? nums) 1)
                                              ((number? (car nums)) (if (zero? (car nums))
                                                                        (exit-on-zero 0)
                                                                        (* (car nums)
                                                                           (product (cdr nums)))))
                                              (else (* (product (car nums))
                                                       (product (cdr nums))))))))
                      (product nums)))))
    (call/cc receiver)))
(display (product-list (list '(1 2 3) 4 '(2 (5 5)))))    ; 1200
;; Rationale: using call/cc to escape the recursion adds little code to the original recursive
;;            proc and allows us to avoid much unnecessary computation.
;; Consequences: In a language supporting TCO, tail-recursive procs keep a running result for
;;               their computation, so this pattern is useless.
;;               However, if the computation is expensive at each step, and doesn't build up the
;;               stack too much, this pattern can be used as an alternative to tail-recursion.


;; Loop Via Continuations.
;;------------------------
;; Creates a loop using continuations. We use call/cc to create an escape proc that returns to
;; the context of the beginning of the loop body.
;; Context: We've dealt with escaping from an infinite loop. We may also wish to loop over a
;;          portion of a procedure or computation (which may be separated over portions of
;;          several procs) until some condition is met.
;; Problem: How do we loop until a condition is met using continuations?
;; Forces:  * Other looping mechanisms such as for, while, etc, are more common.
;;          * for and while loops must be contained within a single procedure.
;;          * Separating the mechanism for looping over several procs may decrease readability.
;;          * We may be able to write a program more easily/quickly by writing the code to
;;            perform a single iteration, then adding the looping mechanism later.
;;          * Looping can be added to portions of existing code with minimal effort and changes
;;            to the code.
;; Solution: We can create a loop by getting the continuation at the beginning of the portion of
;;           the procedure to be looped over and storing it in a temporary var. If we determine
;;           looping is necessary, we simply invoke the continuation - this escapes the current
;;           context and returns to the beginning of the loop.
;;           Getting a handle on the continuation can be easily done with the identity function..
;;           (call/cc (lambda (proc) proc)).
;;           For terminating the loop, we need to cause the termination condition to become false.
;;           This can be done by defining a variable outside the loop and using set! to set it.
;;           Another approach would be to add the current values to the continuation's argument
;;           (see example below).
;;           When the portion of code to be looped over is distributed amongst several procs, we
;;           can just pass the continuation along as an argument. Then whenever the looping
;;           condition is met, we can invoke the continuation.
;; Sample Code...
(define identity (lambda (x) x))
(define loop-condition #f)
(define (partial-loop args)
  (display "preliminary portion...")
  (let ((continue (call/cc identity)))
    (display "loop portion...")
    (set! loop-condition (not loop-condition))    ; loop once
    (if loop-condition
        (continue continue)
        (display "final portion"))))
;; Why (continue continue)?
;; Consider the context of the continuation...
;; (lambda (□) (let ((continue □)) (display "loop portion...") ..etc..(display "final portion)))
;; So when we call the continuation to escape back to this context, whatever arg we pass in to
;; the continuation will be bound to "continue" the next time around. So in order for our loop to
;; remain working, we must pass "continue" back into itself to be rebound to its name.

;; Example: Suppose we are given a nonempty set of numbers and wish to increment each element of
;;          the list by 1 until the first element is greater than or equal to 100.
;;          (We separate out the initial construction of the continuation for readability)...
;;          Note that here we add the current values to the contination's argument, rather than
;;          set variables that we initially declare outside the loop.
(define (get-continuation-with-values values)
  (let ((receiver (lambda (proc) (cons proc values))))
    (call/cc receiver)))
(define (loop-with-current-value values)
  (let ((here-with-values (get-continuation-with-values values)))
    (let ((continue (car here-with-values))
          (current-values (cdr here-with-values)))
      (display current-values)(newline)
      (if (< (car current-values) 100)
          (continue (cons continue
                          (map (lambda (x) (+ x 1))
                               current-values)))
          (display "Done!")))))
;; If we call (loop-with-current-value '(1 2 3)), the first value of here-with-values will be
;; '(*continuation* 1 2 3). We separate this into *continuation* (bound to "continue") and
;; '(1 2 3) (bound to current-values).
;; The first time we encounter the code...
;; (continue (cons continue
;;                 (map (lambda (x) (+ x 1)) current-values)))
;; ...we rebind here-with-values to (*continuation* 2 3 4) and run through the code again. The
;; second time we execute the looping code, we rebind here-with-values to (*continuation* 3 4 5)
;; ..and so on. This continues until here-with-values gets to (*continuation* 100 101 102) at
;; which point (car current-values) is 100 and the exit-condition is satisfied, so we don't
;; enter the branch which calls continue.

;; Rationale: The use of continuations allows a looping mechanism to be created easily with
;;            little code. With a few comments explaining the use of continuations to return to
;;            the beginning off the loop body, the code remains quite readable.
;;            With the code broken into procs, code can be reused in different loops that use
;;            this mechanism (passing in their own continuations to the procedures).


;; Escape From And Reentry Into Recursion.
;;----------------------------------------
;; Escape from a computation while allowing the process to be reversed.
;; We create a break proc which stores the current continuation in a specified scope, then escapes
;; from the recursive process. Invoking the stored continuation resumes the recursive process.
;; Context: We have a recursive process (possibly deep recursion) from which we wish to escape
;;          but retain the ability to go back and continue execution. This would allow a coder
;;          to debug the code by testing if some condition is met. We can also use it to change
;;          internal values, etc, of the function while it progresses to manipulate the
;;          computation on the fly.
;;          The reentry of the computation can be instigated by the user.
;; Problem: How do we escape a recursive computation while retaining the ability to reenter at
;;          the point of exit?
;; Forces: * We want to minimise the amount of code added while maintaining readability.
;;           This will allow the changes to be easily removed if made for debugging purposes.
;;         * We want to avoid modifications to the interpreter/compiler - we don't want a full
;;           debugger.
;; Solution: 
;;           We need 2 globally available procs..
;;           "break" must do 2 things: (1) store the current continuation; and (2) stop the
;;                   current computation. break can also take an arg which is passed in to the
;;                   continuation when it is resumed.
;;           "resume-computation" will resume the currently escaped computation.
;;           For "break", We use the "escaper" proc from "Scheme and the Art of Programming" to
;;           escape the computation and call/cc to store the context.
;; Sample Code...
(define resume-computation 'to-be-initialised)
(define (break arg)
  (let ((exit-procedure (lambda (continuation)
                          (set! resume-computation (lambda () (continuation arg)))
                          (display "Execution paused. Try (resume-computation)\n")
                          ((escaper (lambda () arg))))))
    (call/cc exit-procedure)))
;; to create the "escaper" proc, we must first store a continuation ("escape-thunk") with the
;; context (lambda (□) (□)). We can do this by first assigning it to some proc...
(define escape-thunk (lambda () "escape-thunk initialised"))
;; Then we need a receiver func for a continuation that will assign the continuation to
;; escape-thunk...
(define (escape-thunk-init continue)
  (set! escape-thunk continue))
;; then we just create the continuation using call/cc with this receiver func...
;; ((call/cc escape-thunk-init)) ; code as written gives error
(call/cc escape-thunk-init) ; kludge to remove error: doesn't work properly (doesn't print values
;; at each step, only when "finished"
;; TODO: Fix this! work out why the original code won't load and/or modify
;; ..and define escaper as...
(define escaper
  (lambda (procedure)
    (lambda args
      (escape-thunk (lambda () (apply procedure args))))))
;; Example: flatten-list proc which escapes every time a non-list arg is passed...
;; NOTE: escaper is NOT working as specified
;; eg. (+ ((escaper *) 5 2) 3)  should give 10, but gives a procedure.
;; TODO: fix escaper and document.
(define (flatten-list arg)
  (cond ((null? arg) '())
        ((not (list? arg)) (list (break arg)))
        (else (append (flatten-list (car arg))
                      (flatten-list (cdr arg))))))


;; Coroutines.
;;------------
;; Coroutines allow sequential control between procs. We store the current context of a proc
;; so it can later be invoked.
;; Context: The process of many progs can be viewed as passing control sequentially among several
;;          entities, like a card game where each player takes his turn and passes control to the
;;          next player.
;; Problem: How do we allow sequential control among several entities?
;; Forces: * Having each entity's process in one procedure lets us view each process in one place.
;;         * Breaking each enitity's process up into many small procs that force the sequential
;;           control makes it hard to follow and hard to modify the control flow.
;;         * Using a mechanism such as threads or semaphores may have a high overhead or be
;;           unavailable.
;;         * Switching of threads is traditionally done at a lower level and designing threads to
;;           wait until called upon to act would be complex and hard to follow.
;; Solution: We can use continuations to implement coroutines. Coroutines allow interruption and
;;           resumption of processes. We begin by creating a coroutine for each entity that will
;;           share control.
;;           The procedure "coroutine-maker" (from "Scheme and the Art of Programming") takes as
;;           arg a proc representing the "body" or actions of the coroutine.
;;           This "body-procedure" takes 2 args: a "resumer" proc for resuming the next coroutine,
;;           and an initial value.
;;           coroutine-maker creates a private update-continuation! which stores the current
;;           continuation of the proc. Each time the coroutine is invoked with a value, it passes
;;           that value to its continuation.
;;           When its time for the coroutine to pass control to the next coroutine, it updates its
;;           current state to its current continuation, then resumes the second coroutine.
;; Sample Code...
(define coroutine-maker
  (lambda (body-proc)
    (let* ((saved-continuation '())
           (update-continuation! (lambda (v)
                                   (display "updating\n")
                                   (set! saved-continuation v)))
           (resumer (resume-maker update-continuation!))
           (first-time #t))
      (lambda (value)
        (if first-time
            (begin (set! first-time #f)
                   (body-proc resumer value))
            (saved-continuation value))))))
(define resume-maker
  (lambda (update-proc!)
    (lambda (next-coroutine value)
      (let ((receiver (lambda (continuation)
                        (update-proc! continuation)
                        (next-coroutine value))))
        (call/cc receiver)))))
;; Example: using the coroutine-maker, we create two procs, "ping" and "pong" which will switch
;;          control back and forth between them three times.
;;          ping-procedure and pong-procedure will be the body-procs for ping and pong, resp...
(define ping
  (let ((ping-procedure (lambda (resume value)
                          (display "Pinging 1\n")
                          (resume pong value)
                          (display "Pinging 2\n")
                          (resume pong value)
                          (display "Pinging 3\n")
                          value)))
    (coroutine-maker ping-procedure)))

(define pong
  (let ((pong-procedure (lambda (resume value)
                          (display "Ponging 1\n")
                          (resume ping value)
                          (display "Ponging 2\n")
                          (resume ping value)
                          (display "Ponging 3\n")
                          value)))
    (coroutine-maker pong-procedure)))
;; (ping 1) results in the expected output, BUT the *value* is wrong!
;; It should have value 1 according to the paper, but instead has #<void>.
;; This is the same value causing problems in the last section
;; (" procedure application: expected procedure, given: #<void> (no arguments)")
;; Rationale: Using continuations to create coroutines allows us to write the process for each
;;            entity in it own proc, which makes the program more readable and easier to modify.
;;            Continuations let us achieve the same result as threads without changes to the
;;            compiler/interpreter.


;; Non-Blind Backtracking.
;;------------------------
;; Allows us to jump to a past or future point of execution. Continuations are stored for past
;; and future points and "devil" and "angel" procs let us jump back/forward resp.
;; Context: We've reached a point in the computation where we want to stop or suspend the current
;;          process and jump to a previous point in the computation.
;; Problem: How do we construct a mechanism that allows us to stop or suspend the computation and
;;          jump back to a past point of execution and follow another path to determine the
;;          solution.
;; Forces: * We need a simple method for moving back and forth in a computation which is robust
;;           enough to handle situations where we can't just go back and forth.
;;         * Modifying a program to add the mechanism should be simple and not add too much
;;           complexity to the code that would make it unreadable.
;;         * Implementing backtracking by breaking the process into recursive processes that
;;           achieve backtracking by returning back to the calling procedure, would be very
;;           inefficient if we want to return to a point that was reached early in the program.
;;           It would also be hard to follow for complex backtracking programs.
;; Solution: We can implement non-blind backtracking using call/cc to get escape procs to
;;           various contexts. We store these in a global structure so they can be invoked and
;;           the process will return to the corresponding contexts in the computation.
;;           In Friedman, Haynes & Kohlbecker, "Programming with Continuations" (1984), the
;;           concept of "devils", "angels" and "milestones" are introduced. A devil will return
;;           us to the context in which the last milestone was created. The devil will pass on
;;           to the continuation the value that was passed to it. This value is used as though
;;           it were the result of the original milestone expression, potentially allowing us
;;           to follow a different path.
;;           An angel will send the computation forward to the last encounter with a devil.
;;           Again, the value passed to the angel is passed on to the devil's continuation, so
;;           we return to the context of the devil with this value replacing the value returned
;;           by the devil. This lets us move to more advanced states.
;;           A milestone records the current context for use by encountered devils.
;;           We need 2 data structures for holding (a) milestones; and (b) future points (where
;;           devils were invoked). A simple approach would be to keep two stacks (past and
;;           future) since in the simplest case, we would only be returning to the previous
;;           milestone or the previous invocation of a devil.
;; Sample code...
(define (push stack data)
  (let ((first (car stack))
        (rest  (cdr stack)))
    (set-car! stack data)
    (set-cdr! stack (cons first rest))))
;; we define pop to return the identity function if the stack is empty...
(define (pop stack)
    (let ((first (car stack))
        (rest  (cdr stack)))
      (if (eq? first 'empty)
          (lambda (x) x)
          (begin (set-car! stack (cadr stack))
                 (set-cdr! stack (cddr stack))
                 first))))
(define past (list 'end))
(define future (list 'end))
;; milestone stores the current state in the past stack and returns the intial value...
(define (milestone x)
  (call/cc (lambda (k)
             (push past k)
             x)))
;; a devil stores the current continuation as a future and returns to the last milestone with
;; a new value...
(define (devil x)
  (call/cc (lambda (k)
             (push future k)
             ((pop past) x))))
;; an angel returns to the next future on the stack...
(define (angel x)
  ((pop future) x))
;; Rationale: We can use these procs to store important points in the computation to return to,
;;            return to stored states to continue computation and return to computations which
;;            were only partially complete.
;;            These procs provide a simple isolated framework which can be reused in various
;;            programs.
;;            Note that we can expand the behaviour by changing the data structures holding the
;;            continuations from stacks to something else.

;; Example (from Friedman, "An Introduction to Scheme")..
(define (mda)
  (cond ((= (milestone 3) 4) (begin (display "here!")
                                    (angel 'all-is-well)))
        ((= (milestone 4) 5) (angel (devil 4)))
        (else (begin (display "made it to ")
                     (devil 5)))))


;; Multitasking.
;;--------------
