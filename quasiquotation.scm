(define array-name (make-vector array-size)) (define array-size 10) (define init-val 1)
;; ****************************
;; *                          *
;; *  QUASIQUOTATION IN LISP  *
;; *       Alan Bawden.       *
;; *                          *
;; ****************************


;; Quasiquotation is the technology usually employed in Lisp to write program-generating programs



;; 1. INTRODUCTION.
;;-----------------
;; Quasiquotation is a parametrized version of normal quotation:
;; rather than specify a value exactly, some "holes" are left to be filled in later.
;; A quasiquotation is a "template".

;; Suppose we wanted to write a program-generating program in C.
;; We could use "fprintf" to perform quasiquotation, ie. interpolating values into an expression.
;; fprintf(out, "for (i=0; i<%s; ++i) %s[i] = %s;\n", array_size, array_name, init-val);
;; This has the advantages...
;; 1. brevity, clarity.
;; 2. the generating expressions look very similar to the generated output:
;;    this lets us see at a glance that an expression will produce valid C code, which would
;;    not be the case if we used a sequence of "puts"s.
;; However, fprintf has some problems as a quasiquotation mechanism...
;; 1. The params are associated with values positionally: you have to count args to match them.
;; 2. The underlying string substitution has no understanding of the syntactic structure of the
;;    language: as a result, unusual values may change the meaning of generated code in
;;    unexpected ways, eg. if array_name was "*x", the resulting code would be parsed as "*(x[i])"
;;    rather than "(*x)[i]".
;; The first problem could be addressed by somehow moving the parameter expressions into the
;;   template (like Perl's string interpolation), eg...
;;   subst("for (i=0; i<$array_size; ++i) $array_name[i] = $init_val;");
;; The second problem can be reduced by inserting extra pairs of parens to ensure expressions
;;   parse the way intended (this is what C macro writers do).
;; However, ideally, we would like to use a more suitable data structure: flat character strings
;;   are not a good match for recursive structures like expressions.

;; 3 Goals for a Successful Quasiquotation Implementation...
;; 1. The programmer should be able to write down what he wants the output to look like,
;;    modified only slightly to parametrize it.
;; 2. The parameter expressions should appear inside the templates, in the places where their
;;    values will be inserted.
;; 3. The underlying data structures manipulated by quasiquotation should be rich enough to
;;    represent recursively defined structures like expressions.



;; 2. QUASIQUOTATION IN LISP.
;;---------------------------
;; The natural way to generate a Lisp program in Lisp is to build S-Exprs.
;; So if we wish to generate the sexpr...
(do ((i 0 (+ i 1)))
    ((>= i array-size))
  (vector-set! array-name i init-val))
;; ...the primitive Lisp code to construct this sexpr is...
(list 'do '((i 0 (+ i 1)))
      (list (list '>= 'i array-size))
      (list 'vector-set! array-name 'i init-val))
;; Lisp's quasiquotation facility lets us replace this with...
`(do ((i 0 (+ i 1)))
     ((>= i ,array-size))
   (vector-set! ,array-name i ,init-val))

;; Lisp's expr parser (traditionally called "read") expands a backquote followed by a template
;;   into Lisp code that constructs the defired sexpr.
;; So the two Lisp expressions above are identical.