#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/compiler/bindchek.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.7
 | File mod date:    2003-12-15 09:34:00
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  compiler
 |
 `------------------------------------------------------------------------|#

;;
;; there are three cases to consider for an <ic-bind> B
;;
;;  (1) regular initialization
;;          (let (((a <string>) (foo))
;;                ((b <list>) (cons 1 2)))
;;             ...)
;;      in this case, B.inits isa <ic-multi>
;;
;;  (2) argument validation
;;          (define (foo (a <string>) (l <list>))
;;              ...)
;;      in this case, B.inits == #f
;;
;;  (3) procedure initialization (binding multiple return values)
;;          (bind ((a b c (foo)))
;;             ...)
;;      in this case, B.inits isa <expr-icode> that's NOT an <ic-multi>
;;      (in this particular case, it would be an <ic-call>)
;;
;;  case (2) requires a wrapper around the body -- there's nowhere else
;;  to hang new code
;;
;;  Cases (1) and (3)
;;  can be handled with a wrapper around the initializer(s)
;;  This can even be done for case (3) with something like
;;
;;         (bind (((a <string>) (b <integer>) (foo)))
;;           ...)
;;   ==>
;;         (bind (((a <string> :trust-me) 
;;                 (b <integer> :trust-me)
;;                        (bind ((a b (foo)))
;;                           (assert-type a <string>)
;;                           (assert-type b <integer>)
;;                           (values a b))))
;;           ...)
;;
;;  with decent register assignment, there wouldn't be any more code
;;  or register motion than you could get in any case.
;;
;;  note that a #rest arg should be propagated to the INNER bind, with
;;  a free <list> :trust-me on the outer one
;;
;;         (bind (((a <string>) (b <integer>) #rest x (foo)))
;;           ...)
;;   ==>
;;         (bind (((a <string> :trust-me) 
;;                 (b <integer> :trust-me)
;;                 (x <list> :trust-me)
;;                        (bind ((a b #rest x (foo)))
;;                           (assert-type a <string>)
;;                           (assert-type b <integer>)
;;                           (values a b x))))
;;           ...)
;;   
;;
;;  if the #rest arg is MORE restrictive than <list> 
;;  (ie, <pair> or <empty-list>), then the check still needs to be made
;;
;;  if the #rest arg is LESS restrictive but compatible (an ancestor),
;;  then the <list> restriction is free, so put it in (this will happen
;;  a lot when the #rest are is <object>)
;;
;;  if the #rest arg is incompatible, we have two options:
;;
;;    if we want to be nice and the arg isa <collection>, then we can
;;    coerce the list to the desired collection (emit an "as" call?)
;;    if we want to be efficient and the arg isa <vector> or some useful
;;    common classes (other than <vector>, none comes to mind), then we
;;    can have special VM insns that collect into that form instead of
;;    into a list (e.g., COLLECT3_AS_VECTOR()?)
;;
;;    if we aren't being nice or the arg isn't a <collection>, signal
;;    a compile time SEVERE WARNING (== this code WILL trap at runtime)
;;    and insert an (assert #f).  For that matter, what's the point?
;;    Just make it a fatal error.  They can comment it out if they want.
;;
;;  (on the other hand, if we want to be lazy -- the current case --
;;  then we prohibit decls on #rest args more restrictive than <object>!)


;; return a new <lexical-contour> that only has <object> type restrictions
;; this is what we'll use to make the new <ic-bind> that binds
;; the values temporarily

(define (objs-only-envt envt)
  (let ((e (make <lexical-contour>
		 name->bindings: '() ;; will never do a lookup in here
		 bindings: '()
		 lexical-enclosing: '()
		 dynamic-enclosing: envt)))
    (set-bindings! e (map (lambda ((b <lexical-var>))
			    (make <lexical-var>
				  name: (name b)
				  type: '<obj>))
			  (bindings envt)))
    e))
	

;;  vars is a list of the <lexical-var>'s that hold the values
;;  temporarily (the bindings from objs-only-envt)
;;
;;  types is a parallel list of types from the original <lexical-envt>


(define (mfg-required-type-checks vars types initers)
  (let loop ((vars vars)
	     (types types)
	     (initers initers)
	     (checks '()))
    (if (null? vars)
	checks
	;; first, try to divine the type that the value already
	;; is, by looking at the return-type of the initer, if there
	;; is one and it's known
	(let ((type-already (if (pair? initers)
				(first-return-type (car initers))
				(if (null? initers)
				    ;; leftover vars will be set to #f
				    '<boolean>
				    '<obj>))))
	  (loop (cdr vars) 
		(cdr types) 
		(if (pair? initers)
		    (cdr initers)
		    initers)
		(if (or (trust-me-type? (car vars))
			(ct-compatible-type? type-already (car types)))
		    ;; it's already a compatible type
		    checks
		    ;; it needs a type check
		    (cons (gen-assert-type (car vars) (car types))
			  checks)))))))

(define (gen-assert-type var type)
  (type-checked-expr* (make <ic-lex-ref>
			    var: var
			    mode: 'value)
		      type))

(define (mfg-multi-return vars types)
  (make <ic-multi>
	arg-list: (map (lambda (v t)
			 (make <ic-lex-ref>
			       var: v
                               return-types: (list t)))
		       vars
                       types)))

;;


(define (install-type-checks-in-body! self)
  (let ((x (mfg-required-type-checks (bindings (envt self))
				     (map type (bindings (envt self)))
				     #f)))
    (if (not (null? x))
	(let ((new-body (make <ic-seq>
			      stmt-list: (reverse (cons (body self) x)))))
	  ;(format #t "new body: ~s\n" new-body)
	  (set-body! self new-body)))))

(define (install-type-checks-in-inits! self)
  (let* ((e (objs-only-envt (envt self)))
         (tt (map type (bindings (envt self)))) ; target types
	 (x (mfg-required-type-checks (bindings e)
                                      tt
				      (if (instance? (inits self) <ic-multi>)
					  (arg-list (inits self))
					  #f))))
    (if (not (null? x))
	;;
	;; wrap the new code around the initializer
	;;
	(let ((new-initer (make <ic-seq>
				stmt-list: (reverse
					    (cons (mfg-multi-return 
						   (bindings e)
                                                   tt)
						  x)))))
	  ;(format #t "new inits: ~s\n" new-initer)
	  ;; replace our own initializer with a recursive <ic-bind>
	  ;; that does the checking
	  (set-inits! self
		      (make <ic-bind>
			    inits: (inits self)
			    envt: e
			    body: new-initer
			    rest?: (rest? self)))
	  (set-rest?! self #f)))))

;;
;;

(define-method initialize ((self <ic-bind>))
  (set-does-lambda?! self (or (does-lambda? (body self))
			      (and (inits self) 
				   (does-lambda? (inits self)))))
  (set-does-save?! self (or (does-save? (body self))
			    (and (inits self) 
				 (does-save? (inits self)))))
  (set-num-args! self (- (num-bindings (envt self)) 
			 (if (rest? self) 1 0)))
  ;;
  ;; we don't bother to strip out references and captures of our
  ;; own variables because nobody above us will ever care about them
  ;;
  (set-vars-referenced! self (append (if (inits self)
					 (vars-referenced (inits self))
					 '())
				     (vars-referenced (body self))))
  (set-vars-captured! self (append (if (inits self)
				       (vars-captured (inits self))
				       '())
				   (vars-captured (body self))))
  ;;
  (set-return-types! self (return-types (body self)))
  ;;
  ;(format #t "ic-bind: ~j\n" (map car (name->bindings (envt self))))
  ;(format #t "code: ~s\n" (inits self))
  ;;
  ;;  only bother if at least something needs a type check 
  ;;  (ie, is restricted more than an <obj> and isn't a :trust-me)
  ;;
  (if (any? (lambda (v)
	      (and (not (trust-me-type? v))
		   (not (eq? (type v) '<obj>))
		   (not (eq? (type v) <object>))))
	    (bindings (envt self)))
      ;;
      (begin
	;(format #t "----> installing type checks for <ic-bind>\n")
	(if (inits self)
	    (install-type-checks-in-inits! self)
	    (install-type-checks-in-body! self))))
  self)
