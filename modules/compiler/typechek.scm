#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/compiler/typechek.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.15
 | File mod date:    1999-01-21 18:32:15
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  compiler
 |
 | Purpose:          code to install type checks
 `------------------------------------------------------------------------|#

;;  generate icode that checks the ic expr for
;;  type conformance and raises a runtime exception
;;  if not (emit warnings for tests that will always
;;  fail at runtime?)
;;
;;  required-type is either a <<target-class>>,
;;  a <patch> which resolves to one,
;;  or a <symbol> naming a primtype

;; in the current implementation, there are only
;; two cases:  either the given icode is already
;; of the required type, in which case nothing
;; is done, or a full type check is required
;;

;;---------------------------------------------------------------------

;; returns an <ic-multi> that is type-checked and coerced
;; (basically, the input can be anything in the world, and
;; the result will be one of:
;;    (1) a compile-time error if the exprs and result aren't conformal
;;    (2) an <ic-multi> with conformal result-types, with or without
;;        appropriate type checks installed

(define (multi-checked-coerce ic-exprs required-types rest-type)
  (let loop ((tcx '())
	     (exprs ic-exprs)
	     (types required-types))
    (if (null? exprs)
	;; we've run out of exprs -- if we've run out of required
	;; types too, then we're happy
	(if (null? types)
	    (make <ic-multi>
		  arg-list: (reverse tcx)
		  return-types: (map first-return-type (reverse tcx))
		  mode: 'value)
	    ;; otherwise, they didn't give us enough
	    (error/syntax 
	     "(1) incompatible argument list\ngiven ~d args, required ~a ~d"
	     (length ic-exprs)
	     (if rest-type
		 "at least"
		 "exactly")
	     (length required-types)))
	;; there are more exprs
	(if (null? types)
	    ;; but we've run out of types -- if there is a "rest" type,
	    ;; then keep going, with it.
	    (if rest-type
		(loop (cons (coerced-expr (car exprs) rest-type) tcx)
		      (cdr exprs)
		      '())
		;; otherwise, they gave us too much
		(error/syntax 
		 "(2) incompatible argument list\ngiven ~d args, required exactly ~d"
		 (length ic-exprs)
		 (length required-types)))
	    ;; we haven't run out of types... keep going
	    (loop (cons (coerced-expr (car exprs) (car types)) tcx)
		  (cdr exprs)
		  (cdr types))))))

;;;

(define (wrap-in-conversions (conversions <list>) 
			     (expr <expr-icode>))
  (let loop ((expr expr)
	     (src (reverse conversions)))
    (if (null? src)
	expr
	(let ((conversion-primop (well-known (car src))))
	  (if (instance? (actual-bdg conversion-primop) <primop>)
	      (loop (make <ic-call-prim>
			  function: conversion-primop
			  args: (make <ic-multi>
				      arg-list: (list expr)
				      return-types: (return-types expr))
			  return-types: (list (result-type 
					       (actual-bdg
						conversion-primop))))
		    (cdr src))
	      (error/internal 
	       "coercion operator `~s' is not a primop\nexpr: ~s"
	       (car src)
	       expr))))))

;;;

(define (coerced-expr (expr <expr-icode>) required-type)
  (let* ((link-type-already (first-return-type expr))
	 (type-already (actual-value link-type-already)))
    (if (ct-compatible-type? type-already (actual-value required-type))
	;; it's already a compatible type, but it may need some coercing
	;;
	;; check for needed conversions
	;; BIG NOTICE:  this only works for both-types-are-classes
	;; because you classes with primtypes are sealed (can't be
	;; subclassed)
	;;
	(let ((req-prim (ct-type->prim-type-name required-type))
	      (is-prim (ct-type->prim-type-name type-already)))
	  (wrap-in-conversions
	   (prim-conversion (lookup-prim-type is-prim)
			    (lookup-prim-type req-prim))
	   expr))
	(type-checked-expr* expr required-type))))


;;; this is only called if the expr's type isn't already compatible 
;;; with the required type
;;;
;;;  this current algorithm is to convert the required type into it's
;;;  corresponding (most specific) primtype.  If that primtype is <obj>,
;;;  then we know there is no useful prim checker we can use and a
;;;  general check is done (using the `check-instance' primop)
;;;
;;;  On the other hand, if the corresponding primtype an exact match
;;;  for the required type, then we use the corresponding prim checker.
;;;
;;;  Otherwise, we have a situation where the required type is more
;;;  restricted but still a subtype of the corresponding primtype.  For
;;;  example, if #{<foo>} is a subclass of #{<function>}, and the required 
;;;  type is #{<foo>}, then the primtype will still be <function>, but
;;;  `check-function' will be insufficient to check for the required type
;;;  and `check-instance' must be used.
;;;
;;;  

(define (type-checked-expr* (expr <expr-icode>) required-type)
  (let ((checker-primop (checker-primop-for-type required-type)))
    ;;
    (coerced-expr
     (if checker-primop
	 ;; there is a checker primop, so use it directly
	 (make <ic-call-prim>
	       mode: 'value
	       function: (well-known checker-primop)
	       args: (make <ic-multi>
			   arg-list: (list expr)
			   mode: 'value)
	       return-types: (list required-type))
	 ;; there isn't one, so use `check-instance'
	 (make <ic-call-prim>
	       function: (well-known 'check-instance)
	       args: (make <ic-multi>
			   arg-list: (list expr 
					   (make <ic-const>
						 value: required-type
						 mode: 'value
						 return-types: '(<<class>>))))
	       mode: 'value
	       return-types: (list required-type)))
     required-type)))

;;; assists in the computation of a type-restricted expression by
;;; determining the name of the checker primop to use to constrict
;;; the type.
;;;
;;; operates by converting the required-type into a corresponding primtype
;;; and then back again.  If the resulting classes are subclasses of each
;;; other, then the primtype sufficiently characterizes the given 
;;; required-type.  Otherwise, the primtype is effectively useless (the
;;; primtype effectively partially determines the required type, but not
;;; completely and therefore not sufficiently)

(define (checker-primop-for-type required-type)
  ;; short-circuit all that work if we are requiring a primtype to start with
  (if (symbol? required-type)
      (or (primtype->checker-primop required-type)
	  (checker-primop-for-type (prim-type->class required-type)))
      (let* ((pt (ct-type->prim-type-name required-type))
	     (c (actual-value (prim-type->class pt))))
	(if (and (target-subclass? (actual-value required-type) c)
		 (target-subclass? c (actual-value required-type)))
	    (primtype->checker-primop pt)
	    #f))))

;;;
;;;  hack the translation in by hand...
;;;

(define (primtype->checker-primop pt)
  (let ((a (assq pt '((<ptr> check-ptr)
		      (<immob> check-immob)
		      (<fixnum> check-fixnum)
		      (<gvec> check-gvec)
		      (<bvec> check-bvec)
		      (<pair> check-pair)
		      (<symbol> check-symbol)
		      (<vector> check-vector)
		      (<function> check-function)
		      (<<class>> check-class)
		      (<string> check-string)
		      (<double-float> check-double-float)
		      (<long-int> check-long-int)
		      (<ascii-char> check-ascii-char)
		      (<unicode-char> check-unicode-char)
		      (<boolean> check-boolean)))))
    (if a
	(cadr a)
	#f)))

;;;

(define (class-recognizer c)
  #f)  ;; not implemented yet

;; returns #t if value-type is compatible with req-type
;; (but not necessarily in the same representation, but
;;  we are guaranteed that a coercion exists)

(define (ct-compatible-type? value-type req-type)
  (cond
   ;------------------------------------------------------------
   ((and (symbol? value-type)
         (symbol? req-type))
    ; do the check in primtype domain
    (prim-compatible-type? (lookup-prim-type value-type)
                           (lookup-prim-type req-type)))
   ;------------------------------------------------------------
   ((symbol? value-type)
    ; do the check in target-class domain, converting the value
    ; type to the closest (l.u.b.) approximation in that domain
    (target-subclass? (actual-value (prim-type->class value-type))
                      (actual-value req-type)))
   ;------------------------------------------------------------
   ((symbol? req-type)
    ; do the check in primtype domain, converting the value
    ; type to the closest approximating primtype
    (prim-compatible-type? (lookup-prim-type
                            (target-class->prim-type-name
                             (actual-value value-type)))
                           (lookup-prim-type req-type)))
   ;------------------------------------------------------------
   (else
    ; both types are target classes, do the check in class domain
    (target-subclass? (actual-value value-type)
                      (actual-value req-type)))))

;; note: either of the classes in question may
;; be themselves or may have superclasses that are
;; external to the current compilation unit (and
;; hence are referred to via patchs.  That's why
;; we have to wrap the reference in `actual-value')

(define (target-subclass? test-class ref-class)
  (if (eq? test-class ref-class)
      #t
      (if (null? (superclasses (actual-value test-class)))
	  #f
	  (target-subclass? (actual-value (car (superclasses 
						(actual-value test-class))))
			    ref-class))))

;;  argument-lists and return types are described by
;;  "pass-type" structures, which encode what the
;;  required types are and whether or not more values
;;  may be present
;;
;;  hence, the pass-type structure on return from
;;  a random call is nothing required and more values possibly present
;;  (ie: () #t)
;;
;;  the pass-type representing the arguments to a
;;  a function with decl  ((x <pair>) y #rest z)
;;  is: (<pair> <object>) #t
;;
;;  each <expr-icode> node has a pass-type associated with it,
;;  which described the data returned by the icode

#|
;******* test stuff.... *****

(define ex (make-top-level-envt))

(define ic1 (compile '(cons 1 2) ex ex 'value))
|#

(define (parse-type-expr texpr lex-envt dyn-envt)
  (let ((b (lookup-aliased texpr lex-envt dyn-envt)))
    (if b
        (if (substitution? b)
            (parse-type-expr (expr b) (envt b) dyn-envt)
	    (if (class-tlv? b)
	        ;; if b is an imported-binding, this will create a (possibly) 
	        ;; new <patch> that represents the VALUE OF the imported bdg
	        (xform b 'value)
	        (error/syntax 
	         "type expr: ~s is not bound to a class tlv (bound to ~s)" 
	         texpr
	         b)))
	(error/syntax "type expr: ~s is not bound" texpr))))
