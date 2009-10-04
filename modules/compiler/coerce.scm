#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/compiler/coerce.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.11
 | File mod date:    2003-12-15 09:32:50
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  compiler
 |
 | Purpose:          standard & built-in coercions
 `------------------------------------------------------------------------|#

;; the main function is "coerce-expr" 
;; which takes an AML expr and coerces
;; it to be of a particular primitive type
;; (note that AML exprs record their own type
;;  in their car)
;;
;; e.g.,
;;    (coerce '(<double-float> (ref (reg 0))) '<raw-float>)
;;
;; the high-level system is responsible for filling in
;; sufficiently restrictive primitive types, and for generating
;; the code where appropriate to ensure that such claims
;; are true.


(define $raw-true '(<raw-bool> raw-bool #t))
(define $raw-false '(<raw-bool> raw-bool #f))

(define (obj-expr aml-expr)
  (let ((t (lookup-prim-type (car aml-expr))))
    (if (primtype-raw? t)
        (coerce-aml aml-expr (preferred-class t))
        aml-expr)))

;;  returns the $raw-true or $raw-false object if the value
;;  is a compile-time #t/#f constant

(define (raw-bool-expr aml-expr)
  (coerce-aml aml-expr
	      '<raw-bool>))

;;
(%early-once-only (primtype-setup))
;;

(define (strip-coercions aml-expr)
  (if (and (eq? (cadr aml-expr) 'primop)
	   (primop-does-coercion (name (actual-value (caddr aml-expr)))))
      (strip-coercions (cadddr aml-expr))
      aml-expr))

(define (coerce-aml aml-expr to-type)
  (coerce-basic-aml (strip-coercions aml-expr) to-type))

(define (coerce-basic-aml aml-expr to-type)
  (let ((c (prim-conversion (lookup-prim-type (car aml-expr))
			    (lookup-prim-type to-type))))
    (if c
	(let loop ((expr aml-expr)
		   (src (reverse c)))
	  (if (null? src)
	      (if (or (eq? expr $raw-true)
		      (eq? expr $raw-false))
		  expr
		  (cons to-type (cdr expr)))
	      (case (car src)
		((true) (loop $raw-true (cdr src)))
		((false) (loop $raw-false (cdr src)))
		(else
		 (let ((conversion-primop (well-known (car src))))
		   ;(format #t "primop: ~s\n" conversion-primop)
		   ;(format #t "\t~s\n" (actual-bdg conversion-primop))
		   (if (instance? (actual-bdg conversion-primop) <primop>)
		       (loop (list (result-type (actual-bdg conversion-primop))
				   'primop
				   conversion-primop
				   expr)
			     (cdr src))
		       (error/internal 
			"coercion operator `~s' is not a primop\nexpr: ~s\nto: ~s"
			(car src)
			aml-expr
			to-type)))))))
	(error/internal "impossible coercions: ~s ==> ~s\nfor: ~s"
			(car aml-expr)
			to-type
			aml-expr))))

(define (coerce-aml-list aml-exprs required-types rest-type)
  (let loop ((tcx '())
	     (exprs aml-exprs)
	     (types required-types))
    (if (null? exprs)
	;; we've run out of exprs -- if we've run out of required
	;; types too, then we're happy
	(if (null? types)
	    (reverse tcx)
	    ;; otherwise, they didn't give us enough
	    ;; (this should have been caught much earlier, BTW)
	    (error/internal "uncaught incompatible arg list (i)"))
	;; there are more exprs
	(if (null? types)
	    ;; but we've run out of types -- if there is a "rest" type,
	    ;; then keep going, with it.
	    (if rest-type
		(loop (cons (coerce-aml (car exprs) rest-type) tcx)
		      (cdr exprs)
		      '())
		;; otherwise, they gave us too much
		(error/internal "uncaught incompatible arg list (ii)"))
	    ;; we haven't run out of types... keep going
	    (loop (cons (coerce-aml (car exprs) (car types)) tcx)
		  (cdr exprs)
		  (cdr types))))))

