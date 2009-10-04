#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/objsys/methods.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.11
 | File mod date:    1998-08-29 19:45:08
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  objsys
 |
 | Purpose:          Code to finish creating GFs and classes
 `------------------------------------------------------------------------|#

;; class introspection

(define-method finalize-class ((self <<class>>))
  (set-class-precedence-list! self (compute-class-precedence-list self)))

(define-method finalize-generic-function ((gf <single-dispatch-gf>))
  ;(format #t "finalizing generic function: ~s\n" (generic-function-name gf))
  (set-template! gf generic-function-dispatch))

;; note: the method may be specialized on classes
;; which are extern (ie, referred to through patches)

(define-method target-add-method ((gf <single-dispatch-gf>) (method <method>))
  (assert (instance? gf <generic-function>))
  (assert (instance? method <method>))
  ;;
  ;; clear it's cache
  ;;
  (clear-gf-cache! gf)
  ;;
  (let ((methods (generic-function-methods gf)))
    ;; 
    ;; examine the function's specializers to see where to add it
    ;;
    (let-syntax ((insert-before (syntax-form (prev rest)
				  (if prev
				      (set-cdr! prev (cons method rest))
				      (set-generic-function-methods! gf
						       (cons method rest)))
				  method))
		 (method-key-class (syntax-form (m)
				      (car (function-specializers m)))))
      (let loop ((i methods)
		 (prev #f))
	(cond ((null? i)
	       (insert-before prev '()))
	      ((eq? (method-key-class method)
		    (method-key-class (car i)))
	       ; this is a REPLACEMENT of an existing method
	       (set-car! i method)
	       method)
	      ((subclass? (method-key-class method)
				 (method-key-class (car i)))
	       (insert-before prev i))
	      (else (loop (cdr i) i)))))))

;;
;;  `patch-implicit-methods'
;;
;;  this function goes through a list of methods that are being
;;  loaded (as from a module) and installs the appropriate template
;;  pointer.
;;
;;  normal methods already have a template pointer; this function
;;  is used to patch up those that use "well known" templates, such
;;  as getter and setter methods.
;;
;;  (I wonder if this should be handled by using named pointers in
;;   the context of module unpickling...  Would cross-compilation
;;   be a problem?)

(define (patch-implicit-methods (methods <list>) implicit-form)
  (let ((t (case implicit-form
	     ((getter-template) getter-template)
	     ((setter-template) setter-template)
	     ((restricted-setter-template) restricted-setter-template)
	     (else (error "implicit method form `~s' not recognized"
			  implicit-form)))))
    (for-each (lambda ((meth <method>))
		(set-template! meth t))
	      methods)))

