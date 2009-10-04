#|------------------------------------------------------------*-Scheme-*--|
 | File:	    modules/corelib/delegate.scm
 |
 |          Copyright (C)1998 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |	    See <http://www.rscheme.org/> for the latest info.
 |
 | File version:     1.1
 | File mod date:    1998-12-02 10:56:47
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  corelib
 |
 | Purpose:          `define-delegation' convenience macro
 `------------------------------------------------------------------------|#

;;;
;;;  `define-delegate' defines a collection of methods which
;;;  invoke the same method with the same arguments on some other
;;;  object.
;;;
;;;  This makes it convenient to build wrapper classes that invoke
;;;  an underlying object for the methods of the protocol
;;;
;;;
;;;  Usage:
;;;
;;;    (define-delegation ((SELF TYPE) DELEGATE-EXPR) 
;;;      METHOD-SIGS ...)
;;;
;;;  where:
;;;
;;;    SELF is the name used to refer to the receiver; typically just `self'
;;;    TYPE is the class of SELF, which is the one doing delegation
;;;    DELEGATE-EXPR is an expression for computing the delegate;
;;;     it may involve SELF or any argument that is common to all
;;;     methods
;;;
;;;    METHOD-SIGS are signatures for methods to be delegated.
;;;
;;;      METHOD-SIG ::=  (METHOD-NAME METHOD-ARG ...)
;;;
;;;    exactly one METHOD-ARG should be just SELF -- in the delegating
;;;    wrapper method, this argument will be replaced with DELEGATE-EXPR.
;;;    In the method declaration itself, this will be the restriction
;;;    (SELF TYPE).
;;;
;;;  Example:
;;;
;;;    (define-class <my-input-port> (<input-port>)
;;;      (really type: <input-port>))
;;;
;;;    (define-delegation ((self <my-input-port>) (really self))
;;;      (input-port-read-char self)
;;;      (input-port-peek-char self)
;;;      (input-port-read-line self)
;;;      (input-port-read self)
;;;      (input-port-scan-token self)
;;;      (close-input-port self))
;;;

(define-macro (define-delegation ((self type) delegate-expr) . method-sigs)
  (let ((methods (map 
		  (lambda (method-sig)
		    (let* ((method-name (car method-sig))
			   (method-sig-args (cdr method-sig))
			   (method-args (map 
					 (lambda (sig-arg)
					   (if (eq? sig-arg self)
					       `(,sig-arg ,type)
					       sig-arg))
					 method-sig-args))
			   (delegation-args (map
					     (lambda (sig-arg)
					       (if (eq? sig-arg self)
						   delegate-expr
						   (if (pair? sig-arg)
						       (car sig-arg)
						       sig-arg)))
					     method-sig-args)))
		      `(define-method ,method-name (,@method-args)
			 (,method-name ,@delegation-args))))
		  method-sigs)))
    `(begin ,@methods)))
