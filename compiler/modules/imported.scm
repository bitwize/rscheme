#|------------------------------------------------------------*-Scheme-*--|
 | File:    compiler/modules/imported.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.4
 | File mod date:    1997-11-29 23:10:27
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  (rsc)
 |
 `------------------------------------------------------------------------|#


(define-method compile-head ((self <imported-binding>)
			     orig
			     form 
			     lex-envt 
			     dyn-envt 
			     mode)
  (compile-head (remote-binding self)
		orig
		form
		lex-envt
		dyn-envt
		mode))

(define-method compile-ref ((self <imported-binding>)
			    orig
			    lex-envt 
			    dyn-envt 
			    mode)
  (compile-ref (remote-binding self)
	       orig
	       lex-envt
	       dyn-envt
	       mode))

(define-method compile-set ((self <imported-binding>) 
			    orig value-expr lex-envt dyn-envt mode)
  (if (ib-writable? self)
      (make <ic-tl-set> 
	    var: orig
	    rhs: (compile-set (remote-binding self)
			      orig value-expr lex-envt dyn-envt 'value)
	    mode: mode)
      (error/syntax "top-level `~a' in set! is imported, not writable"
		    (name self))))

(define (bdg-instance? (b <binding>) (c <<class>>))
  (instance? (actual-bdg b) c))

;;

(define (compile-well-known-function sf form lex-envt dyn-envt mode)
  ;;
  (define (get-key-value keyword default)
    (let ((x (memq keyword (cdr form))))
      (if x
	  (if (pair? (cdr x))
	      (cadr x)
	      (error/syntax "bad keyword layout: ~s" form))
	  default)))
  ;;
  (define (make-envt items)
    (if (null? items)
	'()
	(make-gvec* <binding-envt>
		    (make-envt (cdr items))
		    (map compile-time-const-value (car items)))))
  ;;
  (let ((code-info (get-key-value 'code: #f))
	(literals-info (get-key-value 'literals: '()))
	(properties-info (get-key-value 'properties: '()))
	(envt-info (get-key-value 'envt: '())))
    (if (not code-info)
	(error/semantic "well-known-function: `code:' keyword is required"))
    (if (not (list? literals-info))
	(error/syntax "well-known-function: `literals:' not supplied a list"))
    (if (not (and (list? envt-info)
		  (every? list? envt-info)))
	(error/syntax 
	 "well-known-function: `envt:' not supplied a list of lists"))
    (if (or (not (list? code-info))
	    (not (eq? (length code-info) 3))
	    (not (string? (car code-info)))
	    (not (fixnum? (cadr code-info)))
	    (not (fixnum? (caddr code-info))))
	(error/syntax "well-known-function: `code: ~s' is invalid"
		      code-info))
    (let ((the-literals (map (lambda (expr)
			   (compile expr lex-envt dyn-envt 'value))
			 literals-info))
	  (the-envt (map (lambda (frame)
			       (map (lambda (expr)
				      (compile expr lex-envt dyn-envt 'value))
				    frame))
			     envt-info)))
      (bind ((code-ptr linkage-info (bind-to-code (car code-info)
						  (cadr code-info)
						  (caddr code-info))))
	(if (and (every? (lambda (level)
			   (every? compile-time-const? level))
			 the-envt)
		 (every? compile-time-const? the-literals))
	    (make <ic-const>
		  value: (make <target-closure>
			       template: (make-gvec*
					  <template>
					  code-ptr
					  linkage-info
					  properties-info
					  (map compile-time-const-value
					       the-literals))
			       environment: (make-envt the-envt)))
	    (error/semantic "well-known-function: not constant"))))))
