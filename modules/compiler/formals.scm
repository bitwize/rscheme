#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/compiler/formals.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.8
 | File mod date:    1997-11-29 23:10:35
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  compiler
 |
 | Purpose:          formal argument list analysis and parsing
 `------------------------------------------------------------------------|#

(define-syntax (process-formals$ formals-list nil-thunk last-proc middle-proc)
    (let loop ((p formals-list))
	(cond ((null? p) (nil-thunk))
	      ((symbol? p) (last-proc p))
	      ((pair? p)
	       (if (eq? (car p) '#rest)
		   (if (pair? (cdr p))
		       (last-proc (cadr p))
		       (error/syntax "Illegal #rest clause in formals: ~s" p))
		   (middle-proc (car p) (loop (cdr p)))))
	      (else (error/syntax "Badly formatted formals: ~s" 
	      		          formals-list)))))

(define (compute-min-args formals-list)
    (process-formals$ 
	formals-list
	(lambda () 0)
	(lambda (last) 0)
	(lambda (item rest) (add1 rest))))

(define (compute-num-args formals-list)
    (process-formals$ 
	formals-list
	(lambda () 0)
	(lambda (last) 1)
	(lambda (item rest) (add1 rest))))

(define (compute-has-rest formals-list)
    (process-formals$ 
	formals-list
	(lambda () #f)
	(lambda (last) #t)
	(lambda (item rest) rest)))


(define (compute-specializers formals-list lex-envt dyn-envt)
  (process-formals$ 
   formals-list
   (lambda () 
     '())
   (lambda (last)
     (if (pair? last)
	 (parse-type-expr (cadr last) lex-envt dyn-envt)
	 (xform (well-known '<object>) 'value)))
   (lambda (item rest)
     (cons (if (pair? item)
	       (parse-type-expr (cadr item) lex-envt dyn-envt)
	       (xform (well-known '<object>) 'value))
	   rest))))

;; convert a "specifier" into a binding
;; of the given class.  The lex-envt is
;; used to look up type names, if given
;; a specifier is either "name" or "(name type)", for now

;; (this needs to be fixed to handle intentional capturing
;;  of variables.  In particular, if a name is given in the
;;  variable position, and the name is bound to a substitution,
;;  then we descend into the substitution, looking to bottom
;;  out in a name that is not so bound.  Then, an "alias" binding
;;  needs to be inserted into the dynamic chain to record that
;;  a particular name in a particular (captured) lexical environment
;;  really means something completely different.  THEN, when
;;  we do a lookup, after we find the lexical var, we need to
;;  look up the dynamic chain for an aliasing.)

(define (specifier->lex-var spec lex-envt dyn-envt)
  (cond 
   ((list? spec)
    (let ((var-name (car spec))
	  (type (if (> (length spec) 1)
		    (parse-type-expr (cadr spec) lex-envt dyn-envt)
		    '<obj>))
	  (optional (if (> (length spec) 2)
			(caddr spec)
			#f)))
      (if (and optional
	       (not (eq? optional ':trust-me)))
	  (error/syntax "Variable '~s' optional specifier '~s' is invalid\n   (only ':trust-me' is recognized" optional))
      (make <lexical-var> 
	    name: var-name
	    trust-me-type?: (eq? optional ':trust-me)
	    type: type)))
   ((symbol? spec)
    (make <lexical-var> 
	  name: spec
	  type: '<obj>))
   (else
    (error/syntax "~s: Unrecognized variable specifier form\n   (must be 'name'or '(name type [:trust-me])'" spec))))

;; formals is a list of formal variables in CANONICAL format
;;   formals ::= ( specifier ... )
;;               ( specifier ... #rest specifier )  <== future
;;               ( specifier ... . name )
;;   specifier ::= name
;;                 | ( name type )

(define (make-lex-vars formals lex-envt dyn-envt)
    (process-formals$
	formals
	(lambda () '())
	(lambda (last) 
	    (list (specifier->lex-var last lex-envt dyn-envt)))
	(lambda (item rest) 
	    (cons
		(specifier->lex-var item lex-envt dyn-envt)
		rest))))
