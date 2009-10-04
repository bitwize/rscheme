#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/compiler/bdgs.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.25
 | File mod date:    2004-01-12 14:29:56
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  compiler
 |
 | Purpose:          compile-time binding representation operations
 `------------------------------------------------------------------------|#

(define *basis* #f)
(define *basis-generator* #f)

(define (get-compiler-basis)
  (if (not *basis*)
      (set! *basis* (*basis-generator*)))
  *basis*)

(define (set-compiler-basis-generator! (basis <function>))
  (set! *basis-generator* basis)
  (set! *basis* #f))

(define (well-known name)
  ;(format #t "requesting well-known: ~s\n" name)
  (let ((b (lookup (get-compiler-basis) name)))
    (if b
	b
	(error "access denied (~s not bound)" name))))

;; Recognizers

(define (macro? x)
    (instance? x <macro>))

(define (substitution? x)
    (instance? x <substitution>))

;; Implementation

(define-method write-object ((self <binding>) port)
  (format port "#[~a ~s]" (class-name (object-class self)) (name self)))

(define-method write-object ((self <top-level-var>) port)
    (format port "#[tlv ~a]" (name self)))

(define-method write-object ((self <primop>) port)
    (format port "#[primop ~a]" (name self)))

(define-method to-string ((self <binding>))
  (symbol->string (name self)))

;;;

(define (type-for-each-arg args types rest-type)
  (if (null? args)
      '()
      (if (null? types)
	  (cons rest-type 
		(type-for-each-arg (cdr args) '() rest-type))
	  (cons (car types)
		(type-for-each-arg (cdr args) (cdr types) rest-type)))))

(define-method compile-head ((self <primop>) orig form lex-envt dyn-envt mode)
  (let* ((args (compile/multi (cdr form) lex-envt dyn-envt 'value))
	 (n-args (length (arg-list args))))
    (if (rest-type self)
	(if (< n-args (length (arg-types self)))
	    (error/syntax*
	     form
	     "Primop `~a' called with too few args (required at least ~d, got ~d)"
	     (name self)
	     (length (arg-types self))
	     n-args))
	(if (not (eq? n-args (length (arg-types self))))
	    (error/syntax* form
	     "Primop `~a' called with wrong # args (required exactly ~d, got ~d)"
	     (name self)
	     (length (arg-types self))
	     n-args)))
    (make <ic-call-prim>
	  function: self
	  args: (multi-checked-coerce (arg-list args)
				      (arg-types self)
				      (rest-type self))
	  return-types: (if (result-type self)
			    (list (result-type self))
			    '()))))

(define-method compile-ref ((self <primop>) orig lex-envt dyn-envt mode)
  (if (full-procedure-bdg self)
      (make <ic-tl-ref>
	    var: (xform orig 'full-procedure-bdg)
	    mode: mode)
      (error/syntax 
       "Primop `~a' illegally referenced as variable" 
       (name self))))

(define-method compile-set ((self <primop>) orig value-expr lex-envt dyn-envt mode)
    (error/syntax "Primop `~a' targetted in set!" (name self)))

(define-method compile-head ((self <special-form>) orig form lex-envt dyn-envt mode)
    (if (and (syntax-checker self)
	     (not ((syntax-checker self) form lex-envt)))
	(error/syntax "~a form doesn't pass syntax-checker" (name self))
	((special-form-compiler self) self form lex-envt dyn-envt mode)))

(define-method compile-ref ((self <special-form>) orig lex-envt dyn-envt mode)
    (error/syntax "Special form name `~a' referenced as variable" (name self)))

(define-method compile-set ((self <special-form>) 
			    orig value-expr
			    lex-envt
			    dyn-envt
			    mode)
    (error/syntax "Special form name `~a' used in variable position in set!" 
    		  (name self)))


(define-method compile-head ((self <top-level-var>) orig form lex-envt dyn-envt mode)
    (compile/combo form lex-envt dyn-envt mode))

(define-method compile-ref ((self <top-level-var>) orig lex-envt dyn-envt mode)
  ;; special case class and function TLVs 
  (if (const-tlv? self)
      (let* ((v (actual-value (value self)))
	     (vt (cond
		  ((instance? v <<target-class>>) '<<class>>)
		  ((instance? v <target-gf1>)
		   (xform (well-known '<single-dispatch-gf>)
			  'value))
		  ((instance? v <target-function>)
		   '<function>)
		  (else '<obj>))))
	;;(format #t "<tl-ref> ~s => ~s (~s)\n" v vt (actual-value vt))
	(make <ic-tl-ref>
	      var: orig
	      mode: mode
	      return-types: (list vt)))
      (make <ic-tl-ref>
	    var: orig 
	    mode: mode)))

;; write-protection & constant TLVs probably aren't a good
;; idea at the moment (could use <value-of>, but prefer
;; to avoid at the moment, since this feature isn't being used
;; anyway)
;;    (if (write-prot self)
;;	(make-const (value self) mode)

(define-method compile-set ((self <top-level-var>) 
			    orig value-expr lex-envt dyn-envt mode)
    (if (write-prot self)
	(error/syntax "Top-level `~a' in set! is write protected" 
    		  (name self))
	(if (and (pair? value-expr)
		 (null? (cdr value-expr)))
	    (make <ic-tl-set>
		  var: orig
		  rhs: (compile (car value-expr) 
				lex-envt dyn-envt 'value)
		  mode: mode)
	    (error/syntax "`set! ~s' used with wrong number args"
			  (name self)))))


(define-method compile-head ((self <lexical-var>) orig form lex-envt dyn-envt mode)
    (compile/combo form lex-envt dyn-envt mode))

(define-method compile-ref ((self <lexical-var>) orig lex-envt dyn-envt mode)
    (make <ic-lex-ref> var: self 
		       mode: mode
		       return-types: (list (type self))))

(define-method compile-set ((self <lexical-var>) 
			    orig value-expr lex-envt dyn-envt mode)
  (if (and (pair? value-expr)
	   (null? (cdr value-expr)))
      (let ((rhs (compile (car value-expr) lex-envt dyn-envt 'value)))
	(if (trust-me-type? self)
	    (make <ic-lex-set> 
		  var: self
		  rhs: rhs
		  mode: mode)
	    (make <ic-lex-set>
		  var: self
		  rhs: (coerced-expr rhs (type self))
		  mode: mode)))
      (error/syntax "`set! ~s' used with wrong number args" (name self))))

; Note that compiling a macro use (or a substitution, for that matter)
; preserves the mode (ie, even if it's top).  Hence, you can have
; top-level forms INSIDE of macro bodies that are properly interpreted
; at the top level.
; (THIS IS A FEATURE, and indeed is almost the entire purpose to
;  the mode parameter (certainly to the 'top value of mode)!)

(define-method compile-head ((self <macro>) orig form lex-envt dyn-envt mode)
  (let* ((subst-site (make <substitution-site-record>
			   some-info: (list form)
			   lexical-enclosing: lex-envt
			   dynamic-enclosing: dyn-envt))
	 (specific (find-match self (cdr form) subst-site subst-site)))
    (if specific
	(let ((outer-place *place*))
	  (thread-let ((*place* (if (memq (name self) outer-place)
                                    outer-place
                                    (cons (name self) outer-place))))
	    (compile/begin self
			   (cons 'begin (body (car specific)))
			   (cdr specific)
			   (cdr specific);;dyn-envt
			   mode)))
	(if (else-bdg self)
	    (compile-head (else-bdg self)
			  (else-bdg self)
			  form
			  lex-envt
			  dyn-envt
			  mode)
	    (error/syntax "Use of macro ~a does not match defn" 
			  (car form))))))

(define-method full-procedure-bdg ((self <macro>))
  (if (instance? (else-bdg self) <top-level-var>)
      (else-bdg self)
      (if (and (instance? (else-bdg self) <substitution>)
	       (symbol? (expr (else-bdg self)))
	       (instance? (lookup (envt (else-bdg self)) 
				  (expr (else-bdg self)))
			  <top-level-var>))
	  (lookup (envt (else-bdg self))
		  (expr (else-bdg self)))
	  (error "Macro `~s' doesn't have a regular `else' binding"
		 (name self)))))

(define-method compile-ref ((self <macro>) orig lex-envt dyn-envt mode)
  (if (else-bdg self)
      (compile-ref (else-bdg self) (else-bdg self) lex-envt dyn-envt mode)
      (error/syntax "Macro name `~a' referenced as variable" (name self))))

(define-method compile-set ((self <macro>) 
			    orig value-expr
			    lex-envt
			    dyn-envt
			    mode)
  (if (null? (setter-forms self))
      (if (else-bdg self)
	  (compile-set (else-bdg self) 
		       (else-bdg self) 
		       value-expr
		       lex-envt
		       dyn-envt
		       mode)
	  (error/syntax "Macro `~a' used in variable position in set!" 
			(name self)))
      (let* ((subst-site (make <substitution-site-record>
			       some-info: (list (list 'set! 
						      (name self)
						      value-expr))
			       lexical-enclosing: lex-envt
			       dynamic-enclosing: dyn-envt))
	     (specific (find-setter-match self 
					  value-expr
					  subst-site
					  subst-site)))
	(if specific
	    (thread-let ((*place* (cons* (name self) 'set! *place*)))
	      (compile/begin self
			     (cons 'begin (body (car specific)))
			     (cdr specific)
			     (cdr specific);;dyn-envt
			     mode))
	    (if (else-bdg self)
		(compile-set (else-bdg self)
			     (else-bdg self)
			     value-expr
			     lex-envt
			     dyn-envt
			     mode)
		(error/syntax "set!: use of macro ~a does not match defn" 
			      (name self)))))))

(define-method compile-ref ((self <substitution>) orig lex-envt dyn-envt mode)
    (compile (expr self) (envt self) dyn-envt mode))

(define-method compile-head ((self <substitution>) orig form 
						   lex-envt dyn-envt mode)
    ; if our expr is a symbol, that symbol may be ct-bound to 
    ; a substitution rather than a variable...
    (let loop ((e (expr self)) (lxe (envt self)))
	(if (symbol? e)
	    (let ((bdg (lookup-aliased e lxe dyn-envt)))

		; random thoughts from space:
		;  if we think of macros as syntactic CLOSURES, is
		;  what we're doing here (threading through substitutions,
		;  etc.) analagous to syntactic CONTINUATIONS?
		;(format #t ">> subst.. ~s is bound to: ~s\n" e bdg)
		(if (substitution? bdg)
		    (loop (expr bdg) (envt bdg))
		    (if (or (macro? (actual-bdg bdg))
			    (instance? (actual-bdg bdg) <primop>))
			(compile-head bdg orig form lex-envt dyn-envt mode)
			(make-combo (compile e lxe dyn-envt 'value)
				    (compile/multi (cdr form) 
						   lex-envt 
						   dyn-envt 
						   'value)
				    dyn-envt
				    mode))))
	    (make-combo (compile e lxe dyn-envt 'value)
			(compile/multi (cdr form) 
				       lex-envt 
				       dyn-envt 
				       'value)
			dyn-envt
			mode))))

(define-method compile-set ((self <substitution>) 
			    orig value-expr
			    lex-envt
			    dyn-envt
			    mode)
    ; if our expr is a symbol, that symbol may be ct-bound to 
    ; a substitution rather than a variable...
    (let loop ((e (expr self)) (lxe (envt self)))
	(if (symbol? e)
	    ; if this subst. is bound to a name
	    ; then look up that name and transfer responsibility
	    ; to it (note that we can't just recurse on substitutions,
	    ; because we have to remember the lex. envt of the
	    ; value-expr)
	    (let ((bdg (lookup-aliased e lxe dyn-envt)))
		(if (substitution? bdg)
		    (loop (expr bdg) (envt bdg))
		    (compile-set bdg bdg value-expr lex-envt dyn-envt mode)))
	    (error/syntax 
		"Cannot set! subst. `~a' (replace with `~a')" 
			(name self)
			(expr self)))))

;============================= Loop Variables =============================

(define-method compile-head ((self <loop-var>) 
			     orig form lex-envt dyn-envt mode)
  (if (and (eq? mode 'tail)
           (eq? *place* (in-procedure self)))
      (make <ic-jump> 
	    args: (compile/multi (cdr form) lex-envt dyn-envt 'value)
	    loop-var: self)
      (begin
	#|
	(format #t 
		"Compiling loop `~s' with full semantics (non-tail)\n" 
		(name self))
	|#
	((compile-with-full-semantics self)))))

;; if a loop variable is ever referenced or set (or called from a non-tail
;; position)
;; then we jump back to compile it with full semantics

(define-method compile-ref ((self <loop-var>) orig lex-envt dyn-envt mode)
  ;(format #t "Compiling loop `~s' with full semantics (refd)\n" (name self))
  ((compile-with-full-semantics self)))

(define-method compile-set ((self <loop-var>) 
			    orig value-expr lex-envt dyn-envt mode)
  ;(format #t "Compiling loop `~s' with full semantics (set!)\n" (name self))
  ((compile-with-full-semantics self)))

