
(define-class <generalized-var> (<object>) :abstract)

(define-class <plain-gvar> (<generalized-var>)
  var)

(define-method binding->gvar ((self <lexical-var>))
  (make <plain-gvar>
	var: self))

(define-method binding->gvar ((self <top-level-var>))
  (make <plain-gvar>
	var: self))

;;;	

(define (compile-gen-var expr lex-envt dyn-envt)
  (if (symbol? expr)
      (let ((a (lookup-aliased expr lex-envt dyn-envt)))
	(if a
	    (if (substitution? a)
		(compile-gen-var (expr a) (envt a) dyn-envt)
		(binding->gvar a))
	    (error "~s: not bound" expr)))
      

(define (compile/set! sf form lex-envt dyn-envt mode)
  (let ((bdg (compile-gen-var (cadr form) lex-envt dyn-envt)))
    (compile-set bdg bdg (cddr form) lex-envt dyn-envt mode)))

(define-method compile-set ((self <ic-