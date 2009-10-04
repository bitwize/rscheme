#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/compiler/mainc.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.19
 | File mod date:    2003-11-05 19:26:33
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  compiler
 |
 | Purpose:          Main compiler entry point
 `------------------------------------------------------------------------|#

;; mode is one of:
;;	'tail	value is to be returned (ie, expr is in tail position)
;;	'value	value is needed, but not in tail position
;;	'effect	value is not needed
;;	'top 	top-level form

(define-class <compile-point> (<scope-record>)
  dynamic-enclosing
  owner)

(define (compile expr lexical-envt dynamic-envt mode)
  (compile-expr expr lexical-envt 
                (make <compile-point>
                      dynamic-enclosing: dynamic-envt
                      owner: expr)
                mode))

;;
;;

(define (untop mode)
  (if (eq? mode 'top)
      'tail
      mode))

(define-generic-function self-evaluating?)
(define-generic-function compile-expr)

(define-method compile-expr ((expr <object>) lex-envt dyn-envt mode)
  (if (self-evaluating? expr) 
      (make-const expr mode)
      (error/syntax* expr "Unrecognized expression form")))

(define-method compile-expr ((expr <symbol>) lex-envt dyn-envt mode)
  (if (or (keyword? expr)
	  (flag? expr))
      (make-const expr mode)
      (compile/symbol expr lex-envt dyn-envt mode)))

(define-method compile-expr ((expr <pair>) lex-envt dyn-envt mode)
  (compile/list expr lex-envt dyn-envt mode))

(define-method self-evaluating? ((self <boolean>))         #t)
(define-method self-evaluating? ((self <number>))          #t)
(define-method self-evaluating? ((self <string>))          #t)
(define-method self-evaluating? ((self <curly-braced>))    #t)
(define-method self-evaluating? ((self <char>))            #t)

(define-method self-evaluating? ((self <object>))          #f)

;; a list of tlv's that should be checked at some point...

(define *unbound-vars-created* '())

;; like `lookup-aliased', but creates an #unbound TLV if necessary

(define (find name lex-envt dyn-envt)
  (let ((bdg-info (lookup-aliased name lex-envt dyn-envt)))
    (if bdg-info
	bdg-info
	(let ((v (make <top-level-var> 
		       value: '#unbound
		       name: name)))
	  (set! *unbound-vars-created*
		(cons (cons v (current-place-name))
		      *unbound-vars-created*))
	  (bind! (the-top-level lex-envt) v)
	  v))))

(define (warn-about-unbound-vars-created)
  (if (not (null? *unbound-vars-created*))
      (begin
	(for-each (lambda (info)
		    (let ((v (car info))
			  (place (cdr info)))
		      (if (eq? (value v) '#unbound)
			  (warning "~s references unbound variable ~s"
				   place
				   (name v)))))
		  (reverse *unbound-vars-created*))
	(set! *unbound-vars-created* '()))))

(define (compile/list expr lex-envt dyn-envt mode)
    (if (symbol? (car expr))
	(let ((b (find (car expr) lex-envt dyn-envt)))
	  (compile-head b
			b
			expr 
			lex-envt 
			dyn-envt 
		      mode))
	(compile/combo expr lex-envt dyn-envt mode)))

;; This function compiles a form that is known to be a
;; combination.  It optimizes ((lambda (formals) body) args)
;; into (let (formals args) body)

(define (compile/combo form lex-envt dyn-envt mode)
  (let ((fn (compile (car form) lex-envt dyn-envt 'value))
	(args (compile/multi (cdr form) lex-envt dyn-envt 'value)))
    (make-combo fn args dyn-envt mode)))

(define (make-combo (fn <expr-icode>)
		    (args <expr-icode>)
		    dyn-envt
		    mode)
  (if (and (eq? (object-class fn) <ic-lambda>)
	   (assq 'inline (code-properties fn)))
      ;; re-compile the LAMBDA as a LET
      ;; (we have to recompile, because the body was
      ;;  originally compiled in TAIL mode, which it
      ;;  will no longer be in)
      (let ((inline-info (cdr (assq 'inline (code-properties fn)))))
	(let ((formals (vector-ref inline-info 0))
	      (body (vector-ref inline-info 1))
	      (lex-envt (vector-ref inline-info 2))
	      (name (vector-ref inline-info 3)))
	  (let ((new-envt (make-lexical-envt 
			   (make-lex-vars formals lex-envt dyn-envt)
			   lex-envt 
			   dyn-envt)))
	    (make <ic-bind>
		  envt: new-envt
		  inits: args
		  rest?: (compute-has-rest formals)
		  body: (compile/body body 
				      new-envt 
				      new-envt 
				      (untop mode))))))
      (or (inlined-version fn args)
	  ;; otherwise, make it a normal call
	  (make <ic-call> 
		function: fn
		args: args
		mode: mode))))


(define (compile/multi items lex-envt dyn-envt mode)
  (let loop ((i items) (r '()))
    (cond 
     ((symbol? i) (let ((binding (lookup-aliased i lex-envt dyn-envt)))
		    (if (substitution? binding)
			(begin
			  (set! lex-envt (envt binding))
			  (loop (expr binding) r))
			(error/syntax
			 "Rest symbol ~a not a substitution" i))))
     ((null? i) (let ((args (reverse r)))
		  (make <ic-multi>
			arg-list: args
			mode: mode)))
     ((pair? i) (loop (cdr i)
		      (cons (compile (car i) lex-envt dyn-envt 'value)
			    r)))
     (#t (error/syntax "Illegal args form: ~a" items)))))

(define (make-no-values mode)
  (make <ic-multi> 
	arg-list: '() 
	mode: mode))

;(define-thread-var *place* #f)
(define-thread-var *procedure-name* 'anon-lambda)
(define-thread-var *num-lambdas* 0)

(define (current-procedure-name)
  *procedure-name*)

(define (current-place-name)
  (or *place* (list (current-procedure-name))))

(define (next-lambda-name)
  (let ((n *num-lambdas*)
	(pn *procedure-name*))
    (set! *num-lambdas* (+ n 1))
    (cons n
          (if (pair? pn)
              pn
              (cons pn '())))))
