#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/compiler/sforms.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.22
 | File mod date:    2005-01-20 19:58:21
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  compiler
 |
 | Purpose:          Compiler's implementation of standard special forms
 `------------------------------------------------------------------------|#

;; Standard Special Forms
;; ----------------------

(define (compile/values sf form lex-envt dyn-envt mode)
    (compile/multi (cdr form) lex-envt dyn-envt mode))

;; BEGIN is a little strange because the Scheme standard
;; defines that (begin (define ...)) is the same as (define ...)
;; so we have to process the contents of a top-level begin
;; at the top level
;;  (actually, this property has been moved to compile/body,
;;   so that let-syntax and letrec-syntax can partake of its
;;   strangeness)
;;
;; Other compound constructs, when compiled at the top-level,
;; regard their tail part as being in 'tail position (using the untop fn)
;; (although they continue to regard themselves as being at top)

(define (compile/begin sf form lex-envt dyn-envt mode)
  (compile/body (cdr form) lex-envt dyn-envt mode))
	
(define (compile/let sf form lex-envt dyn-envt mode)
    (if (symbol? (cadr form))
	(compile/named-let sf form lex-envt dyn-envt mode)
	(compile/let-bdg sf form lex-envt dyn-envt mode)))

(define (map-car lst)
  (map (lambda (x) (car x)) lst))

(define (compile/named-let* form lex-envt dyn-envt mode)
  (let ((loop-vars (map-car (caddr form)))
	(loop-inits (map cadr (caddr form)))
	(loop-body (cdddr form))
	(loop-name (cadr form))
	(loop-binding (specifier->lex-var (cadr form)
					  lex-envt
					  dyn-envt)))
    (let ((new-envt (make-lexical-envt  (list loop-binding)
					lex-envt
					dyn-envt)))
      (make-letrec new-envt
		   (list (make <ic-lambda> 
			       mode: 'value
			       code-properties: (list (cons 
						       'formals-min-args
						       (length loop-vars)))
			       proc: (compile/procedure
				      loop-name
				      loop-vars
				      loop-body
				      new-envt
				      new-envt)))
		   (make <ic-call> 
			 function: (make <ic-lex-ref> 
					 var: loop-binding 
					 mode: 'value)
			 args: (compile/multi loop-inits 
					      lex-envt 
					      dyn-envt 
					      'value)
			 mode: mode)
		   mode))))


(define (compile/named-let sf form lex-envt dyn-envt mode)
  (let ((ic (call-with-current-continuation
	     (lambda (exit)
	       (let* ((loop-var (make <loop-var> 
				      name: (cadr form)
				      label: #f
				      loop-icode: #f
                                      in-procedure: *place*
				      compile-with-full-semantics: 
				      (lambda ()
					(exit #f))))
		      (loopv-envt (make-lexical-envt (list loop-var)
						     lex-envt
						     dyn-envt))
		      (loop-args (caddr form))
		      (arg-envt (make-lexical-envt 
				 (make-lex-vars 
				  (map-car loop-args) 
				  loopv-envt
				  loopv-envt) 
				 loopv-envt 
				 loopv-envt))
		      (ic (make <ic-loop>
				mode: mode
				envt: arg-envt
				loop-var: loop-var
				inits: (compile/multi 
					(map cadr loop-args) 
					loopv-envt
					loopv-envt
					'value)
				body: (compile/body 
				       (cdddr form) 
				       arg-envt 
				       arg-envt 
				       ;; I think this should
				       ;; be 'tail (at least for calls to `loop'
				       ;; but maybe not for other forms
				       (untop mode)))))
		 (set-loop-icode! loop-var ic)
		 ic)))))
    (if ic
	;; it was successful as an optimized loop
	ic
	;; it failed as an optimized loop.. compile it
	;; as a loop with full semantics
	(compile/named-let* form lex-envt dyn-envt mode))))
	
(define (make-letrec new-envt inits body mode)
  (define (make-set var val)
    (make <ic-lex-set> var: var
	  rhs: val
	  mode: 'effect))
  (make <ic-bind>
	mode: mode
	envt: new-envt
	rest?: #f
	inits: (make <ic-multi> 
		     arg-list: (map (lambda (x)
				      (make-const '#uninit 'value))
				    inits)
		     mode: 'value)
	body: (make <ic-seq>
		    stmt-list: 
		    (list (make <ic-seq> stmt-list:(map make-set
							(bindings new-envt)
							inits))
			  body))))


(define (compile/letrec sf form lex-envt dyn-envt mode)
    (let ((new-envt (make-lexical-envt 
    				(make-lex-vars (map-car (cadr form)) 
					       lex-envt
					       dyn-envt) 
				lex-envt 
				dyn-envt)))
	(make-letrec new-envt
		     (map (lambda (bdg-info)
			    (compile (cadr bdg-info) new-envt new-envt 'value))
			  (cadr form))
		     (compile/body (cddr form) 
				    new-envt 
				    new-envt 
				    (untop mode))
		     mode)))

(define (compile/let-bdg sf form lex-envt dyn-envt mode)
    (let ((new-envt (make-lexical-envt 
    				(make-lex-vars 
				 (map-car (cadr form))
				 lex-envt
				 dyn-envt) 
				lex-envt 
				dyn-envt)))
	(make <ic-bind>
	    mode: mode
	    envt: new-envt
	    rest?: #f
	    inits: (compile/multi (map cadr (cadr form)) 
				    lex-envt
				    dyn-envt
				    'value)
	    body: (compile/body (cddr form) 
	    			new-envt 
				new-envt 
				(untop mode)))))

;; Dylan's version of "let" (but with the additional ability to
;; have a #rest and arbitrary function for the initializer)
;;
;; usage something like:
;;
;;     (bind ((a b c (values 1 2 3))
;;            (x y #rest z (list->values *a-bunch*)))
;;        ...)

(define (all-but-last l)
  (if (pair? l)
      (if (pair? (cdr l))
	  (cons (car l) (all-but-last (cdr l)))
	  '())
      l))
 
(define (compile/bind sf form lex-envt dyn-envt mode)

    (define (compile-binds binds body lex-envt dyn-envt)
	(if (pair? binds)
	    ;; "binds" is a list of binding forms
	    ;; each binding form is like:
	    ;;    (a b #rest c init-expr)
	    (let* ((bdg-form (all-but-last (car binds)))
		   (init-expr (last (car binds)))
		   (new-envt (make-envt bdg-form lex-envt dyn-envt)))
		(compile-one-bind init-expr
				  (compute-has-rest bdg-form)
				  lex-envt
				  dyn-envt
				  new-envt
				  (compile-binds (cdr binds)
				  		 body 
						 new-envt 
						 new-envt)))
	    (compile/body body lex-envt dyn-envt (untop mode))))
    
    (define (make-envt formals lex-envt dyn-envt)
	(make-lexical-envt (make-lex-vars formals lex-envt dyn-envt)
			    lex-envt 
			    dyn-envt))
    
    (define (compile-one-bind init-expr rest? lex-envt dyn-envt new-envt body)
	(make <ic-bind>
	    mode: mode
	    envt: new-envt
	    inits: (compile init-expr lex-envt dyn-envt 'bind)
	    body: body
	    rest?: rest?))
    
    (compile-binds (cadr form) (cddr form) lex-envt dyn-envt))


(define (compile/let-syntax sf form lex-envt dyn-envt mode)
  (let ((new-envt (make-lexical-envt
		   (map 
		    (lambda (x) 
		      (compile-macro (car x) (cdr x) lex-envt))
		    (cadr form))
		   lex-envt
		   dyn-envt)))
    (compile/body (cddr form)
		  new-envt 
		  new-envt
		  mode)))

(define (compile/letrec-syntax sf form lex-envt dyn-envt mode)
    (let ((new-envt (make-lexrec-envt
		     (lambda (new-envt)
		       (map 
			(lambda (x) 
			  (compile-macro (car x) (cdr x) new-envt))
			(cadr form)))
		     lex-envt
		     dyn-envt)))
      (compile/body
       (cddr form) 
       new-envt 
       new-envt
       mode)))


(define (compile/set! sf form lex-envt dyn-envt mode)
  (let ((bdg (find (cadr form) lex-envt dyn-envt)))
    (compile-set bdg bdg (cddr form) lex-envt dyn-envt mode)))
	
(define (compile/quote sf form lex-envt dyn-envt mode)
    (if (eq? (length form) 2)
	(make-const (cadr form) mode)
	(error/syntax "Badly constructed: ~a" form)))

(define (compile-point-stack dyn-envt)
  (let ((t (current-location-table)))
    (map (lambda (x)
           (let ((o (owner x)))
             (list (and t (table-lookup t o)) o)))
         (select (lambda (x)
                   (instance? x <compile-point>))
                 (dynamic-enclosing-chain dyn-envt)))))

;; find the closest entry on the stack for which we have
;; a line number

(define (compile-point-file-and-line dyn-envt)
  (let ((t (current-location-table)))
    (if t
        (let loop ((d dyn-envt))
          (if (instance? d <top-level-contour>)
              #f ; XXX should know something about where the module came from!
              (if (and (instance? d <compile-point>)
                       (table-lookup t (owner d)))
                  (reverse (table-lookup t (owner d)))
                  (loop (dynamic-enclosing d)))))
        #f)))

(define (compile-FUNCTION sf form lex-envt dyn-envt mode)
  ;;
  (define (get-file-and-line)
    (let ((p (current-source-point)))    ; this is w.r.t. the top level form
      (if p
          (case (vector-ref p 0)
            ((file) (values (vector-ref p 1)
                            (vector-ref p 2)))
            ((repl) (values "repl"
                            (vector-ref p 1))))
          (values "?" 1))))
  ;;
  (let ((v (if (pair? (cdr form))
               (case (cadr form)
                 ((file)
                  (bind ((f l (get-file-and-line)))
                    f))
                 ((line)
                  (bind ((f l (get-file-and-line)))
                    l))
                 ((file-line)
                  (bind ((f l (get-file-and-line)))
                    (list f l)))
                 ((%stack)              ; for test/debug of compiler
                  (compile-point-stack dyn-envt))
                 ((form)
                  (compile-point-file-and-line dyn-envt))
                 (else
                  (error/syntax "Unknown *FUNCTION* qualifier: ~s" (cadr form))))
               (current-place-name))))
    ;;
    (make-const v mode)))

(define (compile/mquote sf form lex-envt dyn-envt mode)
  ;;
  (define (compile-mquoted mquoted-expr lex-envt)
    (if (symbol? mquoted-expr)
	(let ((bdg (lookup-aliased mquoted-expr lex-envt dyn-envt)))
	  (if (substitution? bdg)
	      (compile-mquoted
	       (expr bdg)
	       (envt bdg))
	      (make-const mquoted-expr mode)))
	(make-const mquoted-expr mode)))
  ;;
  (if (eq? (length form) 2)
      (compile-mquoted (cadr form) lex-envt)
      (error/syntax "Badly constructed: ~a" form)))


(define (compile/lambda sf form lex-envt dyn-envt mode)
  (bind ((name formals body (if (and (pair? (cadr form))
				     (eq? (caadr form) 'quote))
				(values (cadadr form)
					(caddr form)
					(cdddr form))
				(values (next-lambda-name)
					(cadr form)
					(cddr form)))))
    (make <ic-lambda>
	  mode: mode
	  code-properties: (append
			    (if (compute-has-rest formals)
				'((formals-has-rest? . #t))
				'())
                            (append-source-property
                             (list (list 'function-scope name)
                                   (cons 'formals-min-args
                                         (compute-min-args formals))
                                   (cons 'inline
                                         (vector formals
                                                 body
                                                 lex-envt
                                                 name)))
                             dyn-envt))
	  proc: (compile/procedure name formals body lex-envt dyn-envt))))

(define (compile/if sf form lex-envt dyn-envt mode)
  (make <ic-if>
	mode: mode
	condition: (compile (cadr form) lex-envt dyn-envt 'value)
	if-true: (compile (caddr form) lex-envt dyn-envt (untop mode))
	if-false: (if (null? (cdddr form))
		      (make-const '#undef (untop mode))
		      (compile (cadddr form) lex-envt dyn-envt (untop mode)))))

(define (compile/bdg sf form lex-envt dyn-envt mode)
  ;;
  (define (resolv exp env)
    (let ((b (lookup-aliased exp env dyn-envt)))
      (if b
	  (if (substitution? b)
	      (resolv (expr b) (envt b))
	      b)
	  #f)))
  ;;
  (let ((b (resolv (cadr form) lex-envt)))
    (if b
	(make <ic-const> value: b)
	(error/semantic "~s: not bound" (cadr form)))))

(define (make-special-forms)
  (let-syntax ((sf (syntax-form (n)
		     (make <special-form>
			   name: (mquote n)
			   compiler-proc: #f
			   compiler-description: (mquote n)))))
    (list
     (sf begin)
     (sf quote)
     (sf mquote)
     (sf quasiquote)
     (sf if)
     (sf let)
     (sf letrec)
     (sf bind)
     (sf let-syntax)
     (sf letrec-syntax)
     (sf set!)
     (sf %values)
     (sf *FUNCTION*)
     (sf well-known-function)
     (sf with-module)
     (sf lambda)
     (sf if-implements)
     (sf cond-expand)
     (sf %slot-index)
     (sf &))))

(define (core-compiler->proc description)
  (case description
    ((begin) compile/begin)
    ((quote) compile/quote)
    ((mquote) compile/mquote)
    ((quasiquote) compile/quasiquote)
    ((if) compile/if)
    ((let) compile/let)
    ((letrec) compile/letrec)
    ((bind) compile/bind)
    ((let-syntax) compile/let-syntax)
    ((letrec-syntax) compile/letrec-syntax)
    ((set!) compile/set!)
    ((%values) compile/values)
    ((*FUNCTION*) compile-FUNCTION)
    ((well-known-function) compile/well-known-function)
    ((lambda) compile/lambda)
    ((with-module) compile/with-module)
    ((if-implements) compile/if-implements)
    ((cond-expand) compile/cond-expand)
    ((&) compile/bdg)
    (else #f)))

(define *special-form-compilers* '())

(define (add-special-form-compiler! proc)
  (set! *special-form-compilers*
	(cons proc *special-form-compilers*)))

(%early-once-only
  (add-special-form-compiler! core-compiler->proc))

(define (special-form-compiler->proc description)
  (let loop ((i *special-form-compilers*))
    (if (null? i)
	(error "special-form description `~s' unknown" description)
	(let ((p ((car i) description)))
	  (if p
	      p
	      (loop (cdr i)))))))

;;;
;;;  compile a body with the given name bound to an ad-hoc
;;;  special form which invokes the given procedure
;;;

(define (compile-with-ad-hoc-sf (content <list>)
				(m-esc <symbol>)
				(esc-proc <function>)
				top-required?
				lxe 
				dye
				mode)
  (if (and top-required? (not (eq? mode 'top)))
      (error "compile-with-ad-hoc-sf: top required but not AT top"))
  (let* ((cproc (if top-required?
		    (lambda (self form lxe dye mode)
		      (if (eq? mode 'top)
			  (esc-proc (cdr form) lxe dye mode)
			  (error
			   "~s: escape not at top level"
			   m-esc)))
		    (lambda (self form lxe dye mode)
		      (esc-proc (cdr form) lxe dye mode))))
	 (esc-sf (make <special-form>
                       name: m-esc
                       compiler-description: 'ad-hoc-sf
                       compiler-proc: cproc))
         (e2 (make <lexical-contour>
                   name->bindings: (list (cons m-esc esc-sf))
                   bindings: (list esc-sf)
                   lexical-enclosing: lxe
                   dynamic-enclosing: dye)))
    (compile/body content e2 e2 'top)))
