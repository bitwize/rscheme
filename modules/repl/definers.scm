#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/repl/definers.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.17
 | File mod date:    2003-11-05 19:34:36
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  repl
 |
 | Purpose:          Top-level definer forms
 `------------------------------------------------------------------------|#

(define-class <definer> (<special-form>))

(define-method compile-head ((self <definer>) orig tl-expr lex-envt dyn-envt mode)
  (if (not (eq? mode 'top))
      (error/syntax "definer form `~s' not at top level" (name self)))
  (if (and (syntax-checker self)
	   (not ((syntax-checker self) tl-expr lex-envt)))
      (error/syntax "~a form doesn't pass syntax-checker" (name self))
      ((special-form-compiler self) tl-expr lex-envt dyn-envt)))

;; note that these definers return a symbol instead
;; of #f to indicate that the TL should ignore them

;; DEFINERs are special kinds of SPECIAL FORMs, with the
;; property that they create new bindings for top-level
;; variables (or other kinds of objects, like macros)
;; A given binding can only have one definer for it in
;; all of input.  If a binding is not marked write-protected,
;; then it can have it value changed, of course...

(define (car-most form)
    (if (pair? form)
	(car-most (car form))
	form))

(define (install-tl-def name lex-envt dyn-envt bdg)
  (let ((b (lookup-aliased name lex-envt dyn-envt)))
    (if b
	(begin
	  ;; it's already bound.  issue a warning UNLESS
	  ;; its the special case of a top-level-var that
	  ;; had an #unbound binding installed on account of
	  ;; a forward reference
	  (if (eq? (object-class bdg) (object-class b))
	      (if (instance? b <top-level-var>)
		  (begin
		    (if (not (eq? (value b) '#unbound))
			(warning "`~s' is already bound" name))
		    ;; keep the original TLV object, just change its value
		    (set-value! b (value bdg))
		    b)
		  (begin
		    (warning "`~s' is already bound" name)
		    (tl-bind! name lex-envt dyn-envt bdg)
		    bdg))
	      (begin
		(warning "`~s' is already bound as a ~s" 
			 (class-name (object-class b)))
		(tl-bind! name lex-envt dyn-envt bdg)
		bdg)))
	;;
	;; it's not bound yet.. this is the way it should be
	(begin
	  (tl-bind! name lex-envt dyn-envt bdg)
	  bdg))))


(define (compile-tl-define form lex-envt dyn-envt)
  (compile-tl-define-like form lex-envt dyn-envt 'define))

(define (compile-tl-define-const form lex-envt dyn-envt)
  (compile-tl-define-like form lex-envt dyn-envt 'define-const))

(define (compile-tl-define-like form lex-envt dyn-envt head)
  (let ((name (car-most (cdr form))))
    (if (not (symbol? name))
	(error/syntax "~s target not a symbol: ~s" head name))
    (let ((bdg (lookup (the-top-level lex-envt) name)))
      (if (not bdg)
	  (begin
	    (set! bdg (make <top-level-var> 
			    name: name
			    value: '#uninit
			    write-prot: (eq? head 'define-const)))
	    (bind! (the-top-level lex-envt) bdg))
	  (if (instance? bdg <top-level-var>)
	      (if (write-prot bdg)
		  (warning "constant `~s' is being re-defined" name)
		  (if (and (not (eq? (value bdg) '#unbound))
			   (eq? head 'define-const))
		      (warning "`~s' being re-defined as a constant" name)))
	      (begin
		(warning "`~s' being re-defined as a ~s (was a ~s)"
			 name 
			 (if (eq? head 'define-const)
			     "constant"
			     "variable")
			 (class-name (object-class bdg)))
		(set! bdg (make <top-level-var> 
				name: name
				value: '#uninit
				write-prot: (eq? head 'define-const)))
		(bind! (the-top-level lex-envt) bdg))))
      (if (pair? (cadr form))
	  (compile/define-fn bdg (cdr form) lex-envt dyn-envt)
	  (compile/define-var bdg (cdr form) lex-envt dyn-envt))
      name)))

; note that you can say something like
;   (define ((foo bar) x y) ...)
; to make a function of two arguments with a compound
; name.  `bdg' has already been bound to the tlv foo
; but we will compute name as '(foo bar)
;
; (at this point, form starts with the name-arg list)

(define *compile-verbose* #f)
(define *save-source-info* #t)

(define (compile/define-fn bdg form lex-envt dyn-envt)
  (let ((name (caar form))
	(formals (cdar form))
	(body (cdr form)))
    (if *compile-verbose*
	(format #t "compiling function: ~a\n" name))
    (set-value! bdg
		(wrap-tl-proc
		 (append-source-property
                  (list (list 'function-scope name))
                  dyn-envt)
		 (compile/procedure name 
				    formals
				    body
				    lex-envt
				    dyn-envt)))))

(define (compile/define-var bdg form lex-envt dyn-envt)
    (if *compile-verbose*
        (format #t "compiling declaration: ~a\n" (car form)))
    (let ((val (eval-in-envt (cadr form) lex-envt)))
	(set-value! bdg val)))

(define (compile-tl-define-syntax form lex-envt dyn-envt)
  (let ((lhs (cadr form))
	(tl-envt (the-top-level lex-envt)))
    (if (pair? lhs)
	(submit-new-syntax 
	 (make <macro>
	       name: (car lhs)
	       envt: lex-envt
	       else-bdg: #f
	       forms: (list (make <macro-form>
				  args: (cdr lhs)
				  body: (cddr form))))
	 tl-envt)
	(submit-new-syntax (compile-macro lhs (cddr form) lex-envt)
			   tl-envt))))

(define (submit-new-syntax (m <macro>) envt)
  (let ((bdg (lookup envt (name m))))
    (if bdg
	(warning "define-syntax: `~s' is already bound" (name m)))
    (bind! envt m)
    (name m)))

(define (compile-tl-define-alias form lex-envt dyn-envt)
  (let ((name (cadr form))
	(expr (caddr form)))
    (if *compile-verbose*
	(format #t "compiling alias: ~a\n" name))
    (bind! (the-top-level lex-envt)
	   (make <substitution> name: name
		 expr: expr
		 envt: lex-envt))
    name))


(define (compile-tl-define-rewriter form lex-envt dyn-envt)
  (let* ((decl (cadr form))
	 (name (car decl))
	 (formals (cdr decl))
	 (envt (the-top-level lex-envt)))
    (bind! envt
	   (make <local-rewriter>
		 name: name
		 rewriter-args: formals
		 rewriter-lex-envt: lex-envt
		 rewriter-dyn-envt: dyn-envt
		 rewriter-body: (cddr form)))
    name))

(define (top-level-compiler->proc description)
  (case description
    ;;
    ;; definers
    ;;
    ((define) compile-tl-define)
    ((define-constant) compile-tl-define-const)
    ((define-syntax) compile-tl-define-syntax)
    ((define-rewriter) compile-tl-define-rewriter)
    ((define-alias) compile-tl-define-alias)
    ((define-module) compile-tl-define-module)
    ((define-module-extend) compile-tl-module-extend)
    (else #f)))

(%early-once-only (add-special-form-compiler! top-level-compiler->proc))

(define (make-definer-forms)
  (let-syntax ((def (syntax-form (n)
		      (make <definer>
			    name: (mquote n)
			    compiler-proc: #f
			    compiler-description: (mquote n)))))
    (list
     (make <special-form>
	   name: 'module
	   compiler-proc: compile/module
	   compiler-description: 'module)
     (def define)
     (def define-constant)
     (def define-syntax)
     (def define-alias)
     (def define-module)
     (def define-module-extend)
     (def define-rewriter))))
