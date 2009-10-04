#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/repl/userinit.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.29
 | File mod date:    2005-04-11 15:07:23
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  repl
 |
 | Purpose:          Construct the user-initial environment
 `------------------------------------------------------------------------|#

(define (make-module . opt-name)
  (let* ((e (make-top-level-contour))
	 (m (make <module>
		  name: (if (pair? opt-name) (car opt-name) 'anon)
		  top-level-envt: e)))
    (set-owner! e m)
    (values m e)))

#|------------------------------------------------------------------------|#

(define (make-r4rs-module)
  (bind ((m pedantic-top-level (make-module 'r4rs)))
    (let ((src (table (top-level-envt *basic-top-level*)))
	  (dst (table pedantic-top-level)))
      (set-module-exports! m dst) ;; export everything
      ;; 
      ;;  mark target envt as dirty
      ;;
      (set-dirty?! pedantic-top-level #t)
      ;;
      ;; install the pedantic special forms
      ;;
      (vector-for-each 
       (lambda (n)
	 (table-insert! dst n (table-lookup src n)))
       $R4RS-specials)
      ;;
      ;; install the pedantic top-level bindings
      ;;
      (vector-for-each 
       (lambda (n)
	 (let ((b (table-lookup src n)))
	   (if b
	       (table-insert! dst
			      n
			      (make <top-level-var>
				    name: n
				    value: (pedantic-value b)
				    write-prot: #f))
	       (format #t "pedantic: R4RS feature not supported: ~s\n" n))))
       $R4RS-variables)
      ;;
      ;; install the special-cased bindings
      ;;
      (table-insert! dst 'load (make-load-bdg pedantic-top-level))
      ;;
      m)))

(define (pedantic-value bdg)
  (if (or (instance? bdg <macro>)
	  (instance? bdg <primop>))
      (let ((full (full-procedure-bdg bdg)))
	(if full
	    (value full)
	    (begin
	      (format #t
		      "pedantic: ~s does not have a full procedure bdg\n"
		      (name bdg))
	      #f)))
      (value bdg)))
	    
(define $R4RS-specials '#(and begin case cond define delay do
			     if lambda let let* letrec or quasiquote
			     quote set!))

(define $R4RS-variables '#(* + - / < <= = > >=
			    abs acos append apply asin
			    assoc assq assv atan boolean?
			    car cdr
			    caar cadr cddr cdar caaar caadr caddr 
			    cadar cdaar cdadr cdddr cddar caaaar caaadr 
			    caaddr caadar cadaar cadadr cadddr caddar
			    cdaaar cdaadr cdaddr cdadar cddaar cddadr 
			    cddddr cdddar
			    call-with-current-continuation
			    call-with-input-file call-with-output-file
			    ceiling char->integer
			    char-alphabetic? char-ci<=? char-ci<?
			    char-ci=? char-ci>=? char-ci>? char-downcase
			    char-lower-case? char-numeric? char-ready?
			    char-upcase char-upper-case? char-whitespace?
			    char<=? char<? char=? char>=? char>? char?
			    close-input-port close-output-port complex?
			    cons cos current-input-port current-output-port
			    display eof-object? eq? equal? eqv? even?
			    exact->inexact exact? exp expt floor for-each
			    force gcd inexact->exact inexact? input-port?
			    integer->char integer? lcm length list
			    list->string list->vector list-ref list-tail
			    list? #|load|# log make-string make-vector map
			    max member memq memv min modulo negative?
			    newline not null? number->string number?
			    odd? open-input-file open-output-file
			    output-port? pair? peek-char positive? 
			    procedure? quotient rational? read read-char
			    real? remainder reverse round set-car!
			    set-cdr! sin sqrt string string->list
			    string->number string->symbol string-append
			    string-ci<=? string-ci<? string-ci=?
			    string-ci>=? string-ci>? string-copy
			    string-fill! string-length string-ref
			    string-set! string<=? string<? string=?
			    string>=? string>? string? substring
			    symbol->string symbol? tan transcript-off
			    transcript-on truncate vector vector->list
			    vector-fill! vector-length vector-ref
			    vector-set! vector? with-input-from-file
			    with-output-to-file write write-char zero?))

#|------------------------------------------------------------------------|#

(define (add-eval-bindings! (self <top-level-contour>))
  (bind! self (make <top-level-var>
                    name: '*self*
                    value: self))
  (bind! self (make <top-level-var>
                    name: 'eval
                    value: (lambda 'eval (s-expr . rest)
                             (if (null? rest)
                                 (eval-in-envt s-expr self)
                                 (eval-in-envt s-expr (car rest))))))
  (bind! self (make-load-bdg self)))

(define (make-load-bdg envt)
  (make <top-level-var>
	name: 'load
	value: (lambda args
		 (apply load-into envt args))
	write-prot: #t))

(define (make-user-initial)
  (bind ((m e (make-module 'user-initial)))
    (module-uses-module m '*basic* *basic-top-level*)
    ;; export everything except the eval bindings
    (set-module-exports! m (hash-table-copy (table e)))
    ;;
    (add-eval-bindings! e)
    (values e m)))

(define *basic-top-level* #f)


(define (make-scheme-module)
  (bind ((m e (make-module 'rscheme)))
    ;;
    (multi-bind! e (make-special-forms))
    (multi-bind! e (make-definer-forms))
    (multi-bind! e (make-objsys-forms))
    (multi-bind! e (make-hackerly-forms))
    ;;
    (set-module-exports! 
     m
     (let ((t (make-symbol-table)))
       (for-each
	(lambda (n)
	  (let ((b (lookup e n)))
	    (if (not b)
		(format #t "==> missing: ~s\n" n))
	    (table-insert! t n 
			   (or b 
			       (make <special-form>
				     compiler-proc: #f
				     compiler-description: '*not-available*
				     name: n)))))
	*scheme-module-fns*)
       t))
    ;;
    m))

(define *scheme-module-fns* 
  '(define-primop well-known-function lambda mquote
     define-generic-function && quasiquote %early-once-only values
     define-method *FUNCTION* define letrec-syntax bind
     define-rewriter %strategy quote define-class let-syntax letrec
     let & define-glue set! begin make define-full-bdg define-syntax
     if with-module define-constant
     define-module define-module-extend module
     if-implements cond-expand %slot-index))

(define (init-user-initial-modules)
  (install-module! '*scheme* (make-scheme-module))
  (install-module! '*basic* (make-basic-user-module))
  (set! *basic-top-level* (get-module '*basic*))
  (bind ((e m (make-user-initial)))
    (install-module! 'usual-inlines m))
  (bind ((e m (make-user-initial)))
    (install-module! 'user (enforce-module-safety! m)))
  (install-module! 'r4rs (make-r4rs-module)))

(define (module-uses-module (target <module>)
			    (used-name <symbol>)
			    (used <module>))
  (use-module-in used-name used (top-level-envt target)))

(define (make-basic-user-module)
  (bind ((m e (make-module 'basic-user)))
    ;;
    (for-each (lambda (u)
		(format #t "basic top level includes: ~s\n" u)
		(module-uses-module m u (get-module u)))
	      '(*scheme*
		;; primops has to come *before* corelib, because
		;; corelib defines new versions of some primops
		;; (new versions that have full procedure bindings)
		primops 
		;;
		corelib
		iolib
		low-scheme
		high-scheme
		mathlib))
    ;;
    (let ((compiler-basis (copy-top-level-contour e)))
      (use-in 'objsys compiler-basis)
      (set-compiler-basis-generator! (lambda () compiler-basis)))
    ;;
    ;(bind! e (& initialize))
    (bind! e (& restart-with))
    (bind! e (make <special-form>
		   name: 'exported-value
		   compiler-description: 'exported-value
		   compiler-proc: compile/exported-value))
    (set-module-exports! m (table e))
    m))

(define (enforce-module-safety! (m <module>))
  (let ((copy '())
	(my-table (table (top-level-envt m))))
    ;;
    (table-for-each
     my-table
     (lambda (h k v)
       (cond
	((instance? v <top-level-var>)
	 ;;
	 ;; INCREDIBLY EGREGIOUS HACK!!  don't clone
	 ;; the bindings whose NAMES are of the form *foo*
	 ;; (instead of looking at the module export mode)
	 ;;
	 (let (((n <string>) (symbol->string (name v))))
	   (if (and (not (eq? (string-ref n 0) #\*))
		    (not (eq? (string-ref n (- (string-length n) 1)) #\*)))
	       (set! copy (cons (clone v) copy)))))
	((and (instance? v <primop>)
	      (full-procedure-bdg v))
	 (set! copy (cons (clone (full-procedure-bdg v)) copy))))))
    ;;
    (for-each (lambda (c)
		;(set-write-prot! c #f)
		(table-insert! my-table (name c) c))
	      copy)
    m))

(define (compile/exported-value sf form lex-envt dyn-envt mode)
  (bind ((module name (usage-check form 
				   '(exported-value (module :: <name>) 
						    (name :: <name>))
				   lex-envt)))
    (let ((b (table-lookup (module-exports (get-module module)) name)))
      (if b
	  (if (instance? b <top-level-var>)
	      (make <ic-tl-ref>
		    var: b
		    mode: mode)
	      (error/semantic* form
			       "name ~s from module ~s is not a TLV"
			       name module))
	  (error/semantic* form
			   "name ~s not exported by module ~s"
			   name module)))))

;; NOP

(define (compile-early-once-only sf form lex-envt dyn-envt mode)
  (compile (cons 'begin (cdr form)) lex-envt dyn-envt mode))

(define (make-hackerly-forms)
  (list
   (make <special-form>
	 name: '%early-once-only
	 compiler-description: 'early-once-only
	 compiler-proc: compile-early-once-only)))

(define (compile-early-once-only* form lex-envt dyn-envt)
  (compile (cons 'begin (cdr form)) lex-envt dyn-envt 'top))

(define (compiler-control-sf->proc description)
  (case description
    ((%early-once-only) compile-early-once-only*)
    ;;
    (else #f)))

(%early-once-only (add-special-form-compiler! compiler-control-sf->proc))
