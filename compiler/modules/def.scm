#|------------------------------------------------------------*-Scheme-*--|
 | File:    compiler/modules/def.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.7
 | File mod date:    1997-11-29 23:10:28
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  (rsc)
 |
 `------------------------------------------------------------------------|#


(define *tl-report* #f)

;; compile a top-level "define" construct

(define (compile-tl-define tl-def tl-envt dyn-envt)
  (compile-tl-define* tl-def tl-envt dyn-envt #f))

(define (compile-tl-define-const tl-def tl-envt dyn-envt)
  (compile-tl-define* tl-def tl-envt dyn-envt #t))

(define (compile-tl-define* tl-def tl-envt dyn-envt const?)
  (let ((lhs (cadr tl-def)))
    (if (pair? lhs)
	;; it's a function definition, compile the procedure body
	;; and store it in the appropriate TLV
	;; NOTE that the semantics here are slightly different:
	;; we store the value in the <tlv> at compile time.  A program
	;; that says (define x 10) (define (x) 9) when loaded
	;; will get x that has a procedure value but when compiled
	;; will get x that has the value 10
	;; (actually, that's not entirely true -- we store ct consts
	;; at compile time.  rewrite above as (define x (+ 9 1)) and
	;; the statement is true)
	(compile-tl-fn-def tl-def tl-envt dyn-envt const?)
	;; it's a variable definition, so turn it into a "set!"
	(if (symbol? lhs)
	    (compile-tl-var-def lhs (caddr tl-def) tl-envt dyn-envt const?)
	    (error/syntax "illegal define target: ~s\n" lhs)))))

(define (compile-tl-var-def var expr tl-envt dyn-envt const?)
    (let* ((tlv (ensure-writable-tlv var tl-envt))
	   (icode (compile expr tl-envt tl-envt 'value)))
      (if (compile-time-const? icode)
	  ;; the RHS is a compile-time constant,
	  ;; so do the assignment at compile time (ie, now)
	  (begin
	    (if *tl-report*
		(format #t "compiling top-level var ~s = ~s\n"
			var
			(compile-time-const-value icode)))
	    (set-value! tlv (compile-time-const-value icode))
	    (if const?
		(set-write-prot! tlv #t))
	    ;; no initialization code in this case
	    #f)
	  ;; otherwise, make an init-thunk that will compute
	  ;; and store the answer at load time
	  (begin
	    (if *tl-report*
		(format #t "compiling top-level var ~s := ~s\n" var icode))
	    (set-value! tlv '#uninit) ;; bound but not initialized
	    (if const?
		(set-write-prot! tlv #t))
	    (make <ic-tl-set>
		  var: tlv
		  rhs: icode)))))

(define (compile-tl-fn-def tl-def tl-envt dyn-envt const?)
  (let ((name (caadr tl-def))
	(formals (cdadr tl-def))
	(body (cddr tl-def))
	(tlv #f))
    (if *tl-report*
	(format #t "compiling top-level fn ~s\n" name))
    (set! tlv (ensure-writable-tlv name tl-envt))
    (let* ((cc (make-code-ctx (list (list 'function-scope name))))
	   (ic (compile/procedure name formals body tl-envt tl-envt))
	   (asm (procedure->aml ic '() cc)))
      (set-value! tlv (make <target-closure> 
			    environment: '() 
			    template: (gen-template asm cc)))
      (if (or const? (fn-def-are-const? *build-context*))
	  (set-write-prot! tlv #t))
      #f))) ;; #f => no initialization stuff
