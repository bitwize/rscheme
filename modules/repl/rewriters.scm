#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/repl/rewriters.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.14
 | File mod date:    2003-11-05 19:29:35
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  repl
 |
 | Purpose:          Rewriter compile-time dispatch
 `------------------------------------------------------------------------|#

;============================= Rewriters =============================

(define *rewriter-cache* #f)
(define *rewriter-envt* #f)

(define (s-expr->type* expr envt)
  (let ((r (return-types (compile expr envt envt 'value))))
    (if (pair? r)
        (if (symbol? (car r))
            (prim-type->class (car r))
            (car r))
        <object>)))

(define (clear-rewriter-envt)
  (set! *rewriter-cache* #f)
  (set! *rewriter-envt* #f))

(define (init-rewriter-envt)
  (set! *rewriter-cache* (make-object-table))
  (set! *rewriter-envt* (make-user-initial))
  (use-in 'objsys *rewriter-envt*)
  (use-in 'tables *rewriter-envt*)
  (use-in 'compiler *rewriter-envt*))

(define (envt-self envt)
  envt)

(define (envt-owner envt)
  (owner (the-top-level envt)))

(define (rewriter-envt-procs)
  (list (list 's-expr->value (lambda (envt expr)
				(eval-in-envt expr envt)))
	(list 's-expr->type (lambda (envt expr)
			      (s-expr->type* expr envt)))
	(list 'bound? envt-bound?)
	(list 'binding envt-binding-of)
	(list 'bound-to-gf? envt-bound-to-gf?)
	(list 'current-environment envt-self)
	(list 'current-environment-owner envt-owner)
	(list 'bound-to-class? envt-bound-to-class?)))

(define (rewriter-envt-syntax-bindings)
  (map (lambda (proc-info)
	 (let ((rewriter-body-proc-name (car proc-info))
	       (actual-procedure (cadr proc-info)))
	   `(,rewriter-body-proc-name
	     (syntax-form args
	       (',actual-procedure $envt . args)))))
       (rewriter-envt-procs)))

(define-method rewriter-lex-envt ((self <rewriter>))
  *rewriter-envt*)

(define-method rewriter-dyn-envt ((self <rewriter>))
  *rewriter-envt*)

(define-method compute-rewriter-proc ((self <rewriter>))
  (wrap-tl-proc
   (list (list 'function-scope 'rewriter (name self)))
   (compile/procedure 
    (name self)
    (append (rewriter-args self) '($envt $dynamic-envt))
    `((let-syntax ,(rewriter-envt-syntax-bindings)
	,@(rewriter-body self)))
    (rewriter-lex-envt self)
    (rewriter-dyn-envt self))))

(define-method compile-head ((self <rewriter>) orig form lex-envt dyn-envt m)
  (if (not *rewriter-cache*)
      (init-rewriter-envt))
  (let ((proc (table-lookup *rewriter-cache* self)))
    (if (not proc)
	(begin
	  (set! proc (compute-rewriter-proc self))
	  (table-insert! *rewriter-cache* self proc)))
    (compile (proc form lex-envt dyn-envt) lex-envt dyn-envt m)))


(define-method compile-ref ((self <rewriter>) orig lex-envt dyn-envt mode)
  (error/syntax "Rewriter form name `~a' referenced as variable" (name self)))

(define-method compile-set ((self <rewriter>)
			    orig value-expr
			    lex-envt
			    dyn-envt
			    mode)
  (error/syntax "Rewriter form name `~a' used in variable position in set!" 
		(name self)))
