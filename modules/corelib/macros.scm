#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/corelib/macros.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.11
 | File mod date:    2006-01-28 16:41:05
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  precore
 |
 | Purpose:          standard scheme macros
 `------------------------------------------------------------------------|#

;; 
;;
;;  macros to support standard scheme
;;

(define-syntax cond
  (syntax-form (('else . action))
    (begin . action))
  (syntax-form ((guard) . more-clauses)
    (let ((temp guard)) 
      (if temp 
	  temp 
	  (cond . more-clauses))))
  (syntax-form ((guard '=> proc) . more-clauses)
    (let ((temp guard)) 
      (if temp 
	  (proc temp)
	  (cond . more-clauses))))
  ;; this is a bit of a hack
  (syntax-form ((guard '==> proc) . more-clauses)
    (bind ((first #rest rest guard))
      (if first 
	  (apply proc first rest)
	  (cond . more-clauses))))
  (syntax-form ((guard . actions) . more-clauses)
    (if guard 
	(begin . actions) 
	(cond . more-clauses)))
  (syntax-form ()
    (begin)))

(define-syntax or
  (syntax-form (clause)
    clause)
  (syntax-form (clause . more-clauses)
    (let ((temp clause))
      (if temp
	  temp
	  (or . more-clauses))))
  (syntax-form () #f))

(define-syntax cons*
  (syntax-form (a b)
    (cons a b))
  (syntax-form (a b . more)
    (cons a (cons* b . more))))

(define-syntax and
  (syntax-form (clause1 clause2 . more-clauses)
    (if clause1
	(and clause2 . more-clauses) 
	#f))
  (syntax-form () 
    #t)
  (syntax-form (clause) 
    clause))

(define-syntax case
  (syntax-form (expr . branches)
    (letrec-syntax
	((test-cases
	  (syntax-form (var ((g0 . g1) . actions) . more-cases)
	    (if (test-guards var g0 . g1)
		(begin . actions)
		(test-cases var . more-cases)))
	  (syntax-form (var ('else . actions))
	    (begin . actions))
	  (syntax-form (var)
	    (begin)))
	 (test-guards
	  (syntax-form (var item)
	    (eq? var (mquote item)))
	  (syntax-form (var item . more-items)
	    (if (eq? var (mquote item))
		#t
		(test-guards var . more-items)))
	  (syntax-form (var)
	    #f)))
      (let ((temp expr))
	(test-cases temp . branches)))))

(define-rewriter (do form)
  (let ((bdgs (car (cdr form)))
	(test (car (cdr (cdr form))))
	(body (cdr (cdr (cdr form))))
	(loop-name (gensym)))
    (let ((vars (map car bdgs))
	  (inits (map cadr bdgs))
	  (incs (map (lambda (x) 
		       (if (null? (cdr (cdr x)) )
			   (car x) 
			   (car (cdr (cdr x))) ))
		     bdgs)))
      (list
       'let
       loop-name 
       (map list vars inits)
       (list
	'if
	(car test)
	(cons 'begin (cdr test))
	(list
	 'begin
	 (cons 'begin body)
	 (cons loop-name incs)))))))


(define-rewriter (let* form)
  (let ((bdgs (car (cdr form))))
    (if (null? bdgs)
	(cons 'begin (cdr (cdr form)))
	(list
	 'let
	 (list (car bdgs))
	 (cons
	  'let*
	  (cons (cdr bdgs)
		(cdr (cdr form))))))))
