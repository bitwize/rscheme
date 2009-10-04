#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/compiler/body.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.5
 | File mod date:    1997-11-29 23:10:35
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  compiler
 |
 | Purpose:          Code to compile "body" forms
 |------------------------------------------------------------------------|
 | Notes:
 |      compile/body is used by many compilation procedures, as
 |      it is used for compiling syntactic "body" constructs, such
 |      as the body of a let or let-syntax
 `------------------------------------------------------------------------|#

(define (compile/body body lex-envt dyn-envt mode)
  (if (eq? mode 'top)
      (compile/top-body body lex-envt dyn-envt mode)
      (compile/non-top-body body lex-envt dyn-envt mode)))

(define (compile/top-body body lex-envt dyn-envt mode)
  (let loop ((stmts body)
	     (r '()))
    (if (null? stmts)
	(cond ((null? r) 
	       (if (eq? mode 'top)
		   #f
		   (make-no-values mode)))
	      ((null? (cdr r)) 
	       (car r))
	      (else
	       (make <ic-seq>
		     return-types: (return-types (car r))
		     stmt-list: (reverse r)
		     mode: mode)))
	(if (symbol? stmts)
	    ;; check for substituion...
	    (let ((bdg (lookup-aliased stmts lex-envt dyn-envt)))
	      (if (substitution? bdg)
		  (begin
		    (set! lex-envt (envt bdg))
		    (loop (expr bdg) r))
		  (error/syntax
		   "Rest symbol ~a not a substitution" stmts)))
	    (let ((ic (compile (car stmts) lex-envt dyn-envt 'top)))
	      (loop (cdr stmts)
		    (if (icode? ic) 
			(cons ic r)
			r)))))))

(define (compile/non-top-body body lex-envt dyn-envt mode)
  ;;
  ;; a helper function to turn a list of define statements
  ;; into a single letrec
  ;; ie,
  ;;   ((define a b) (define c d) (define (x) a))
  ;; ==>
  ;;   (letrec ((a b)
  ;;            (c d)
  ;;            (x (lambda () a)))
  ;;    <stmts>)
  ;;
  (define (rewrite-as-letrec defines stmts)
    (cons* 'letrec
	   (map
	    (lambda (def)
	      (if (pair? (cadr def))
		  ; (define (x y) ...)
		  (list
		   (caadr def)
		   (cons* 'lambda
			  (cdadr def)
			  (cddr def)))
		  ; (define x ...)
		  (cdr def)))
	    defines)
	   stmts))
  ;;
  (define (compile-stmts stmts)
    (let loop ((i stmts) (r '()))
      (if (null? i)
	  (cond 
	   ((null? r) 
	    (make-no-values mode))
	   ((null? (cdr r)) 
	    (car r))
	   (else 
	    (make <ic-seq> 
		  return-types: (return-types (car r))
		  stmt-list: (reverse r)
		  mode: mode)))
	  (if (symbol? i)
	      (let ((bdg (lookup-aliased i lex-envt dyn-envt)))
		(if (substitution? bdg)
		    (begin
		      (set! lex-envt (envt bdg))
		      (loop (expr bdg) r))
		    (error/syntax
		     "Rest symbol ~a not a substitution" i)))
	      (loop (cdr i)
		    (cons (compile (car i) 
				   lex-envt
				   dyn-envt
				   (if (null? (cdr i)) 
				       (untop mode) 
				       'effect))
			  r))))))
  ;;
			    
    ;;  Note -- Due to the way we are doing macros, I think there
    ;;  is something slightly broken:  You can't define a macro
    ;;  which contains a body which accepts internal defines!
    ;;  Solution:  We should search for internal defines AS we process
    ;;  the items in the sequence.  OR, we should make two passes
    ;;  over the sequence, descending into substitutions looking for
    ;;  defines.
    
    ;;  filters out the defines from a sequence and returns
    ;;  a pair of the list of defines and the other statements
    
    (define (filter-internal-defines expr-list)
	(let loop ((define-list '()) 
		    (stmt-list '())
		    (last-stmt #f)
		    (exprs expr-list))
	    (if (pair? exprs)
		(if (and (pair? (car exprs)) (eq? (caar exprs) 'define))
		    (loop (cons (car exprs) define-list)
			    stmt-list
			    last-stmt
			    (cdr exprs))
		    (let ((last (cons (car exprs) '())))
			(if last-stmt
			    (set-cdr! last-stmt last))
			(loop define-list
				(if last-stmt stmt-list last)
				last
				(cdr exprs))))
		(begin
		    (if last-stmt
			(begin
			    (set-cdr! last-stmt exprs)
			    (cons (reverse define-list) stmt-list))
			(cons (reverse define-list) exprs))))))

    (let ((parts (filter-internal-defines body)))
	(if (null? (car parts))
	    (compile-stmts (cdr parts))
	    (compile/letrec #f 
			    (rewrite-as-letrec (car parts) (cdr parts))
			    lex-envt
			    dyn-envt
			    mode))))

