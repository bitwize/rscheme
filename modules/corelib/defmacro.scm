#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/corelib/defmacro.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.5
 | File mod date:    2002-11-13 07:58:11
 | System build:     v0.7.3.4-b7u, 2007-05-30
 |
 `------------------------------------------------------------------------|#

;;;
;;;  three forms:
;;;
;;;     (define-macro name (macro-rules () ((_ . pattern) expr) ...))
;;;     (define-macro name (lamdba args expr))
;;;     (define-macro (name . pattern) expr)
;;;

(define-rewriter (define-macro form)
  ;;
  (define (special-identifier? n)
    (or (keyword? n)
	(flag? n)))
  ;;
  (define (pattern-vars pattern lits)
    (cond
     ((special-identifier? pattern)
      '())
     ((memq pattern lits)
      '())
     ((symbol? pattern)
      (list pattern))
     ((pair? pattern)
      (cond
       ((eq? (car pattern) '#rest)
	(pattern-vars (cadr pattern) lits))
       ((eq? (car pattern) '#key)
	(map (lambda (k)
	       (if (pair? k)
		   (car k)
		   k))
	     (cdr pattern)))
       (else
	(append (pattern-vars (car pattern) lits)
		(pattern-vars (cdr pattern) lits)))))
     (else
      '())))
  ;;


  (define (def-macro-rules name mrules)
    (let* ((literals (cadr mrules))
	   (tests (map (rcurry def-macro-case literals) (cddr mrules))))
      `(define-rewriter (,name form)
	 ;;
	 ;; these utility functions wind up getting copied 
	 ;; into every `define-macro' macro
	 ;;
	 (define (kvlist->vector kv-list)
	   (let ((q (make-dequeue)))
	     (let loop ((p kv-list))
	       (if (pair? p)
		   (if (and (keyword? (car p))
			    (pair? (cdr p)))
		       (begin
			 (dequeue-push-back! q (car p))
			 (dequeue-push-back! q (cadr p))
			 (loop (cddr p)))
		       (error "malformed keyword list: ~s" kv-list))
		   (if (null? p)
		       (dequeue-state q)
		       (error "malformed keyword list: ~s" kv-list))))))
	 ;;
	 (define (using-keyword-value kwd (v <vector>) 
				      found-proc
				      notfound-proc)
	   (let ((i (vassq kwd v)))
	     (if (fixnum? i)
		 (let ((a (vector-ref v i)))
		   (let loop ((i i))
		     (if (fixnum? i)
			 (begin
			   (vector-set! v (- i 1) #f)
			   (loop (vassq kwd v)))
			 (found-proc a))))
		 (notfound-proc))))
	 ;;
	 (define (get-keyword-value (kvv <vector>) keyword default)
	   (using-keyword-value
	    keyword
	    kvv
	    (lambda (item)
	      item)
	    (lambda ()
	      default)))
	 ;;
	 (define (match-keys keywords actual ok fail envt)
	   (let ((kvv (kvlist->vector actual)))
	     (let loop ((e envt)
			(klist keywords))
	       loop ;; turn off opt
	       (cond
		((null? klist)
		 (ok e))
		((pair? (car klist))
		 (let ((v (get-keyword-value
			   kvv 
			   (symbol->keyword (caar klist))
			   (cadar klist))))
		   (loop (cons (cons (caar klist) v) e) (cdr klist))))
		(else
		 (let ((k (symbol->keyword (car klist)) kvv))
		   (using-keyword-value
		    k 
		    kvv 
		    (lambda (item)
		      (loop (cons (cons (car klist) item) e) (cdr klist)))
		    fail)))))))
	 ;
	  (define (macro-match pattern actual ok fail envt)
	    (cond
	     ((or (keyword? pattern) 
		  (flag? pattern)
		  (memq pattern ',literals))
	      (if (eq? actual pattern)
		  (ok envt)
		  (fail)))
	     ((symbol? pattern)
	      (ok (cons (cons pattern actual) envt)))
	     ((pair? pattern)
	      (cond
	       ((eq? (car pattern) '#rest)
		(macro-match (cadr pattern) actual ok fail envt))
	       ((eq? (car pattern) '#key)
		(match-keys (cdr pattern) actual ok fail envt))
	       ((pair? actual)
		(macro-match (car pattern) 
			     (car actual) 
			     (lambda (e)
			       (macro-match (cdr pattern) (cdr actual) ok fail e))
			     fail
			     envt))
	       (else
		(fail))))
	     (else
	      (if (equal? actual pattern)
		  (ok envt)
		  (fail)))))
	  (let loop ((cases (list ,@tests)))
	    (if (null? cases)
		(error "~s: macro mismatch on ~s" ',name form)
		(let ((r (macro-match (caar cases) (cdr form) identity (lambda () #f) '())))
		  (if r
		      (apply (cdar cases) (map cdr (reverse r)))
		      (loop (cdr cases)))))))))

  (define (def-macro-case mrule lits)
    (let* ((pat (cdar mrule))
	   (body (cdr mrule))
	   (vars (pattern-vars pat lits)))
      `(cons ',pat (lambda (,@vars) ,@body))))
  
  (if (pair? (cadr form))
      (def-macro-rules 
	(caadr form)
	`(macro-rules () ((_ ,@(cdadr form)) ,@(cddr form))))
      (case (caaddr form)
        ((macro-rules)
         (def-macro-rules 
           (cadr form)
           (caddr form)))
        ((lambda)
         (let ((name (cadr form)))
           `(define-rewriter (,name form)
              (let ((procedure ,(caddr form)))
                (apply procedure (cdr form)))))))))
            
