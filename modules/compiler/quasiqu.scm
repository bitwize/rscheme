#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/compiler/quasiqu.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.7
 | File mod date:    2004-01-12 14:29:56
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  compiler
 |
 | Purpose:          Quasiquote compilation
 `------------------------------------------------------------------------|#

(define (compile/quasiquote sf form lex-envt dyn-envt mode)
  (bind ((body args (qq-main (cadr form))))
    (let ((m (make <macro>
		   name: 'quasiquotation
		   envt: (get-compiler-basis)
		   else-bdg: #f
		   forms: (list (make <macro-form>
				      args: (map car args)
				      body: (list body))))))
      ;;
      (compile-head m 
		    m
		    (cons 'quasiquotation (map cadr args))
		    lex-envt
		    dyn-envt
		    mode))))

;;

(define (qq* item depth)
  (if (and (pair? item)
	   (eq? (car item) 'unquote-splicing)
	   (eq? depth 1)
	   (pair? (cdr item))
	   (null? (cddr item)))
      (values (assign-qq-arg (cadr item)) 'append)
      (bind ((sub head (qq item depth)))
	(if (eq? head 'append)
	    (values (cons 'append sub) 'expr)
	    (values sub head)))))

(define-thread-var *qq-args*)

(define (assign-qq-arg actual)
  (let ((formal (string->symbol (string-append 
				 "arg-"
				 (number->string (length (car *qq-args*)))))))
    (set-car! *qq-args* (cons (list formal actual) (car *qq-args*)))
    formal))

(define (qq-main form)
  (thread-let ((*qq-args* (list '())))
    (bind ((sub head (qq form 1)))
      (values
       (case head
	 ((quote) (list 'quote sub))
	 ((expr) sub)
	 ((append) (cons 'append sub)))
       (reverse (car *qq-args*))))))

;;

(define (qq item depth)
  (cond
   ;;
   ((and (pair? item)
	 (memq (car item) '(unquote quasiquote unquote-splicing))
	 (pair? (cdr item))
	 (null? (cddr item)))
    (qq-special (car item) (cadr item) depth))
   ;;
   ((pair? item)
    (qq-pair item depth))
   ;;
   ((vector? item)
    (qq-vector item depth))
   ;;
   (else
    (values item 'quote))))

(define (qq-pair (item <pair>) depth)
  (bind ((car-sub car-head (qq* (car item) depth))
	 (cdr-sub cdr-head (qq (cdr item) depth)))
    (case cdr-head
      ((append)
       (case car-head
	 ((quote)
	  (values (cons (list 'quote (list car-sub)) cdr-sub) 'append))
	 ((expr)
	  (values (cons (list 'list car-sub) cdr-sub) 'append))
	 ((append)
	  (values (cons car-sub cdr-sub) 'append))))
      ((quote)
       (case car-head
	 ((quote)
	  (values item 'quote))
	 ((expr)
	  (values (list 'cons car-sub (list 'quote cdr-sub)) 'expr))
	 ((append)
	  ;; one optimization:  `(,@x) => x
	  (if (null? cdr-sub)
	      (values car-sub 'expr)
	      (values (cons car-sub (list (list 'quote cdr-sub)))
		      'append)))))
      ((expr)
       (case car-head
	 ((quote)
	  (values (list 'cons (list 'quote car-sub) cdr-sub) 'expr))
	 ((expr)
	  (values (list 'cons car-sub cdr-sub) 'expr))
	 ((append)
	  (values (cons car-sub (list cdr-sub)) 'append))))
      (else
       (error "bad cdr head: ~s" cdr-head)))))

;; special operator

(define (qq-special special content depth)
  (case special
    ((unquote)
     (if (eq? depth 1)
	 (values (assign-qq-arg content) 'expr)
	 (bind ((sub head (qq content (- depth 1))))
	   (case head
	     ((quote) (values (list 'unquote sub) 'quote))
	     ((expr) (values (list 'list ''unquote sub) 'expr))
	     (else (error "bad unquote head: ~s" head))))))
    ((unquote-splicing)
     (if (eq? depth 1)
	 (values (assign-qq-arg content) 'append)
	 (bind ((sub head (qq content (- depth 1))))
	   (case head
	     ((quote) (values (list 'unquote-splicing sub) 'quote))
	     ((expr) (values (list 'list ''unquote-splicing sub) 'expr))
	     (else (error "bad unquote-splicing head: ~s" head))))))
    ((quasiquote) 
     (bind ((sub head (qq content (+ depth 1))))
       (case head
	 ((quote)
	  (values content 'quote))
	 ((expr)
	  (values (list 'list ''quasiquote sub) 'expr))
	 (else (error "bad qq head: ~s" head)))))))

(define (qq-vector (item <vector>) depth)
  (letrec ((all-quoted (lambda (src res)
			 (if (null? src)
			     (values item 'quote)
			     (bind ((sub head (qq* (car src) depth)))
			       (case head
				 ((quote)
				  (all-quoted (cdr src)
					      (cons sub res)))
				 ((expr)
				  (some-exprs 
				   (cdr src)
				   (cons sub
					 (map (lambda (q)
						(list 'quote q))
					      res))))
				 ((append)
				  (some-appends 
				   (cdr src)
				   (cons sub
					 (map (lambda (q)
						(list 'quote (list q)))
					      res)))))))))
	   (some-exprs (lambda (src res)
			 (if (null? src)
			     (values (cons 'vector (reverse res))
				     'expr)
			     (bind ((sub head (qq* (car src) depth)))
			       (case head
				 ((quote)
				  (some-exprs (cdr src)
					      (cons (list 'quote sub) res)))
				 ((expr)
				  (some-exprs (cdr src)
					      (cons sub res)))
				 ((append)
				  (some-appends 
				   (cdr src)
				   (cons sub
					 (map (lambda (xpr)
						(list 'list xpr))
					      res)))))))))
	   (some-appends (lambda (src res)
			   (if (null? src)
			       (values (list 'list->vector
					     (cons 'append (reverse res)))
				       'expr)
			       (bind ((sub head (qq* (car src) depth)))
				 (case head
				   ((quote)
				    (some-appends 
				     (cdr src)
				     (cons (list 'quote (list sub)) res)))
				   ((expr)
				    (some-appends
				     (cdr src)
				     (cons (list 'list sub) res)))
				   ((append)
				    (some-appends
				     (cdr src)
				     (cons sub res)))))))))
    (all-quoted (vector->list item) '())))
