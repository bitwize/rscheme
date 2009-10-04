#|------------------------------------------------------------*-Scheme-*--|
 | File:    packages/sets/intersect.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |	    See <http://www.rscheme.org/> for the latest info.
 |
 | File version:     1.1
 | File mod date:    1997-10-25 22:11:44
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  sets
 |
 | Purpose:          Dylan intersection operation
 `------------------------------------------------------------------------|#

(define-syntax (intersect-operator new-table insert-op! hash-op lookup-op)
  (lambda sets
    (if (null? sets)
	'()
	(let ((tbl new-table))
	  (let (((seq <sequence>) (car sets)))
	    (let first-loop ((s (initial-state seq)))
	      (if s
		  (let ((e (current-element seq s)))
		    (insert-op! tbl (hash-op e) e (cons 1 '()))
		    (first-loop (next-state seq s)))
		  (let sets-loop ((sets (cdr sets))
				  (num-sets 1))
		    (if (null? sets)
			(let ((result '()))
			  (table-for-each
			   tbl
			   (lambda (h k (v <pair> :trust-me))
			     (if (eq? (car v) num-sets)
				 (set! result (cons k result)))))
			  result)
			(let (((seq <sequence>) (car sets)))
			  (let inloop ((s (initial-state seq)))
			    (if s
				(let* ((e (current-element seq s))
				       (x (lookup-op tbl (hash-op e) e)))
				    (if x
					(let (((x <pair> :trust-me) x))
					  (set-car! x (add1 (car x)))))
				    (inloop (next-state seq s)))
				(sets-loop (cdr sets) 
					   (add1 num-sets))))))))))))))

(define intersectq
  (intersect-operator
   (make-object-table)
   object-table-insert!
   transient->hash
   object-table-lookup))

(define intersectstr
  (intersect-operator
   (make-string-table)
   string-table-insert!
   string->hash
   string-table-lookup))

(define (intersect-test a b test)
  (cond
   ((eq? test eq?)
    (intersectq a b))
   ((eq? test string=?)
    (intersectstr a b))
   (else
    (let ((ok? (member?-tester (value-sequence a) test)))
      (let loop ((s (initial-state b))
		 (r '()))
	(if s
	    (let ((e (current-element b s)))
	      (loop (next-state b s) 
		    (if (ok? e)
			(cons e r)
			r)))
	    r))))))

(define (intersection a b . opt)
  (intersect-test a b (opt-test-fn opt)))
