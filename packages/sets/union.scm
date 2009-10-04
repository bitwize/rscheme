#|------------------------------------------------------------*-Scheme-*--|
 | File:    packages/sets/union.scm
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
 | Purpose:          Dylan union operation
 `------------------------------------------------------------------------|#

(define-syntax (union-operator new-table insert-op! hash-op)
  (lambda sets
    (let ((tbl new-table))
      (let sets-loop ((sets sets))
	(if (null? sets)
	    (table-keys->list tbl)
	    (let (((seq <sequence>) (car sets)))
	      (let loop ((s (initial-state seq)))
		(if s
		    (let ((k (current-element seq s)))
		      (insert-op! tbl (hash-op k) k #t)
		      (loop (next-state seq s)))
		    (sets-loop (cdr sets))))))))))

(define unionq
  (union-operator
   (make-object-table)
   object-table-insert!
   transient->hash))

(define unionstr
  (union-operator
   (make-string-table)
   string-table-insert!
   string->hash))


(define (union-test a b test)
  (cond
   ((eq? test eq?)
    (unionq a b))
   ((eq? test string=?)
    (unionstr a b))
   (else
    (let ((in (value-sequence a)))
      (let loop ((s (initial-state b))
		 (in in)
		 (already? (member?-tester in test)))
	(if s
	    (if (already? (current-element b s))
		(loop (next-state b s) in already?)
		(let ((in (cons (current-element b s) in)))
		  (loop (next-state b s) 
			in
			(member?-tester in test))))
	    in))))))

(define (union a b . opt)
  (union-test a b (opt-test-fn opt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

