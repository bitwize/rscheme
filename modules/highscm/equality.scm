#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/highscm/equality.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.7
 | File mod date:    1999-01-05 21:04:48
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  high-scheme
 |
 | Purpose:          implementation of equality predicates
 `------------------------------------------------------------------------|#

#|   R4RS  section 6.2   |#

(define-method eqv? ((x <number>) y)
  (if (number? y)
      (= x y)
      #f))

(define-method equal? ((x <vector>) y)
    (if (vector? y)
        (let (((y <vector>) y))
	    (if (eq? (vector-length x)
		     (vector-length y))
		(let loop (((i <fixnum>) (vector-length x)))
		    (if (eq? i 0)
			#t
			(if (equal? (vector-ref x (sub1 i))
				    (vector-ref y (sub1 i)))
			    (loop (sub1 i))
			    #f)))
		#f))
	#f))

(define-method equal? ((x <number>) y)
    (eqv? x y))

(define-method equal? ((x <string>) y)
    (and (string? y)
	 (string=? x y)))

(define-method equal? ((x <pair>) y)
    (and (pair? y)
	 (equal? (car x) (car y))
	 (equal? (cdr x) (cdr y))))

(define (gvec-equal? a b)
  (let ((n (gvec-length a)))
    (if (and (eq? (object-class a) (object-class b))
	     (eq? n (gvec-length b)))
	(let loop ((i 0))
	  (if (eq? i n)
	      #t
	      (if (equal? (gvec-ref a i) (gvec-ref b i))
		  (loop (+ i 1))
		  #f)))
	#f)))
