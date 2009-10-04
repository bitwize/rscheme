#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/mathlib/numthy.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.7
 | File mod date:    1998-12-28 10:26:31
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  mathlib
 |
 | Purpose:          number theory functions (lcm, gcd, ...)
 `------------------------------------------------------------------------|#

(define-syntax (remainder* chop a b)
  (- a (* b (chop (/ a b)))))

(define (remainder a b)
  (base-remainder a b))

(define (modulo a b)
  (base-modulo a b))
  
(define (quotient a b)
  (base-quotient a b))

;;  The old gcd and lcm algorithms
;;  which work only for non-negative fixnums

; from Algorithms, 2nd. Ed.
;      Sedgewick
;      p. 8

(define (raw-gcd u v)
  (if (< v u)
      (raw-gcd v u)
      (let loop ((u u) (v v))
	(if (eq? u 0)
	    v
	    (loop (remainder v u) u)))))

(define (raw-lcm x y)
  (let ((gcd (raw-gcd x y)))
    (if (eq? gcd 0)
	0
	(quotient (* x y) gcd))))

;; The generalized gcd and lcm algorithms
;; Date: Sat, 04 Jun 1994 21:13:15 -0500
;; From: Jun Sawada <sawada@cs.utexas.edu>

(define (gcd . args) 
  (cond ((not (pair? args)) 0)
	((not (pair? (cdr args)))
	 (if (number? (car args))
	     (abs (car args))
	     (error "gcd: num ~a is not a number." (car args))))
	(else
	 (let* ((return-inexact? #f)
		(normals
		 (map (lambda (x)
			(if (number? x)
			    (if (exact? x)
				(abs x)
				(let ((y (inexact->exact x)))
				  (set! return-inexact? #t)
				  (if (= (- y x) 0)
				      (abs y)
				      (error "gcd: num ~a is not an integer." 
					     x))))
			    (error "gcd: num ~a is not a number." x)))
		      args)))
	   (let loop ((first (car normals))
		      (rest (cdr normals)))
	     (if (pair? rest)
		 (loop (raw-gcd first (car rest)) (cdr rest))
		 (if return-inexact?
		     (exact->inexact first)
		     first)))))))

(define (lcm . args) 
  (cond ((not (pair? args)) 1)
	((not (pair? (cdr args)))
	 (if (number? (car args))
	     (abs (car args))
	     (error "lcm: num ~a is not a number." (car args))))
	(else
	 (let* ((return-inexact? #f)
		(normals
		 (map (lambda (x)
			(if (number? x)
			    (if (exact? x)
				(abs x)
				(let ((y (inexact->exact x)))
				  (set! return-inexact? #t)
				  (if (= (- y x) 0)
				      (abs y)
				      (error "lcm: num ~a is not an integer." x))))
				
			    (error "lcm: num ~a is not a number." x)))
		      args)))
	   (let loop ((first (car normals))
		      (rest (cdr normals)))
	     (if (pair? rest)
		 (loop (raw-lcm first (car rest)) (cdr rest))
		 (if return-inexact?
		     (exact->inexact first)
		     first)))))))

;; rational
(define (numerator a)
  (if (eq? (object-class a) <mp-rational>)
      (numerator-raw a)
      a))

(define (denominator a)
  (if (eq? (object-class a) <mp-rational>)
      (denominator-raw a)
      1))
