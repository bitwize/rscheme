#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/mathlib/numbers.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.15
 | File mod date:    2003-02-23 17:05:24
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  mathlib
 |
 | Purpose:          low-level arithmetic (simple operations)
 `------------------------------------------------------------------------|#

(define-generic-function round)
(define-generic-function abs)

(define (quantity? x) (instance? x <quantity>))
(define (number? x)  (instance? x <number>))
(define (complex? x) (instance? x <complex>))
(define (real? x) (instance? x <real>))
(define (rational? x) (instance? x <rational>))

(define (integer? x)
    (or (fixnum? x)
	(instance? x <bignum>)
	(instance? x <long-int>)
	(and (double-float? x)
	     (base=? x (round x)))))

(define (exact? x)
  (not (inexact? x)))

(define (inexact? x)
  (or 
   (double-float? x)
   (and (instance? x <rect-complex>)
	(or
	 (double-float? (real-part x))
	 (double-float? (imag-part x))))))

(define-syntax +
  (syntax-form ()
    0)
  (syntax-form (a)
    a)
  (syntax-form (a b)
    (base+ a b))
  (syntax-form (a b . more)
    (base+ a (+ b . more)))
  (else full+))

(define-syntax *
  (syntax-form ()
    1)
  (syntax-form (a)
    a)
  (syntax-form (a b)
    (base* a b))
  (syntax-form (a b . more)
    (base* a (* b . more)))
  (else full*))


(define-syntax -
  (syntax-form ()
    0)
  (syntax-form (a)
    (base- 0 a))
  (syntax-form (a b)
    (base- a b))
  (syntax-form (a b . more)
    (- (base- a b) . more))
  (else full-))

(define-syntax /
  (syntax-form ()
    1)
  (syntax-form (a)
    (base/ 1 a))
  (syntax-form (a b)
    (base/ a b))
  (syntax-form (a b . more)
    (/ (base/ a b) . more))
  (else full/))

(define (full+ . args)
  (let loop ((x args) 
	     (sum 0))
    (if (null? x)
	sum
	(loop (cdr x)
	      (base+ sum (car x))))))

(define (full- arg1 . args)
  (if (null? args)
      (base- 0 arg1)
      (let loop ((x (cdr args))
		 (diff (base- arg1 (car args))))
	(if (null? x)
	    diff
	    (loop (cdr x) (base- diff (car x)))))))

(define (full* . args)
  (let loop ((x args) (prod 1))
    (if (null? x)
	prod
	(loop (cdr x) (base* prod (car x))))))

(define (full/ arg1 . args)
  (if (null? args)
      (base/ 1 arg1)
      (let loop ((x (cdr args))
		 (quotient (base/ arg1 (car args))))
	(if (null? x)
	    quotient
	    (loop (cdr x) (base/ quotient (car x)))))))

(define-syntax %transitively
    (syntax-form (binary? arg1 arg2 rest)
	(if (null? rest)
	    (binary? arg1 arg2)
	    (let loop ((args (cons arg1 (cons arg2 rest))))
		(if (null? (cdr (cdr args)))
		    (binary? (car args) (car (cdr args)))
		    (if (binary? (car args) (car (cdr args)))
			(loop (cdr args))
			#f))))))

(define-macro (define-arithmetic-order op primop)
  (let ((full-op (string->symbol (string-append "full" 
						(symbol->string op)
						"?"))))
    `(begin
       (define-syntax ,op
	 (syntax-form (a b)
	   (,primop a b))
	 (syntax-form (a b c . more)
	   (if (,primop a b)
	       (,op b c . more)
	       #f))
	 (else ,full-op))
       (define (,full-op arg1 arg2 . args)
	 (%transitively ,primop arg1 arg2 args)))))

(define-arithmetic-order =  base=?)
(define-arithmetic-order <  base<?)
(define-arithmetic-order <= base<=?)
(define-arithmetic-order >  base>?)
(define-arithmetic-order >= base>=?)

(define (zero? x)
    (= x 0))

(define (negative? x)
    (< x 0))

(define (positive? x)
    (> x 0))

(define (even? n)
  (zero? (bitwise-and n 1)))

(define (odd? n)
  (not (zero? (bitwise-and n 1))))


#| there is a bug in these definitions...
   R4RS p.21:
	Note:  If any argument is inexact, then the result will
	also be inexact...
|#

(define (min arg1 . args)
    (let loop ((x args) (smallest arg1))
	(if (pair? x)
	    (loop (cdr x)
		  (if (base<? (car x) smallest)
		      (car x)
		      smallest))
	    smallest)))

(define (max arg1 . args)
    (let loop ((x args) (largest arg1))
	(if (pair? x)
	    (loop (cdr x)
		  (if (base>? (car x) largest)
		      (car x)
		      largest))
	    largest)))

(define-method abs ((self <number>))
  (if (< self 0)
      (base- 0 self)
      self))
  
(define-method abs ((self <rect-complex>))
  (let ((re (real-part self))
	(im (imag-part self)))
    (sqrt (+ (* re re) (* im im)))))
