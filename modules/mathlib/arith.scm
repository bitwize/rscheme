#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/mathlib/arith.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.15
 | File mod date:    2005-05-16 18:45:24
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  mathlib
 |
 | Purpose:          high-level arithmetic (transcendentals, etc)
 `------------------------------------------------------------------------|#

(define-method floor ((self <integer>))
  self)

(define-method floor ((self <double-float>))
  (float-floor self))

(define-method floor ((self <mp-rational>))
  (quotient (numerator self)
	    (denominator self)))

;;

(define-method ceiling ((self <integer>))
  self)

(define-method ceiling ((self <double-float>))
  (float-ceiling self))

(define-method ceiling ((self <mp-rational>))
  (let ((n (numerator self))
	(d (denominator self)))
    (quotient (+ n d -1) d)))

;;

(define-method truncate ((self <integer>))
  self)

(define-method truncate ((self <double-float>))
  (exact->inexact (float-truncate self)))

(define-method truncate ((self <mp-rational>))
  (if (positive? self)
      (floor self)
      (- (floor (abs self)))))

;;;

(define-method round ((self <integer>))
  self)

(define-method round ((self <mp-rational>))
  (quotient (numerator self)
	    (denominator self)))

(define-method round ((self <double-float>))
  (exact->inexact (float-round self)))

(define-method round ((self <mp-rational>))
  (let ((n (numerator self))
	(d (denominator self)))
    ;; there _must_ be a cheaper way to do this!
    ;; but it should behave so that
    ;;      (round 1/2)          = 0
    ;;  and (round 3/2) [should] = 2
    ;; we need to check the negative cases, too
    (let* ((val (quotient (+ n (quotient d 2)) d))
	   (rem (- self val)))
      (if (= rem (/ -1 2))
	  (if (odd? val)
	      (values (- val 1) (+ rem 1))
	      (values val rem))
	  (values val rem)))))

;;;

(define-glue (float-round x)
{
#if !FULL_NUMERIC_TOWER
  IEEE_64 a = extract_float(x);
  INT_32 ai;
    
  if (a < 0)
    {
      ai = (int)(a - 0.5);
      if (ai >= -536870912)
	REG0 = int2fx( ai );
      else
	REG0 = x;
    }
  else
    {
      ai = (int)(a + 0.5);
      if (ai <= 536870911)
	REG0 = int2fx( ai );
      else
	REG0 = x;
    }
    RETURN(1);
#else
  extern obj float_truncate( IEEE_64 x );
  IEEE_64 a = extract_float( x );

  if (a < 0)
    a -= 0.5; 
  else 
    a += 0.5;
  REG0 = float_truncate( a );
  RETURN(1);
#endif
})

(define-method exact->inexact ((self <fixnum>))
  (raw-int->double-float self))

(define-method exact->inexact ((self <long-int>))
  (raw-longint->double-float self))

(define-method exact->inexact ((self <double-float>))
  self)

(define-method exact->inexact ((self <bignum>))
  (raw-bignum->double-float self))

(define-method exact->inexact ((self <mp-rational>))
  (raw-rational->double-float self))

(define-method exact->inexact ((self <rect-complex>))
  (make-rectangular 
   (exact->inexact (real-part self)) 
   (exact->inexact (imag-part self))))

(define-method inexact->exact ((self <fixnum>))
  self)

(define-method inexact->exact ((self <long-int>))
  self)

(define-method inexact->exact ((self <double-float>))
  (float-truncate self))

(define-method inexact->exact ((self <bignum>))
  self)

(define-method inexact->exact ((self <mp-rational>))
  self)

(define-method inexact->exact ((self <rect-complex>))
  (make-rectangular 
   (inexact->exact (real-part self)) 
   (inexact->exact (imag-part self))))

(define-rewriter (real-operator form)
  (let* ((op (symbol->string (cadr form)))
	 (fl (string->symbol (string-append "float-" op))))
    `(define (,(cadr form) (z <number>))
       (if (double-float? z)
	   (,fl z)
	   (,fl (exact->inexact z))))))

(define (log/e z)
  (if (double-float? z)
      (float-log z)
      (float-log (exact->inexact z))))

(define-inline log
  (nlambda
   ((n) (log/e n))
   ((n base) (/ (log/e n) (log/e base)))))

(real-operator exp)
(real-operator sin)
(real-operator cos)
(real-operator tan)
(real-operator asin)
(real-operator acos)
(real-operator sqrt)

;;

(define-syntax with-raw-float 
  (syntax-form (head x)
    (if (double-float? x)
	(head x)
	(if (fixnum? x)
	    (head (raw-int->raw-float x))
	    (head (exact->inexact x)))))
  (syntax-form (head x y)
    (if (double-float? x)
	(if (double-float? y)
	    (head x y)
	    (if (fixnum? y)
		(head x (raw-int->raw-float y))
		(head x (exact->inexact y))))
	(if (fixnum? x)
	    (if (double-float? y)
		(head (raw-int->raw-float x) y)
		(head (raw-int->raw-float x) (exact->inexact y)))
	    (head (exact->inexact x)
		  (exact->inexact y))))))

(define (atan (z <number>) . rest)
  (if (null? rest)
      (with-raw-float float-atan1 z)
      (if (null? (cdr rest))
	  (with-raw-float float-atan2 z (car rest))
	  (error "atan: too many args"))))

(define (float-result-expt z1 z2)
  (with-raw-float float-pow z1 z2))
