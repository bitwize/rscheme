#|------------------------------------------------------------*-Scheme-*--|
 | File:    test/longint.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.4
 | File mod date:    1997-11-29 23:10:42
 | System build:     v0.7.3.4-b7u, 2007-05-30
 |
 `------------------------------------------------------------------------|#

(define-method unary- ((self <long-int>))
  (raw-int-64-neg self))

(define-method equal? ((self <long-int>) b)
  (if (instance? b <integer>)
      (if (fixnum? b)
	  (raw-int-64=? self (fixnum->raw-int-64 b))
	  (raw-int-64=? self b))
      #f))

(define (un-long x)
  (if (pair? x)
      (cons (un-long (car x)) (un-long (cdr x)))
      (if (long-int? x)
	  (raw-int-64->integer x)
	  x)))

(define (long (a <fixnum>))
  (raw-int-64->long-int (raw-int->raw-int-64 a)))

(define (approx=? a b)
  (< (abs (- a b)) 0.00001))

(define a (long 13))
(define b (long 4))
(define c (long 12))

(define-syntax (trial a-sign b-sign)
  (trial* (mquote a-sign)
	  (mquote b-sign)))

(define (trial* a-sign b-sign)
  (let (((a <long-int>) (if (eq? a-sign '-)
			    (raw-int-64-neg a)
			    a))
	((b <long-int>) (if (eq? b-sign '-)
			    (raw-int-64-neg b)
			    b))
	((c <long-int>) (if (eq? a-sign '-)
			    (raw-int-64-neg c)
			    c)))
    (un-long
     (list (list (list a b)
		 (raw-int-64-quotient a b)
		 (raw-int-64-remainder a b)
		 (raw-int-64-modulo a b))
	   (list (list c b)
		 (raw-int-64-quotient c b)
		 (raw-int-64-remainder c b)
		 (raw-int-64-modulo c b))))))

;;
(test-section
 (num-thy)
 ;;
 (test-section
  (longint)
  ;;
  (check '(((13 4)   3  1  1) ((12 4)   3 0 0)) (trial + +))
  (check '(((13 -4) -3  1 -3) ((12 -4) -3 0 0)) (trial + -))
  (check '(((-13 4) -3 -1  3) ((-12 4) -3 0 0)) (trial - +))
  (check '(((-13 -4) 3 -1 -1) ((-12 -4) 3 0 0)) (trial - -)))
 ;;
 (compare-using
  approx=?
  (test-section
   (float)
   ;;
   (check 0.4 (modulo 13.4 1))
   (check 0.6 (modulo -13.4 1))
   (check 0.4 (remainder 13.4 1))
   (check -0.4 (remainder -13.4 1)))))
