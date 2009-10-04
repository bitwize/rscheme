#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/mathlib/numstr.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.7
 | File mod date:    2007-01-28 09:37:58
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  mathlib
 |
 `------------------------------------------------------------------------|#

;; this isn't the most efficient thing in the world...
;; could have multiple lists depending on static lexical
;; structure, like whether it contains a '.' or a '/'

(%early-once-only
 (define *number-parsers* (list string->fixnum
				string->long-int
				string->bignum
				string->rational
				string->rational*
				string->float
				string->complex)))

;;;
;;;  Handle rational input notation even if we don't
;;;  support exact rationals...
;;;

(define (string->rational* str radix)
  ;;
  (define (string->integer str)
    (or (string->fixnum str 10)
        (string->long-int str 10)
        (string->bignum str 10)))
  ;;
  (and (= radix 10)
       (let ((slash (string-search str #\/)))
         (and slash
              (let ((num (string->integer (substring str 0 slash))))
                (and num
                     (let ((den (string->integer (substring str (+ slash 1)))))
                       (and den (/ num den)))))))))

                   

(define (get-radix (rest <list>) default)
  (if (null? rest)
      default
      (if (null? (cdr rest))
	  (if (fixnum? (car rest))
	      (let (((r <fixnum>) (car rest)))
		(if (and (fixnum>=? r 2)
			 (fixnum<=? r 36))
		    r
		    (error "radix specification `~d' is not in range 2..36" 
			   r)))
	      (error "radix specification `~s' is not a fixnum"
		     (car rest)))
	  (error "radix specification `~s' followed by: ~s" 
		 (car rest)
		 (cdr rest)))))

(define (string->number (str <string>) . rest)
  ;;
  ;; strip off the exactness specification, if any
  ;;
  (define (get-exactness str)
    (let ((len (string-length str)))
      (if (and (> len 2)
	       (eq? (string-ref str 0) #\#))
	  (case (string-ref str 1)
	    ((#\e #\E)
	     (values (substring str 2 len) #t))
	    ((#\i #\I)
	     (values (substring str 2 len) #f))
	    (else
	     (values str #t)))
	  (values str #t))))
  ;;
  ;; loop through the defined number parsers, trying each one
  ;;
  (define (string->number0 (str <string>) (radix <fixnum>))
    (let loop ((p *number-parsers*))
      (if (pair? p)
	  (or ((car p) str radix)
	      (loop (cdr p)))
	  #f)))
  ;;
  (bind ((str ex (get-exactness str))
	 (num (if (and (> (string-length str) 2)
		       (eq? (string-ref str 0) #\#))
		  (case (string-ref str 1)
		    ((#\x #\X) (string->number0 (substring str 2) 16))
		    ((#\o #\O) (string->number0 (substring str 2)  8))
		    ((#\b #\B) (string->number0 (substring str 2)  2))
		    ((#\d #\D) (string->number0 (substring str 2) 10))
		    (else #f))
		  (string->number0 str (get-radix rest 10)))))
    (and num
	 (if (and (not ex)
		  (exact? num))
	     (exact->inexact num)
	     num))))

(define (string->complex (str <string>) (radix <fixnum>))
  (let ((l (- (string-length str) 1)))
    ;
    (define (substring->number str from to)
      (string->number (substring str from to) radix))
    ;
    (if (and (> l 0)
	     (eq? #\i (string-ref str l)))
	(let loop ((ndx (- l 1)))
	  (if (>= ndx 0)
	      (let ((ch (string-ref str ndx)))
		(if (or (eqv? ch #\+)
			(eqv? ch #\-))
		    (let ((re (if (zero? ndx)
				  0
				  (substring->number str 0 ndx)))
			  (im (if (eq? (+ ndx 1) l)
				  (cond
				   ((eqv? ch #\+)
				    1)
				   ((eqv? ch #\-)
				    -1))
				  (substring->number str ndx l))))
		      (if (and (exact? im)
			       (zero? im))
			  re
			  (and re im (make-rectangular re im))))
		    (loop (- ndx 1))))
	      (make-rectangular 0 (substring->number str 0 l))))
	#f)))

(define (rational->string (a <mp-rational>) (radix <fixnum>))
  (let* ((num (numerator a))
	 (den (denominator a))
	 (str (number-as-string num radix)))
    (if (eq? den 1)
	str
	(string-append
	 str "/" (number-as-string den radix)))))

(define (complex->string (a <rect-complex>) (radix <fixnum>))
  (define (image-part->string (a <number>) (radix <fixnum>))
    (cond ((and (exact? a)
		(eq? (abs a) 1))
	   (if (positive? a)
	       "+i"
	       "-i"))
      ((positive? a)
       (string-append "+" (number->string a radix) "i"))
      (else
       (string-append (number->string a radix) "i"))))
  (define (real-part->string (a <number>) (radix <fixnum>))
    (if (and (exact? a)
	     (zero? a))
	""
	(number->string a radix)))
  (string-append (real-part->string (real-part a) radix)
	  (image-part->string (imag-part a) radix)))


(define-method number-as-string ((self <fixnum>) (radix <fixnum>))
  (fixnum->string self radix))

(define-method number-as-string ((self <double-float>) (radix <fixnum>))
  (if (not (eq? radix 10))
      (error "number->string: radix ~d != 10" radix))
  (double-float->string self))

(define-method number-as-string ((self <long-int>) (radix <fixnum>))
  (long-int->string self radix))

(define-method number-as-string ((self <bignum>) (radix <fixnum>))
  (bignum->string self radix))

(define-method number-as-string ((self <mp-rational>) (radix <fixnum>))
  (rational->string self radix))

(define-method number-as-string ((self <rect-complex>) (radix <fixnum>))
  (complex->string self radix))

(define (number->string (num <number>) . rest)
  (number-as-string num (get-radix rest 10)))

(define-method to-string ((self <number>))
  (number-as-string self 10))
