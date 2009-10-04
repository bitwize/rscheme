#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/mathlib/expt.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.6
 | File mod date:    2003-10-22 18:02:16
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  mathlib
 |
 | Purpose:          a correct `expt' implementation for fixnums
 `------------------------------------------------------------------------|#

;; 
;;
;;  improved expt (R4RS conforming -- returns exact
;;                 values when possible & given exact inputs)
;;  incorporated into (v2.0.5m, 94.08.09)

(define-syntax (unary-fixnum-neg x)
  (fixnum- 0 x))

(define (expt z1 z2)
    (if (and (fixnum? z1)
             (fixnum? z2))
	(cond ((eq? z2 0) 1)
	      ((eq? z2 1) z1)
	      ((eq? z1 0) 0)
	      ((eq? z1 1) 1)
	      ((fixnum<? z2 0)
               (float-result-expt z1 z2))
	      ((fixnum>? z1 0)
               (positive-fixnum-expt z1 z2))
	      (else
               (if (even? z2)
                   (positive-fixnum-expt (unary-fixnum-neg z1) z2)
                   ;; special case (-2)^29 because it
                   ;; can be represented, whereas 2^29 cannot be
                   (if (and (eq? z1 -2) (eq? z2 29))
                       -536870912
                       (- (positive-fixnum-expt 
                           (unary-fixnum-neg z1) 
                           z2))))))
	;; not both arguments are fixnums
	(float-result-expt z1 z2)))

;; (map make-entry (range 17))

(define $fixnum-expts
 '#(0
    0
    #(1 2 4 8 16 32 64 128 256 512 1024 2048 
      4096 8192 16384 32768 65536 131072 262144 
      524288 1048576 2097152 4194304 8388608 16777216 
      33554432 67108864 134217728 268435456)
    #(1 3 9 27 81 243 729 2187 6561 19683 59049 177147 
      531441 1594323 4782969 14348907 43046721 129140163 387420489)
    #(1 4 16 64 256 1024 4096 16384 65536 262144 1048576 
      4194304 16777216 67108864 268435456)
    #(1 5 25 125 625 3125 15625 78125 390625 1953125 
      9765625 48828125 244140625)
    #(1 6 36 216 1296 7776 46656 279936 1679616 10077696 
      60466176 362797056)
    #(1 7 49 343 2401 16807 117649 823543 5764801 40353607 282475249)
    #(1 8 64 512 4096 32768 262144 2097152 16777216 134217728)
    #(1 9 81 729 6561 59049 531441 4782969 43046721 387420489)
    #(1 10 100 1000 10000 100000 1000000 10000000 100000000)
    #(1 11 121 1331 14641 161051 1771561 19487171 214358881)
    #(1 12 144 1728 20736 248832 2985984 35831808 429981696)
    #(1 13 169 2197 28561 371293 4826809 62748517)
    #(1 14 196 2744 38416 537824 7529536 105413504)
    #(1 15 225 3375 50625 759375 11390625 170859375)
    #(1 16 256 4096 65536 1048576 16777216 268435456)))
  
;; n1 >= 2, n2 >= 2

(define (positive-fixnum-expt (n1 <fixnum>) (n2 <fixnum>))
  (if (fixnum<=? n1 16)
      (let ((n1-ent (gvec-ref $fixnum-expts n1)))
	(if (fixnum<? n2 (gvec-length n1-ent))
	    (gvec-ref n1-ent n2)
	    (float-result-expt n1 n2)))
      (let ((x (float-result-expt n1 n2)))
	(if (fixnum<=? x 536870911)
	    (round x)
	    x))))

#|
   Some functions I used to construct the $fixnum-expts table...

(define (range n)
    (let ((r '()))
	(let loop ((i n))
	    (if (eq? i 0)
		r
		(let ((j (- i 1)))
		    (set! r (cons j r))
		    (loop j))))))

(define (entry-size n)
    (inexact->exact (floor (/ (log 536870911) (log n)))))

(define (slow-expt n1 n2)
    (inexact->exact (expt n1 n2)))

(define (make-entry n1)
    (list->vector
	(map 
	    (lambda (n2)
		(slow-expt n1 n2))
	    (range (+ (entry-size n1) 1)))))
|#
