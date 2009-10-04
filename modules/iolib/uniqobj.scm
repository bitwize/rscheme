#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/iolib/uniqobj.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.9
 | File mod date:    2000-11-21 23:25:08
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  iolib
 |
 `------------------------------------------------------------------------|#

(define *unique-object-names*
  '#("#none" "#undef" "#uninit" "#unbound" "#rest" "#key" "#all-keys"
	     "#next" "#missing" "#debug-trap" "#unmapped"
             "#optional"))

(define-method write-object ((self <unique-obj>) port)
  (write-string port (to-string self)))

(define-method to-string ((self <unique-obj>))
  (let (((i <fixnum>) (get-immob-value self))
	(v *unique-object-names*))
    (if (and (fixnum<? i (vector-length v))
	     (vector-ref v i))
	(vector-ref v i)
	(string-append "#unique:" (number->string i)))))

(define (string->unique-object str)
  (let loop (((i <fixnum>) 0)
	     ((n <fixnum>) (vector-length *unique-object-names*)))
    (if (eq? n 0)
	#f
	(if (string-ci=? (vector-ref *unique-object-names* i) str)
	    (make-immob 4 i)
	    (loop (add1 i) (sub1 n))))))

;; returns the new unique object

(define (add-unique-object! (name <string>) ix)
  (if ix
      (begin
	(if (< ix (vector-length *unique-object-names*))
	    (vector-set! *unique-object-names* ix name)
	    (set! *unique-object-names*
		  (vector-append
		   *unique-object-names*
		   (make-vector (- ix (vector-length *unique-object-names*)) 
				"")
		   (vector name))))
	(make-immob 4 ix))
      (let ((i (vector-length *unique-object-names*)))
	(set! *unique-object-names*
	      (vector-append *unique-object-names* (vector name)))
	(make-immob 4 i))))

	      