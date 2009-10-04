#|------------------------------------------------------------*-Scheme-*--|
 | File:	    modules/hacks/debugapi.scm
 |
 |          Copyright (C)1998 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |	    See <http://www.rscheme.org/> for the latest info.
 |
 | File version:     1.1
 | File mod date:    1999-01-08 13:25:58
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  &module;
 |
 | Purpose:          Collect together accessors for debugging info
 `------------------------------------------------------------------------|#

;;; debug info is represent as an association list by info
;;; type, the value of each is a mapping from PC "regions" to
;;; values.  The regions are stored packed into vectors in the format
;;;
;;;      #(FROM TO VALUE ...)
;;;
;;;  e.g.,
;;;    ((envt-frames #( 10 100 #(#(a b c))
;;;                    100 200 #(#(x y z) #(a b c))
;;;                    200 300 #(#(a b c))))
;;;     (source-location #(0 10 (146 "disassem.scm")
;;;                        10 20 (147 "disassem.scm")))
;;;     (registers       #(0  10 (a b c)
;;;                        16 20 ((+ a 1))
;;;                        20 24 (temp #f))))
;;;

;;; theoretically, this should be able to work with C-compile code, too,
;;; though without the resolution

(define (get-debug-info (t <template>) (pc <fixnum>) type)
  (let ((info (assq 'debug-info (function-descr t))))
    (if info
	(let ((info-of-type (assq type (cdr info))))
	  (if info-of-type
	      (let loop ((rgns (vector->list (cdr info-of-type))))
		(if (null? rgns)
		    #f
		    (if (and (>= pc (car rgns))
			     (< pc (cadr rgns)))
			(caddr rgns)
			(loop (cdddr rgns)))))
	      #f))
	#f)))

(define (get-source-location (t <template>) (pc <fixnum>))
  (get-debug-info t pc 'source-location))
