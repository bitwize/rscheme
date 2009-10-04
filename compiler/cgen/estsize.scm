#|------------------------------------------------------------*-Scheme-*--|
 | File:    compiler/cgen/estsize.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.2
 | File mod date:    1997-11-29 23:10:27
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  (rsc)
 |
 `------------------------------------------------------------------------|#


;; estimating the size of code...

(define (count-vinsn vi)
    (case (car vi)
	((if)
	    (+ 1 (count-vinsn (caddr vi))
	         (count-vinsn (cadddr vi))))
	((seq)
	    (+ 1 (count-vinsns (cdr vi))))
	(else
	    1)))
	
(define (count-vinsns vinsns)
    (let loop ((i vinsns) (n 0))
	(if (null? i)
	    n
	    (loop (cdr i)
	    	  (+ (count-vinsn (car vinsns))
		     n)))))

(define (count-literal-bytes lc)
    (if (pair? lc)
	(+ (count-literal-bytes (car lc))
	   (count-literal-bytes (cdr lc)))
	(if (string? lc)
	    (string-length lc)
	    0)))
	    
