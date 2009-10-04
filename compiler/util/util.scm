#|------------------------------------------------------------*-Scheme-*--|
 | File:    compiler/util/util.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.2
 | File mod date:    1997-11-29 23:10:28
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  (rsc)
 |
 `------------------------------------------------------------------------|#


(define (find-lex-addr ct-envt var)
    (let loop ((e ct-envt) (i 0))
	(let ((x (memq var (car e))))
	    (if x
		(cons i (- (length (car e)) (length x)))
		(loop (cdr e) (add1 i))))))
