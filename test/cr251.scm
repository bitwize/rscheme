#|------------------------------------------------------------*-Scheme-*--|
 | File:    test/cr251.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.3
 | File mod date:    1997-11-29 23:10:42
 | System build:     v0.7.3.4-b7u, 2007-05-30
 |
 `------------------------------------------------------------------------|#

;;;======================================================================

(define (dhb)
  (bind ((x (bind ((y 3)) 
	      (lambda () y)))) 
    (lambda () x)))

(test-section 
 (double-heap-bind)
 ;;
 (check 3 (and (procedure? ((dhb))) (((dhb))))))

;;;======================================================================

(define (bar x) 
  (values 5 'five))

(define (foo x) 
  (bind (((a <fixnum>) (b <symbol>) (bar x))) 
    a))

;;;======================================================================

(test-section (double-reg-bind)
  (check 5 (foo 3)))

;;;======================================================================

(define *fo* 3)

(define (crs (str <string>))
  (let ((n (list 1 2)))
    (values n n (string-length str))))

(define (v)
  (bind ((#rest r (crs "foo")))
    r))

(test-section
 (parallel-assmt)
 ;;
 (check '((1 2) (1 2) 3) (v)))

