#|------------------------------------------------------------*-Scheme-*--|
 | File:    packages/sets/iterate.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |	    See <http://www.rscheme.org/> for the latest info.
 |
 | File version:     1.1
 | File mod date:    1997-10-25 22:11:44
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  sets
 |
 | Purpose:          Dylan iteration protocol
 `------------------------------------------------------------------------|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-method initial-state ((self <pair>))
  self)

(define-method next-state ((self <pair>) (state <pair>))
  (let ((n (cdr state)))
    (if (pair? n)
	n
	#f)))

(define-method current-element ((self <pair>) (state <pair>))
  (car state))

(define-method initial-state ((self <empty-list>))
  #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  vectors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-method initial-state ((self <vector>))
  0)

(define-method previous-state ((self <vector>) (state <fixnum>))
  (if (eq? state 0)
      #f
      (sub1 state)))

(define-method final-state ((self <vector>))
  (let (((n <fixnum>) (vector-length self)))
    (if (eq? n 0)
	#f
	(sub1 n))))

(define-method next-state ((self <vector>) (state <fixnum>))
  (let (((n <fixnum>) (add1 state)))
    (if (fixnum<? n (vector-length self))
	n
	#f)))

(define-method current-element ((self <vector>) (state <fixnum>))
  (vector-ref self state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  hash tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-method initial-state ((self <table>))
  ;; not terribly efficient...
  (let ((s (value-sequence self)))
    (if (null? s)
	#f
	s)))

(define-method next-state ((self <table>) (state <pair>))
  (let ((n (cdr state)))
    (if (pair? n)
	n
	#f)))

(define-method current-element ((self <table>) (state <pair>))
  (car state))
