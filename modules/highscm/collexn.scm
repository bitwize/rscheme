#|------------------------------------------------------------*-Scheme-*--|
 | File:	    modules/highscm/collexn.scm
 |
 |          Copyright (C)1998 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |	    See <http://www.rscheme.org/> for the latest info.
 |
 | File version:     1.2
 | File mod date:    1999-01-10 01:37:29
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  high-scheme
 |
 | Purpose:          Collection methods for built-in classes
 `------------------------------------------------------------------------|#

;;;   ==================
;;;   Iteration Protocol
;;;   ==================
;;;   The iteration protocol, following Dylan, consists
;;;   of the generic procedures for creating iteration
;;;   states:
;;;       `initial-state'
;;;       `next-state'      ;; return #f at end of sequence
;;;       `final-state'
;;;   and accessing the current element
;;;       `current-element'
;;;

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


(define-method size ((self <list>))
  (length self))

(define-method element ((self <list>) 
			(key <fixnum>) 
			#key (default default: '#unbound))
  (if (eq? default '#unbound)
      (list-ref self key)
      (let ((t (nth-cdr* self key (lambda (lst k) '()))))
	(cond
	 ((pair? t)
	  (car t))
	 ((null? t)
	  default)
	 (else
	  (signal-improper-list self t))))))

(define-method set-element! ((self <list>) 
			     (key <fixnum>) 
			     (value <object>))
  (list-set! self key value))

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

(define-method size ((self <vector>))
  (gvec-length self))

(define-method element ((self <vector>) 
			(key <fixnum>) 
			#key (default default: '#unbound))
  (if (and (fixnum>=? key 0)
	   (fixnum<? key (gvec-length self)))
      (gvec-ref self key)
      (if (eq? default '#unbound)
	  (signal (make <no-such-key>
			collection: self
			key: key))
	  default)))

(define-method set-element! ((self <vector>)
			     (key <fixnum>)
			     (value <object>))
  (if (and (fixnum>=? key 0)
	   (fixnum<? key (gvec-length self)))
      (begin
	(gvec-set! self key value)
	(values))
      (signal (make <no-such-key>
		    collection: self
		    key: key))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-method initial-state ((self <string>))
  0)

(define-method previous-state ((self <string>) (state <fixnum>))
  (if (eq? state 0)
      #f
      (sub1 state)))

(define-method final-state ((self <string>))
  (let (((n <fixnum>) (string-length self)))
    (if (eq? n 0)
	#f
	(sub1 n))))

(define-method next-state ((self <string>) (state <fixnum>))
  (let (((n <fixnum>) (add1 state)))
    (if (fixnum<? n (string-length self))
	n
	#f)))

(define-method current-element ((self <string>) (state <fixnum>))
  (string-ref self state))

(define-method size ((self <string>))
  (string-length self))

(define-method size ((self <unicode-string>))
  (div2 (bvec-length self)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  hash tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-method initial-state ((self <hash-table>))
  ;; not terribly efficient...
  (let ((s (value-sequence self)))
    (if (null? s)
	#f
	s)))

(define-method next-state ((self <hash-table>) (state <pair>))
  (let ((n (cdr state)))
    (if (pair? n)
	n
	#f)))

(define-method current-element ((self <hash-table>) (state <pair>))
  (car state))

