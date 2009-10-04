#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/mlink/seq.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.5
 | File mod date:    1998-02-27 14:27:28
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  mlink
 |
 | Purpose:          <seq> objects (tail-growing lists)
 `------------------------------------------------------------------------|#

(define-class <seq> (<object>)
  seq-first
  seq-last)

(define-method initialize ((self <seq>))
  (let ((i (cons 0 '())))
    (gvec-set! self 0 i)
    (gvec-set! self 1 i)
    self))

(define (make-seq)
  (make <seq>
	seq-first: #f
	seq-last: #f))

(define (seq-add! (seq <seq>) item)
  (let* ((the-item item)
	 (i (cons the-item '())))
    (set-cdr! (gvec-ref seq 1) i)
    (gvec-set! seq 1 i)
    the-item))

(define (seq->list (seq <seq>))
  (cdr (gvec-ref seq 0)))

(define (seq-empty? (seq <seq>))
  (null? (cdr (gvec-ref seq 0))))

