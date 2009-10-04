#|------------------------------------------------------------*-Scheme-*--|
 | File:    test/swizmode.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.2
 | File mod date:    1997-11-29 23:10:42
 | System build:     v0.7.3.4-b7u, 2007-05-30
 |
 `------------------------------------------------------------------------|#

,(use rstore)
;;;
;;;  code to test user-defined swizzler functions
;;;

(define-class <foo> (<object>)
  image-mode: 89
  a
  b
  c)

(define (t-create)
  (let ((p (create-persistent-store "/tmp/test-swiz.sto")))
    (setup-indirect-page p 1 (vector <foo>))
    (add-image-mode-handler! p (transient-cell-mode-handler))
    p))

(define (t-open)
  (let ((p (open-persistent-store "/tmp/test-swiz.sto")))
    (setup-indirect-page p 1 (vector <foo>))
    (add-image-mode-handler! p (transient-cell-mode-handler))
    p))

(define (t-setup ps)
  (commit ps (make <foo>
		   a: '(1 2 3)
		   b: '(the-end)
		   c: '(x y z))))

(define (t-next ps)
  (bind ((r (root-object ps))
	 (n (a r)))
    (set-b! r (cons (if (pair? n)
			(car n)
			'?)
		    (b r)))
    (set-a! r (if (pair? n) (cdr n) '()))))

(define *ps* (t-create))

(t-setup *ps*)
(t-next *ps*)
(t-next *ps*)
(commit *ps*)
(check '(2 1 the-end) (b (root-object *ps*)))
(t-next *ps*)
(t-next *ps*)
(check '(? 3 2 1 the-end) (b (root-object *ps*)))
(close-persistent-store *ps*)

(set! *ps* (t-open))
(check '(2 1 the-end) (b (root-object *ps*)))
(t-next *ps*)
(set-a! (root-object *ps*) '(p q r))
(t-next *ps*)
(commit *ps*)
(t-next *ps*)
(check '(q p ? 2 1 the-end) (b (root-object *ps*)))
(close-persistent-store *ps*)
