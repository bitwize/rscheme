#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/highscm/promise.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.6
 | File mod date:    1998-08-05 10:44:56
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  high-scheme
 |
 | Purpose:          delay and force
 `------------------------------------------------------------------------|#

(define-class <promise> (<object>)
  compute-using
  (computed-value init-value: #f))

(define-method force ((self <object>))
  self)

(define-method force ((self <promise>))
  (let ((thunk (compute-using self)))
    (if thunk
	(begin
	  (set-compute-using! self #f)
	  (set-computed-value! self (thunk))))
    (computed-value self)))

(define-syntax (delay expr)
  (make <promise>
	compute-using: (lambda () expr)))

(define-method write-object ((self <promise>) port)
  (if (compute-using self)
      (format port "#[<promise>]")
      (format port "#[<promise> fulfilled]")))
