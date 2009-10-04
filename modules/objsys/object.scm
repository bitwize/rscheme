#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/objsys/object.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.8
 | File mod date:    1997-11-29 23:10:38
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  objsys
 |
 | Purpose:          methods on <object>
 `------------------------------------------------------------------------|#

;;
;;
;;  methods implemented by <object>
;;

(define-method equal? ((thing1 <object>) (thing2 <object>))
  (eq? thing1 thing2))

(define-method eqv? ((thing1 <object>) (thing2 <object>))
  (eq? thing1 thing2))


(define-class <excess-initializers> (<condition>)
  object
  excess)

(define-method initialize ((self <object>) #rest extra)
  (if (null? extra)
      ;; do nothing
      ;; (it is no longer required, btw, that initialize return self)
      self
      ;; signal an error
      (signal (make <excess-initializers>
		    object: self
		    excess: extra))))

(define-method finalize ((self <object>))
  ;; do nothing
  )
