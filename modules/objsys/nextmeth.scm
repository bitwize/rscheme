#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/objsys/nextmeth.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.4
 | File mod date:    1998-08-29 19:45:08
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  objsys
 |
 `------------------------------------------------------------------------|#

(define (find-next-method-1 (gf <single-dispatch-gf>) (class <<class>>))
  (let ((key-class (car (superclasses class))))
    (let loop ((i (generic-function-methods gf)))
      (if (pair? i)
	  (if (subclass? key-class (method-key-class (car i)))
	      (car i)
	      (loop (cdr i)))
	  #f))))



;; (define-method initialize ((self <my-thread>) #rest keystuff))

#|
;;

(define-class <foo> (<object>) x)

(define-class <bar> (<foo>) y)

(define-method* initialize ((self <foo>) #rest stuff)
  (format #t "foo stuff: ~s\n" stuff)
  (next-method self)
  (set-x! self 10))

(define-method* initialize ((self <bar>) #rest stuff)
  (format #t "bar stuff: ~s\n" stuff)
  (set-x! self 11)
  (next-method)
  (set-y! self (x self)))
|#
