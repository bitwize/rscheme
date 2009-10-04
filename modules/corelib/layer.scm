#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/corelib/layer.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.9
 | File mod date:    2004-03-24 14:39:22
 | System build:     v0.7.3.4-b7u, 2007-05-30
 |
 | Purpose:          Provide first-class procedures for primops
 `------------------------------------------------------------------------|#

(define-full-bdg (pair? thing)
  (pair? thing))

(define-full-bdg (procedure? thing)
  (procedure? thing))

(define-full-bdg (vector? thing)
  (vector? thing))

(define-full-bdg (cons a b)
  (cons a b))

(define-full-bdg (car arg)
  (car arg))

(define-full-bdg (cdr arg)
  (cdr arg))

(define-full-bdg (eq? a b)
  (eq? a b))

(define-full-bdg (string=? a b)
  (string=? a b))

(define-full-bdg (string-ci=? a b)
  (string-ci=? a b))


(define-full-bdg (integer->hash i)
  (integer->hash i))

(define-full-bdg (symbol->hash i)
  (symbol->hash i))

(define-full-bdg (string->hash i)
  (string->hash i))

(define-full-bdg (string-ci->hash i)
  (string-ci->hash i))

(define-full-bdg (transient->hash i)
  (transient->hash i))

(define-full-bdg (immob->hash i)
  (if (ptr? i)
      (type-error immob->hash 0 i "a pointer")
      (immob->hash i)))

(define-full-bdg (object-class thing)
  (object-class thing))

(define-full-bdg (string=? a b)
  (string=? a b))

(define-full-bdg (string-ci=? a b)
  (string-ci=? a b))

;;

(define-full-bdg (vector-ref vector index)
  (vector-ref vector index))

(define-full-bdg (vector-set! vector index item)
  (vector-set! vector index item))

;;

(define-full-bdg (not x)
  (not x))

(define-full-bdg (set-car! a b)
  (set-car! a b)
  (values))

(define-full-bdg (set-cdr! a b)
  (set-cdr! a b)
  (values))

(define-full-bdg (string->symbol s)
  (string->symbol s))

(define-full-bdg (symbol->string x)
  (symbol->string x))


(define-full-bdg (null? x)
  (null? x))

(define-full-bdg (boolean? x)
  (boolean? x))

(define-full-bdg (string? x)
  (string? x))

(define-full-bdg (symbol? x)
  (symbol? x))

;;

(define-full-bdg (instance? a b)
  (instance? a b))

(define-full-bdg (subclass? a b)
  (subclass? a b))

(define-full-bdg (class? c)
  (class? c))

#|
output-port?
transcript-off
transcript-on
char-ready?
char-upcase?
gcd
input-port?
lcm
load
|#
