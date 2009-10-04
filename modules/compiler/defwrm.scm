#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/compiler/defwrm.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.4
 | File mod date:    2002-11-13 07:55:02
 | System build:     v0.7.3.4-b7u, 2007-05-30
 |
 `------------------------------------------------------------------------|#

(define-rewriter (define-write-method form)
  (cons 'define-method (cons 'write-object (cdr form))))

(define-thread-var *place* '())
