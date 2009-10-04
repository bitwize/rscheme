#|------------------------------------------------------------*-Scheme-*--|
 | File:    packages/sets/basic.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |	    See <http://www.rscheme.org/> for the latest info.
 |
 | File version:     1.3
 | File mod date:    1997-10-25 22:12:06
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  sets
 |
 | Purpose:          Dylan set functions
 `------------------------------------------------------------------------|#

(define-method value-sequence ((self <list>))
  self)

(define-method value-sequence ((self <vector>))
  (vector->list self))

