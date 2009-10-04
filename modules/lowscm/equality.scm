#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/lowscm/equality.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.4
 | File mod date:    1997-11-29 23:10:37
 | System build:     v0.7.3.4-b7u, 2007-05-30
 |
 | Purpose:          equality predicates (generic functions)
 `------------------------------------------------------------------------|#

;;
;; these predicates are available...
;;
;; the default behavior (defined in objsys/object.scm) is
;; that of eq?, but they are specialized later (in highscm/equiv?)

(define-generic-function equal?)
(define-generic-function eqv?)
