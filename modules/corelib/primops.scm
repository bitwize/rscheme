#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/corelib/primops.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.3
 | File mod date:    1997-11-29 23:10:39
 | System build:     v0.7.3.4-b7u, 2007-05-30
 |
 `------------------------------------------------------------------------|#


;;
;; these are the `special' primops, which are emitted specially
;; during code generation
;;

(define-primop (%make <<class>> . <obj>) => <obj>
  (bytecode 'make)
  (ccode make))

(define-primop (cons <obj> <obj>) => <pair>
  (bytecode 'cons)
  (ccode "cons"))

(define-primop (car <obj>) => <obj>
  (bytecode 'car)
  (ccode "checked_car"))

(define-primop (cdr <obj>) => <obj>
  (bytecode 'cdr)
  (ccode "checked_cdr"))
