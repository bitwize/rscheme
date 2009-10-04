#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/repl/make.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.3
 | File mod date:    1997-11-29 23:10:31
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  repl
 |
 | Purpose:          code to compile `make'
 `------------------------------------------------------------------------|#

(define (runtime-instance-maker class-ic initializers lex-envt dyn-envt mode)
  (make <ic-call>
	function: (tl-ref-well-known 'make-instance)
	mode: mode
	args: (make <ic-multi>
		    arg-list: (cons class-ic initializers))))

;; the current environment is the environment where the "make"
;; form itself is defined, because this is treated like a macro,
;; not a special form

;; the objsys module provides a function for initializing 
;; runtime-determined classes called make-instance
;; and it is called with 1+2n arguments, where n is the number of keywords
;; specified in the make form:
;; 	a <<class>>
;;	keyword[0]  (a <symbol>)
;;	value[0]
;;      keyword[1]
;;	value[1]
;;	...
;; that way, make-instance can say (class . inits) and get a similar
;; effect to that of #rest in Dylan with keyword arguments
;;
;; make-instance returns a fully initialized instance
;;
;; compile-make is invoked as if by:
;;   (define-syntax (make class . args) ...)

(define (compile-make sf form lex-envt dyn-envt mode)
  (let ((class-ic (compile (cadr form) lex-envt dyn-envt 'value)))
    ;;
    ;; currently only do full calls to make-instance...
    ;;
    (runtime-instance-maker
     class-ic
     (compile-keyword-list (cddr form) lex-envt dyn-envt)
     lex-envt
     dyn-envt
     mode)))
