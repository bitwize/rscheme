#|------------------------------------------------------------*-Scheme-*--|
 | File:	    rs/backend/c/module.scm
 |
 |          Copyright (C)1998 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |	    See <http://www.rscheme.org/> for the latest info.
 |
 | File version:     1.3
 | File mod date:    2005-02-18 15:58:17
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  &module;
 |
 | Purpose:          &purpose;
 `------------------------------------------------------------------------|#

(define-module rs.backend.c ()
  (&module
   (import usual-inlines)
   ;
   (import mlink tables codegen paths syscalls)
   (import rs.util.properties rs.util.msgs))
  ;
  (define-message-table rs.backend.c 415)
  ;
  (&module
   ;
   (load "util.scm")
   (load "asmport.scm")
   (load "cident.scm")
   ;
   (load "assem.scm")
   (load "monotones.scm")
   (load "wrvinsn.scm")
   ;
   (load "parts.scm")
   (load "disclaimer.scm")
   (load "unitlink.scm")
   (load "ccompile.scm")
   (load "cload.scm")
   ;;
   ;;
   (import compiler)
   (load "pragma.scm")
   (load "parseglue.scm")
   (load "defglue.scm")
   ;;
   ;(load "rs-glue.scm") ;; construct the `rs.glue' module
   ;;
   ;; `lazy.scm' actually uses define-glue, 
   ;; so it must come after `rs-glue.scm'.
   ;; it also side-effects some variables in
   ;; this module, like `make-trampoline-template',
   ;; which is a seriously wierd bootstrapping
   ;; tweak.
   (load "lazy.scm")
   ;;
   (export aml->lazy-ccode-template
	   template-policy-switch
	   flush-all-code
	   set-codegen-policy!
	   cload cload-into)
   ))
