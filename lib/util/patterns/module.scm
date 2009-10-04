#|------------------------------------------------------------*-Scheme-*--|
 | File:    %p%
 |
 |          Copyright (C) 2005 Donovan Kolbly <donovan@rscheme.org>
 |          as part of the RScheme project, licensed for free use
 |
 | Version: %I%
 | Date:    %E% %U%
 | Build:   %b%
 |
 | Purpose: Datum-based pattern facilities
 |
 `------------------------------------------------------------------------|#

#|

SIMPLE USAGE:

    (define-patterns munge
      ((hello ?x)    ?x)
      ((hello ?x ?y) ?y))

    (munge '(hello 3))            ==> 3
    (munge '(hello 3 4))          ==> 4

|#

(define-module util.patterns ()
  (&module
   (import usual-inlines tables)
   (load "bindings.scm")
   (load "patspace.scm")
   (load "patmatch.scm")
   (load "transform.scm")
   (load "sublis.scm")
   (export define-patterns 
           make-patterns
           transform-using-patterns
           <patterns> 
	   add-rule
           make-rule
	   add-constant
	   cons-dif)
   ;;
   ;; lower level, but much lighter weight setup...
   (export sublis)))
