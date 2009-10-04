#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/compiler/primtype.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.10
 | File mod date:    1997-11-29 23:10:35
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  compiler
 |
 | Purpose:          Primitive-type recognition and mapping
 |------------------------------------------------------------------------|
 | Notes:
 |      this file defines the interface between the primitive type
 |      system (which is the set of types well known to the compiler)
 |      and the user's type system
 `------------------------------------------------------------------------|#

(define (prim-type->class type-name)
  (let ((pc (preferred-class (lookup-prim-type type-name))))
    (xform (well-known (if (eq? pc '<obj>)
			   '<object>
			   pc))
	   'value)))

(define (ct-unraw-type ct-type)
  (if (symbol? ct-type)
      (preferred-class (lookup-prim-type ct-type))
      ct-type))

(define (ct-unprim-type ct-type)
  (if (symbol? ct-type)
      (prim-type->class ct-type)
      ct-type))

(define (ct-type->prim-type-name compiler-type)
  (if (symbol? compiler-type)
      ;; already a primtype
      compiler-type
      (target-class->prim-type-name (actual-value compiler-type))))

(define (native-class->prim-type-name class)
  ;;
  ;; BIG NOTICE
  ;; this just calls target-class->prim-type-name, because the cross compiler
  ;; is configured to support `class-name', `class-superclasses', 
  ;; and `heap-type' on native class objects
  ;;
  ;; the class being passed is NOT a target-class -- this is just 
  ;; code sharing hackery
  ;;
  (target-class->prim-type-name class))

(define (target-class->prim-type-name class)
  ;;
  ;;  BIG NOTICE
  ;;
  ;;  this is kind of hacked, for the moment, to leverage off
  ;;  names instead of bindings and objects.  Really, a <<class>>
  ;;  should admit what its prim-type is
  ;;
  (let loop ((c class))
    (let* ((n (class-name c))
	   (b (table-lookup *primtype-table* n)))
      (if b
	  ;; there is a primtype by this name, so that's it
	  ;; (assume for the moment (see big notice, above)
	  ;; people aren't making classes with names like <raw-int>)
	  n
	  ;;
	  ;; if it doesn't name itself (ie, #[the class <string>] ==> <string>)
	  ;; then try it's parent, unless there is none, in which case,
	  ;; it's <obj>
	  ;;
	  (let ((p (tclass-supers c)))
	    (if (pair? p)
		(loop (actual-value (car p)))
		;;
		;; we couldn't find any ancestor in the primtype table
		;; -- check it's heap type and use the rule that
		;; a concerete class cannot have subclasses
		;; that are of a different form (ie, a <vector>,
		;; which is a <gvec>, cannot have a subclass <string>
		;; which is a <bvec>.)  This rule implies, for example, that
		;; any subclass of a class which describes gvecs
		;; will describe a gvec.
		;; WARNING: This rule isn't yet enforced by the rest
		;; of the objsys (ie, define-class compiler)
		(case (heap-type class)
		  ((0) '<gvec>)
		  ((1) '<bvec>)
		  ((2) '<immob>)
		  ;; an abstract class that has slots can only have gvec
		  ;; concrete instances
		  ;; (this recognition is necessary in order to make 
		  ;; getter inlining work, because gvec-ref needs a <gvec>)
		  ((3) (if (null? (tclass-slots class))
			   '<obj>
			   '<gvec>))
		  ;; `:weak1' objects are a special kind of gvec
		  ((4) '<gvec>)
		  (else '<obj>))))))))
