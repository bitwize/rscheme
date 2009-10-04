#|------------------------------------------------------------*-Scheme-*--|
 | File:    compiler/target.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.13
 | File mod date:    2003-10-13 13:01:45
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  (rsc)
 |
 `------------------------------------------------------------------------|#

;;; must be in sync with <<class>> and <<standard-class>>

(define-class <<target-class>> (<object>)
  class-name
  heap-type
  image-mode
  superclasses
  (class-category type: <fixnum> init-value: 0)
  (class-hash type: <fixnum> init-value: 0)
  (properties type: <vector> init-value: '#())
  ;;
  direct-slots
  all-slots
  instance-size
  corresponding-primtype
  class-precedence-list
  (spare-0 init-keyword: #f getter: #f setter: #f init-value: #f))

(add-mifio-class "<<standard-class>>" <<target-class>>)

;; the variable <<target-class>> is defined as a regular variable in
;; modules/compiler/classes.scm; we need to make it a constant...

(set-write-prot! (& <<target-class>>) #t)

;;;

(define-class <target-function> (<object>)
  template)

(define-class <target-closure> (<target-function>)
  environment)

(define-class <target-method> (<target-closure>)
  function-specializers
  (sync-method init-value: #f))

(define-class <target-gf1> (<target-function>)
  generic-function-methods
  function-specializers
  generic-function-name
  (gf-cache-0-k init-value: #f)      (gf-cache-0-v init-value: #f) ;; [4 5]
  (gf-cache-1-k init-value: #f)      (gf-cache-1-v init-value: #f) ;; [6 7]
  (gf-cache-2-k init-value: #f)      (gf-cache-2-v init-value: #f) ;; [8 9]
  (gf-cache-3-k init-value: #f)      (gf-cache-3-v init-value: #f) ;; [10 11]
  (gf-cache-V-k init-value: #f)      (gf-cache-V-v init-value: #f) ;; [12 13]
  (gf-cache-overflow init-value: #f)
  (miss-count type: <fixnum> init-value: 0)
  (properties type: <vector> init-value: '#()))

(add-mifio-class "<function>" <target-function>)
(add-mifio-class "<closure>" <target-closure>)
(add-mifio-class "<method>" <target-method>)
(add-mifio-class "<single-dispatch-gf>" <target-gf1>)

;;;

(define-class <target-slot-method> (<target-method>)
  (index type: <fixnum>)
  type-restriction ;; a <<target-class>> or a <patch>
  (slot-descriptor type: <slot-descriptor>))

(define-class <target-getter> (<target-slot-method>))

(define-class <target-setter> (<target-slot-method>))

(add-mifio-class "<getter>" <target-getter>)
(add-mifio-class "<setter>" <target-setter>)

#|
(define $target-classes (list <<target-class>>
			      <target-getter>
			      <target-setter>))
|#
;;;

(define-syntax (target-class? o)
  (instance? o <<target-class>>))

;;;

;; class introspection

;; return a list of all the class's <slot-descriptor>'s (including inherited)

(define (tclass-slots c)
  (if (null? (tclass-supers c))
      (tclass-direct-slots c)
      (append (tclass-slots (car (tclass-supers c)))
	      (tclass-direct-slots c))))

(define (tclass-supers c)
  (superclasses (actual-value c)))

(define (tclass-direct-slots c)
  (direct-slots (actual-value c)))

(define (tclass-precedence-list (c <<target-class>>))
  (if (null? (superclasses c))
      (list c)
      (cons c (tclass-precedence-list (car (superclasses c))))))

(define (target-expr-value expr lex-envt dyn-envt)
  (let ((ic (compile expr lex-envt dyn-envt 'value)))
    (if (compile-time-const? ic)
	(compile-time-const-value ic)
	(error "~s: not a constant expression" expr))))

(define (method-dispatch-class (m <target-method>))
  (car (function-specializers m)))

(define (find-method-by-class (gf <target-gf1>) class)
  (let loop ((i (generic-function-methods gf)))
    (if (pair? i)
	(if (target-subclass? class (method-dispatch-class (car i)))
	    (car i)
	    (loop (cdr i)))
	#f)))

(define-method write-object ((self <<target-class>>) port)
  (format port "#[<<target-class>> ~s]" (class-name self)))

;;;
;;;  standard ones
;;;

(mifio-class "<symbol>" <symbol>)
(mifio-class "<vector>" <vector>)
(mifio-class "<string>" <string>)
(mifio-class "<pair>" <pair>)
(mifio-class "<double-float>" <double-float>)
(mifio-class "<table>" <table>)
(mifio-class "<string-table>" <string-table>)
(mifio-class "<string-ci-table>" <string-ci-table>)
(mifio-class "<object-table>" <object-table>)
(mifio-class "<eq-table>" <eq-table>)
(mifio-class "<table-bucket>" <table-bucket>)
(mifio-class "<long-int>" <long-int>)

;;
;;  these are built in to 0.6, but not 0.5

(mifio-class "<top-level-var>" <top-level-var>)
(mifio-class "<top-level-contour>" <top-level-contour>)

;; note that a <lexical-contour> can occur in a module image if
;; a <macro> captures a contour containing macros.  This may be
;; a good reason right here to distinguish variable-type contours
;; from syntactic contours, since the latter preserve top-levelness
;; and can be in an image, wheras the former do not.

(mifio-class "<lexical-contour>" <lexical-contour>)

(mifio-class "<binding-envt>" <binding-envt>)
(mifio-class "<slot-descriptor>" <slot-descriptor>)

(mifio-class "<rewriter>" <rewriter>)
(mifio-class "<macro>" <macro>)
(mifio-class "<macro-form>" <macro-form>)
(mifio-class "<substitution>" <substitution>)
(mifio-class "<primop>" <primop>)

(mifio-class "<template>" <template>)

;;;

(mifio-class "<bignum>" <bignum>)
(mifio-class "<mp-rational>" <mp-rational>)

(mifio-class "<mp-data>" <mp-data>)
