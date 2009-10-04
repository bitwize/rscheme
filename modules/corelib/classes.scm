#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/corelib/classes.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.39
 | File mod date:    2004-03-24 14:38:27
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  precore
 |
 | Purpose:          standard, core system classes
 `------------------------------------------------------------------------|#

(define-class <object> () :abstract)

(define-generic-function initialize)
(define-generic-function finalize)   ;; only invoked on registered objects

;;; default write-object is `#[classname tostring]'
;;; default display-object is `tostring'

(define-generic-function display-object)
(define-generic-function write-object)

;;; default tostring is `@xxxx_yyyy' (ie, objbits)

(define-generic-function to-string)

;;; there is no default hash-code method, by design.
;;; the only reasonable default, transient->hash, is too
;;; dangerous to make commonly available (ie, its semantics
;;; are too tricky)

(define-generic-function hash-code)

;;;  -----------
;;;  collections
;;;  -----------
;;;  (based on Dylan(TM))

(define-class <collection> (<object>) :abstract)

;; (size <collection>)
(define-generic-function size)

;; (element <collection> key #key default) #signals <no-such-key>
(define-generic-function element)

;; (set-element! <collection> key value) #signals <no-such-key>
(define-generic-function set-element!)

(define-class <sequence> (<collection>) :abstract)

(define-class <vector> (<sequence>))
(define-class <list> (<sequence>) :abstract)
(define-class <pair> (<list>))
(define-class <empty-list> (<list>) :immob)

(define-class <abstract-string> (<sequence>) :abstract)

(define-class <octet-string> (<abstract-string>) :bvec)
(define-class <string> (<octet-string>) :bvec)

(define-class <unicode-string> (<abstract-string>) :bvec)

;;;
;;;  ----------
;;;  characters
;;;  ----------
;;;  these are the elements of strings

(define-class <char> (<object>) :abstract)

;;; should this be `<byte-char>', since these are constituents
;;; of `<byte-string>', and it is really ISO-8859-1, a superset
;;; of ASCII and subset of Unicode?

(define-class <ascii-char> (<char>) :immob)
(define-class <unicode-char> (<char>) :immob)

;;;  ----------------------
;;;  quantities and numbers
;;;  ----------------------

(define-class <quantity> (<object>) :abstract)

(define-class <number> (<quantity>) :abstract)

(define-class <complex> (<number>) :abstract)

(define-class <rect-complex> (<complex>)
  re im)

(define-class <real> (<complex>) :abstract)
(define-class <float> (<real>) :abstract)
(define-class <rational> (<real>) :abstract)

(define-class <mp-rational> (<rational>)
  mp-rat-numerator
  mp-rat-denominator)

(define-class <integer> (<rational>) :abstract)
(define-class <bignum> (<integer>) image-mode: 9
  limb-cap
  limb-size
  mp-data)
  
(define-class <mp-data> (<object>) :bvec)

(define-class <fixnum> (<integer>) :immob)
(define-class <double-float> (<float>) :bvec)
(define-class <long-int> (<integer>) :bvec)

(define-class <boolean> (<object>) :immob)
(define-class <unique-obj> (<object>) :immob)

;;  spare (unused) secondary tag indicators...

(define-class <spare-1> (<object>) :immob)
(define-class <spare-2> (<object>) :immob)
(define-class <spare-3> (<object>) :immob)

(define-class <byte-vector> (<object>) :bvec)

;;
;;  <byte-coded> things are bvecs that contain byte-coded programs
;;  typically, they occur in slot[3] of a <template> 

(define-class <byte-coded> (<object>) :bvec)

;; <symbol>'s are defined here to have image-mode: 0
;; so that when THIS image is being written out
;; (ie, as a boot image), symbols are written out
;; as regular objects instead of in SYMBOL mode

(define-class <symbol> (<object>) #|image-mode: 2|#
  (string-value allocation: internal)
  (hash-value allocation: internal))

;;  instances of <binding-envt> grow to fit the needs...

(define-class <binding-envt> (<object>)
  enclosing)

;; a function is the class of callable objects.
;; it is abstract

(define-class <function> (<object>) :abstract
  class-category: 1
  template)

;; a closure is what you get from `lambda'
;; it's a minimal-functionality (sorry) function

(define-class <closure> (<function>)
  environment)

;; a method is what you get with define-method
;; and is something that's designed to be put in
;; a generic function
;; (in particular, the compilation of a method
;;  may assume that the function specializers have
;;  already been checked (currently, it should only
;;  assume that about the first, because we only
;;  have single-method dispatch))

(define-class <method> (<closure>)
  function-specializers
  (sync-method init-value: #f))

;; useful info is hung off the <gf> itself, because they are not
;; created on the fly like <closure>'s are, and it makes more sense to
;; put it there (since they aren't created on the fly, it doesn't hurt
;; to make them big)

(define-class <generic-function> (<function>)
  ;; NOTE: `generic-function-methods' has to be a sealed slot,
  ;;       or else `find-method-by-class' in the objsys module
  ;;       will get into loop
  (generic-function-methods :sealed)                               ;; [1]
  function-specializers                                            ;; [2]
  generic-function-name)                                           ;; [3]

;;; **NOTE** This must agree with the defn of <target-gf1> in
;;; ../../compiler/target.scm

(define-class <single-dispatch-gf> (<generic-function>)
  (gf-cache-0-k init-value: #f :sealed)
  (gf-cache-0-v init-value: #f :sealed) ;; [4 5]
  (gf-cache-1-k init-value: #f :sealed)      
  (gf-cache-1-v init-value: #f :sealed) ;; [6 7]
  (gf-cache-2-k init-value: #f :sealed)      
  (gf-cache-2-v init-value: #f :sealed) ;; [8 9]
  (gf-cache-3-k init-value: #f :sealed)      
  (gf-cache-3-v init-value: #f :sealed) ;; [10 11]
  (gf-cache-V-k init-value: #f :sealed)      
  (gf-cache-V-v init-value: #f :sealed) ;; [12 13]
  (gf-cache-overflow init-value: #f :sealed)
  ;; this had better be sealed, or `load-cache' will die in a loop
  ;; (trying to load the cache for the miss-count accessor)
  (miss-count type: <fixnum> init-value: 0 :sealed)
  (properties type: <vector> init-value: '#()))

(define-class <type> (<object>) :abstract)

;; this much information is known to the runtime system...

;;; **NOTE** This must be in sync with <<target-class>>
;;; in ../../compiler/target.scm

(define-class <<class>> (<type>)
     class-category: 2
  class-name
  heap-type
  image-mode
  superclasses
  (class-category type: <fixnum> init-value: 0)
  ;
  ;  (an ad hoc analysis of 310 class hash values indicates
  ;   1 4-way, 1 3-way, and 42 2-way collisions, (and 219 non-colliding)
  ;   when using the 10 low bits)
  ;  class-hash == rehash of class-name's hash
  (class-hash type: <fixnum> init-value: 0 :sealed)
  (properties type: <vector> init-value: '#()))

;; the rest is high-level...

;;; **NOTE** This must be in sync with <<target-class>>
;;; in ../../compiler/target.scm

(define-class <<standard-class>> (<<class>>)
  direct-slots
  all-slots
  instance-size
  corresponding-primtype
  class-precedence-list
  (spare-0 init-keyword: #f getter: #f setter: #f init-value: #f))

(define-class <slot-descriptor> (<object>)
    name
    initialization-mode		;; required, optional, prohibited
    init-value
    type-restriction
    (index type: <fixnum>)
    init-keyword
    getter
    setter
    (properties type: <list> init-value: '()))


(define-class <template> (<object>) image-mode: 4
  code-pointer
  linkage-info
  function-descr)

(define-class <partial-continuation> (<object>) image-mode: 5)

(define-class <binding> (<object>)
  name)

(define-class <top-level-var> (<binding>)
  value
  (write-prot init-value: #f
	      type: <boolean>))

(define-class <substitution> (<binding>)
    expr
    envt)   ;; this is the envt. that the expr COMES FROM

(define-class <macro> (<binding>)
    envt
    forms
    else-bdg
    (setter-forms init-value: '()))

(define-class <rewriter> (<binding>)
  rewriter-body    ;; something like '((cons 'foo (cdr form)))
  rewriter-args)   ;; type '(form)

;; a rewriter that has access to it's environment of definition

(define-class <local-rewriter> (<rewriter>)
  rewriter-lex-envt
  rewriter-dyn-envt)

;; note-- this has to be in sync with [base]/compiler/cmplr/bdgs.bbi's <primop>

(define-class <primop> (<binding>)
    (arg-types init-value: '() type: <list>)
    (result-type init-value: '<obj>)
    (rest-type init-value: #f)
    (translations init-value: '() type: <list>)
    (full-procedure-bdg init-value: #f)
    (primop-has-side-effect? init-value: #t)) ;; #f for side-effecting primops



;; a <scope-record> is the compile-time representation
;; of some kind of binding envt.

(define-class <scope-record> (<object>) :abstract)

(define-class <lexical-contour> (<scope-record>)
  name->bindings
  bindings
  lexical-enclosing
  dynamic-enclosing)

(define-class <top-level-contour> (<scope-record>)
  (table init-value: #f)
  (owner init-value: #f)
  (backing init-value: #f)
  (dirty? init-value: #f))

(define-class <macro-form> (<object>)
    args
    body)

#|
	     (<top-level-envt> :gvec)
	     (<imported-binding> :gvec)

	     (<patch> :gvec)
	     (<part-descr> :gvec image-mode: 3)

	     (<module> :gvec)
	     (<binding> :abstract
			(<primop> :gvec)
			(<top-level-var> :gvec)
			(<definer> :gvec))
	     (<byte-coded> :bvec)

	     (<link-cmd> :abstract
			 (<link-xform> :gvec)
			 (<link-value> :gvec)
			 (<link-method> :gvec)
			 (<link-bdgs> :gvec))

	     (<imported-module> :gvec)

	     (<table-bucket> :gvec)
	     (<table> :abstract
		      (<symbol-table> :gvec)
		      (<string-table> :gvec)
		      (<string-ci-table> :gvec)
		      (<object-table> :gvec)
		      (<eq-table> :gvec))

	     (<list> :abstract 
		     (<pair> :gvec)
		     (<empty-list> :immob))))
|#


(define-class <gvec> (<object>) :gvec)
(define-class <bvec> (<object>) :bvec)

(define-class <dequeue> (<object>)
  (state type: <vector>)
  (front type: <fixnum> init-value: 0)
  (back type: <fixnum> init-value: 0))

;;;

(define-class <winding-contour> (<object>) :abstract)

;;;=======================================================================

(define-class <slot-method> (<method>)
  (index type: <fixnum>)
  (type-restriction type: <<class>>)
  (slot-descriptor type: <slot-descriptor>))

(define-class <getter> (<slot-method>))

(define-class <setter> (<slot-method>))

;;;========================================================================
;;;  (used in `fluid.scm', but optimized in this files' functions)

(define-class <fluid-tl-contour> (<winding-contour>)
  ftlc-bindings
  ftlc-inside-values
  ftlc-saved-values)

;;;
;;;  an abstraction for where conditions might arise
;;;

(define-class <place> (<object>) :abstract)

;;; this subclass applies to both bytecoded functions (where the place
;;; code is the offset of the next BC instruction), C-coded 
;;; functions (where the place code is just an index code)
;;;
;;; this is the object constructed by the make_function_place() function in
;;; entry.c

(define-class <function-place> (<place>)
  (template type: <template>)
  (function-place-code type: <fixnum>))

;;;
;;;  some generic generic functions...
;;;

(define-generic-function input-port)
(define-generic-function output-port)

(define-generic-function set-input-port!)
(define-generic-function set-output-port!)

;;;

(define-class <condition> (<object>) :abstract
  (properties type: <list> init-value: '()))


(define-class <simple-warning> (<condition>)
  (simple-condition-msg type: <string> 
			init-value: "warning condition signalled")
  (simple-condition-args type: <list>
			 init-value: '()))


(define-class <serious-condition> (<condition>) :abstract)

;; an error is a condition with no restart protocol

(define-class <error> (<serious-condition>) :abstract)

(define-class <os-error> (<error>)              ; os_error_class in C code
  (error-number type: <fixnum>)
  (system-call type: <string>)
  (arguments type: <vector>))

(define-class <simple-error> (<error>)
  (simple-condition-msg type: <string> init-value: "error signalled")
  (simple-condition-args type: <list> init-value: '()))

(define-class <type-check-failed> (<error>)
  (type-check-required-type type: <symbol>)
  type-check-actual-value
  (place type: <place>))

(define-class <argument-error> (<error>)
  (argument-error-function-name type: <symbol>)
  (argument-error-arguments type: <list>));; may be '() if not easily recovered

;;;
(define-class <argument-type-error> (<argument-error>)
  argument-error-bad-arg
  argument-required-type)

(define-generic-function string-length)

(define-class <substring> (<abstract-string>)
  (basis type: <abstract-string>)
  (offset type: <fixnum>)
  (length type: <fixnum>)
  (string-length type: <fixnum>))

(define-generic-function string-ref)
(define-generic-function string-set!)
