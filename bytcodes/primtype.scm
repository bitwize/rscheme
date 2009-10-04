#|------------------------------------------------------------*-Scheme-*--|
 | File:    bytcodes/primtype.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.10
 | File mod date:    2003-12-15 09:32:17
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  compiler
 |
 | Purpose:          Define the primitive-type system
 |------------------------------------------------------------------------|
 | Notes:
 |      This file is the central and (almost) sole repository of
 |      primtype information
 |      
 |      It provides information to the following subsystems:
 |      
 |      The cross-compiler (compiler/)
 |      The compiler (src/compiler/)
 |      The code generator (src/codegen/)
 |      The bytecode generator (bytcodes/process.scm)
 |      
 |      Because the various subsystems have different metaphors
 |      and optimizations for initializer code, the client package
 |      must call the (primtype-setup) function before using any
 |      of the facilities from this file
 `------------------------------------------------------------------------|#

;;
;;  a primtype descriptor is used to describe each primitive type
;;

(define-class <prim-type> (<object>)
  name
  c-type-name
  eval-stack-union-member-stem
  ;;
  ;; this is a list mapping prim-types to lists of coercion primops
  ;; currently, we only use empty and single-element lists, but
  ;; future possibilities abound :-)
  ;;
  coerces-to
  super-type
  sub-types
  equivalent-types
  recognizer-primop
  preferred-class)


(define-write-method ((self <prim-type>) port)
  (format port "#[prim ~s]" (name self)))

(define *primtype-table* '#uninit)
(define *coercion-primop-table* '#uninit)

(define (lookup-prim-type name)
  (or (table-lookup *primtype-table* name)
      (error "`~s' does not name a primitive type" name)))

;; returns a <pair> of primtype names
;;  (source-type . dest-type)
;; or #f

(define (primop-does-coercion (primop-name <symbol>))
  (table-lookup *coercion-primop-table* primop-name))

;;

(define (prim-type-coercion (from-type <prim-type>)
			    (to-type <prim-type>)
			    (coercer <symbol>))
  ;;
  ;; install such a conversion for all the ancestors of the to-type
  ;;
  (let loop ((t to-type)
	     (l '()))
    (if t
	(begin
	  (table-insert! *coercion-primop-table*
			 coercer
			 (cons (name from-type) (name t)))
	  (loop (super-type t) (cons (list t coercer) l)))
	(set-coerces-to! from-type (append (coerces-to from-type) l)))))

(define (prim-type-coercion-chain (from-type <prim-type>)
				  (to-type <prim-type>)
				  coercer-list)
  ;;
  ;; install the conversion for all the ancestors of the to-type
  ;;
  (let loop ((t to-type)
	     (l '()))
    (if t
	(loop (super-type t) (cons (cons t coercer-list) l)))
	(set-coerces-to! from-type (append (coerces-to from-type) l))))

;;

(define (new-prim-type name
		       recognizer
		       c-type-name
		       eval-stack-union-member-stem
		       super-type
		       pref-class)
  (let ((t (make <prim-type>
		 name: name
		 c-type-name: c-type-name
		 eval-stack-union-member-stem: eval-stack-union-member-stem
		 coerces-to: '()
		 super-type: super-type
		 recognizer-primop: recognizer
		 sub-types: '()
		 preferred-class: pref-class
		 equivalent-types: '())))
    (if super-type
	(set-sub-types! super-type (cons t (sub-types super-type))))
    (if (table-lookup *primtype-table* name)
	(error "primitive type `~s' already defined" name))
    (table-insert! *primtype-table* name t)
    t))

(define-syntax (new-prim-type* name
			       recognizer
			       c-type-name
			       eval-stack-union-member-stem
			       super-type
			       pref-class)
  (new-prim-type (mquote name)
		 (mquote recognizer) 
		 c-type-name
		 eval-stack-union-member-stem
		 super-type
		 (mquote pref-class)))

(define-syntax (new-raw-type name c-type-name stem pref)
  (new-prim-type* name #f c-type-name stem #f pref))

(define-syntax (new-obj-prim-type name recognizer super-type)
  (new-prim-type* name recognizer "obj" "obj" super-type name))

(define-syntax (new-obj-prim-type* name recognizer super-type pref)
  (new-prim-type* name recognizer "obj" "obj" super-type pref))

(define (primtype-raw? (self <prim-type>))
  (not (recognizer-primop self)))

(define (primtype-setup)
  (set! *primtype-table* (make-symbol-table))
  (set! *coercion-primop-table* (make-symbol-table))
  (let* ((<obj> (new-obj-prim-type <obj> #f #f))

	 (<ptr> (new-obj-prim-type* <ptr> ptr? <obj> <obj>))
	 (<immob> (new-obj-prim-type* <immob> immob? <obj> <obj>))
	 (<fixnum> (new-obj-prim-type <fixnum> fixnum? <obj>))

	 (<gvec> (new-obj-prim-type* <gvec> gvec? <ptr> <obj>))
	 (<bvec> (new-obj-prim-type* <bvec> bvec? <ptr> <obj>))

	 (<pair> (new-obj-prim-type <pair> pair? <gvec>))
	 (<symbol> (new-obj-prim-type <symbol> symbol? <gvec>))
	 (<vector> (new-obj-prim-type <vector> vector? <gvec>))
	 (<function> (new-obj-prim-type <function> function? <gvec>))
	 (<<class>> (new-obj-prim-type <<class>> class? <gvec>))
	 
	 (<string> (new-obj-prim-type <string> string? <bvec>))
	 (<double-float> (new-obj-prim-type <double-float> 
					    double-float? 
					    <bvec>))
	 (<long-int> (new-obj-prim-type <long-int> long-int? <bvec>))

	 (<ascii-char> (new-obj-prim-type <ascii-char> boolean? <immob>))
	 (<unicode-char> (new-obj-prim-type <unicode-char> boolean? <immob>))
	 (<boolean> (new-obj-prim-type <boolean> boolean? <immob>))
	 ;;
	 ;; now, on to some others
	 ;;
	 (<raw-float> (new-raw-type <raw-float> "IEEE_64" "raw_float" <double-float>))
	 (<raw-float-32> (new-raw-type <raw-float-32> 
				       "IEEE_32" 
				       "raw_float_32" <double-float>))
	 (<raw-int> (new-raw-type <raw-int> "INT_32" "raw_int" <fixnum>))
	 (<raw-int-64> (new-raw-type <raw-int-64> "INT_64" "raw_int_64" <long-int>))
	 (<raw-int-bytes> (new-raw-type <raw-int-bytes> "INT_32" "raw_int" <fixnum>))
	 (<raw-string> (new-raw-type <raw-string> "char *" "raw_str" <string>))
	 (<raw-bool> (new-raw-type <raw-bool> "rs_bool" "raw_bool" <boolean>)))
    ;;
    ;; (one little hack :-)
    ;;
    ;; or is it? ... (set-preferred-class! <obj> '<object>)
    ;;
    ;; define the coercions among these
    ;;
    (for-each
     (lambda (coercions-from-a-type)
       (let ((from-type (lookup-prim-type (car coercions-from-a-type)))
	     (coercion-list (cdr coercions-from-a-type)))
	 (for-each (lambda (to-type-name coercer)
		     (prim-type-coercion from-type 
					 (lookup-prim-type to-type-name)
					 coercer))
		   (map car coercion-list)
		   (map cadr coercion-list))))
     ;;
     ;;  COERCIONS
     ;;
     ;;  *********************** BIG HUGE NOTICE *************************
     ;;  **  the order that the coercions is listed is IMPORTANT, at
     ;;  **  least in the current implementat.  The system will pick
     ;;  **  the first one it finds
     ;;  **  (the order in the <prim-type> coerces-to is the same
     ;;  **  as given here)
     ;;  **
     ;;  **  for example, consider <raw-int-64> and <obj>
     ;;  **  a <raw-int-64> can be turned into a <fixnum>, which is
     ;;  **  and <obj>, but it can also be turned into a <long-int>,
     ;;  **  which is also an <obj>.  For conversions to <obj>, the
     ;;  **  latter is chosen because it is listed first.
     ;;  *****************************************************************
     ;;
     '((<obj> (<raw-bool> obj->raw-bool))
       (<double-float> (<raw-float> double-float->raw-float)
		       (<raw-bool> true))
       (<raw-float> (<double-float> raw-float->double-float)
		    (<raw-bool> true)
		    (<raw-float-32> raw-float->raw-float-32))
       (<long-int> (<raw-int-64> long-int->raw-int-64)
		   (<raw-bool> true))
       (<raw-int-64> (<raw-int> raw-int-64->raw-int)
		     (<long-int> raw-int-64->long-int)
		     (<raw-bool> true)
		     (<fixnum> raw-int-64->fixnum))
       (<fixnum> (<raw-int> fixnum->raw-int)
		 (<raw-int-64> fixnum->raw-int-64)
		 (<raw-bool> true)
		 (<raw-int-bytes> fixnum-words->raw-int-bytes))
       (<raw-bool> (<boolean> raw-bool->bool))
       (<raw-int> (<fixnum> raw-int->fixnum)
		  (<raw-int-64> raw-int->raw-int-64)
		  (<raw-bool> true)
		  ;(<long-int> raw-int->long-int)
		  (<raw-int-bytes> raw-int-words->raw-int-bytes))
       (<raw-int-bytes> (<fixnum> raw-int-bytes->fixnum-words)
			(<raw-bool> true)
			(<raw-int> raw-int-bytes->raw-int-words))
       (<string> (<raw-string> string->raw-str)
		 (<raw-bool> true))
       (<raw-string> (<string> raw-str->string)
		     (<raw-bool> true))))
    ;;
    ;;  COMPOSED COERCIONS
    ;;  (the primtype system doesn't automatically infer coercion
    ;;  chains, because I'm not sure yet that it should, so we explicitly
    ;;  state the valid coercion compositions)
    ;;
    (for-each 
     (lambda (chain)
       (prim-type-coercion-chain
	(lookup-prim-type (car chain))
	(lookup-prim-type (cadr chain))
	(cddr chain)))
     '((<raw-int> <long-int> raw-int-64->long-int raw-int->raw-int-64)
       (<fixnum> <long-int> raw-int-64->long-int fixnum->raw-int-64)))))

;;
;;  checks to see if a value of a given primitive type
;;  is "compatible" with the primitive type of a storage 
;;  location or type restriction
;;
;;  for example, a <gvec> is compatible with an <obj>
;;               a <raw-int> is compatible with a <fixnum>
;;               a <fixnum> is compatible with a <raw-int>
;;
;;  specifically, a value-type A is compatible with a required-type B
;;  iff A is a subtype of B (or the same type)
;;      *OR* an A any supertype of A is B or can be coerced into one
;;

(define (prim-compatible-type? (value-type <prim-type>)
			       (required-type <prim-type>))
  (and (prim-conversion value-type required-type)
       #t))

;;
;;  returns a list of coercion primops, or #f if the conversion
;;  cannot be done
;;
;;  returns the empty list if the coercion is a nop
;;
       
(define (prim-conversion (value-type <prim-type>)
			 (required-type <prim-type>))
  ;;
  ;; first, make sure we do nothing for a subtype relationship
  ;;
  (if (prim-subtype? value-type required-type)
      '()
      (let loop (((t <prim-type>) value-type))
	(if (eq? t required-type)
	    (error "prim-conversion: ~s => ~s is strangely not subtypal"
		   value-type
		   required-type)
	    (let ((m (assq required-type (coerces-to t))))
	      (if m
		  (cdr m)
		  (let ((p (super-type t)))
		    (if p
			(loop p)
			#f))))))))

;;
;;  a subtype is stronger than a compatible type -- it
;;  is a claim that the ACTUAL REPRESENTATION is the same
;;
;;  That is, a <raw-int> can be compatible with an <obj> (via <fixnum>),
;;  but it isn't a subtype because the actual representation isn't
;;  the same

(define (prim-subtype? (value-type <prim-type>)
		       (required-type <prim-type>))
  (let loop (((t <prim-type>) value-type))
    (if (eq? t required-type)
	#t
	(let ((p (super-type t)))
	  (if p
	      (loop p)
	      #f)))))
