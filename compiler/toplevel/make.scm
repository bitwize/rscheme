#|------------------------------------------------------------*-Scheme-*--|
 | File:    compiler/toplevel/make.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.10
 | File mod date:    1999-01-01 15:47:20
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  (rsc)
 |
 | Purpose:          `make' special form compiler
 `------------------------------------------------------------------------|#

;; steal some code from the source

;;
;; dispatches to either a full-runtime version that
;; calls 'make-instance' or a compile-time version
;; which calls 'make-gvec' and 'initialize'

(define (compile-instance-maker class-ic initializers lex-envt dyn-envt mode)
  ((if (class-constant? class-ic)
       ctime-instance-maker
       runtime-instance-maker)
   class-ic
   (compile-keyword-list initializers lex-envt dyn-envt)
   lex-envt
   dyn-envt
   mode))

;; generated code looks something like:
;;
;;  (let ((temp (make-gvec <class> <s0> ... <sN>)))
;;    (initialize temp <k1> <v1> ...)
;;    temp)
;;
;; where the <si> are potentially type-checked icode for the
;; slot initializers

(define (hideous-class-hack slot-d lex-envt dyn-envt)
  (let ((n (class-name (actual-value (type-restriction slot-d)))))
    (let ((z (xform (lookup-aliased n lex-envt dyn-envt) 'value)))
      (if (not (eq? (actual-value z) 
		    (actual-value (type-restriction slot-d))))
	  (error "*** hideous class hack for ~s FAILED\n" n))
      z)))

(define (ctime-instance-maker class-ic initializers lex-envt dyn-envt mode)
  (if *tl-report*
      (format #t "ctime make of ~s\n" class-ic))
  (let* ((class (value (actual-bdg (var class-ic))))
	 (slots (class-compute-slot-inits class))
	 ((n <fixnum>) (class-instance-size class))
	 (slot-inits (make-vector n #f))
	 (leftover '()))
    ;;
    (let loop ((s initializers))
      (if (pair? s)
	  (let ((the-keyword (value (car s)))
		(the-value (cadr s)))
	    ;;
	    ;; check to see if it's a slot-init
	    ;;
	    (let ((sloti (assq the-keyword slots)))
	      (if sloti
		  (let* ((slot (cdr sloti))
			 (ix (index slot)))
		    (if (vector-ref slot-inits ix)
			(warning "keyword `~s' already specified -- ignored" 
				 the-keyword)
			(if (eq? (initialization-mode slot) 'prohibited)
			    (error/semantic
			     "prohibited keyword `~s' specified" 
			     the-keyword)
			    (vector-set! slot-inits
					 ix
					 (coerced-expr
					  the-value
					  (hideous-class-hack slot
							      lex-envt
							      dyn-envt))))))
		  ;; it's not a slot
		  (set! leftover (append leftover
					 (list (car s) (cadr s))))))
	    ;;
	    (loop (cddr s)))))
    ;;
    ;; fill in default values and check for missing required inits
    ;;
    (for-each 
     (lambda (s)
       (let* ((slot (cdr s))
	      (imode (initialization-mode slot)))
	 (if *tl-report*
	     (format #t "\t~s (~s) => " (name slot) imode))
	 (if (not (vector-ref slot-inits (index slot)))
	     (case imode
	       ((required)
		(error/semantic
		 "required keyword `~s' not supplied" (car s)))
	       ((function)
		(let ((k (make <ic-const>
			       value: (init-value slot))))
		  (if *tl-report* (format #t "init by calling: ~s\n" k))
		  (vector-set! slot-inits
			       (index slot)
			       (make-combo k 
					   (make-no-values 'value)
					   dyn-envt
					   'value))))
	       (else
		(let ((k (make <ic-const>
			       value: (init-value slot))))
		  (if *tl-report* (format #t "init using value: ~s\n" k))
		  (vector-set! slot-inits
			       (index slot) 
			       k)))))))
     slots)
    ;;
    (let* ((allocer (gen-maker slot-inits class-ic #f))
	   ;; 
	   ;; create the temp var
	   ;;
	   (nvar (make <lexical-var> 
		       name: 'new-instance
		       type: class
		       trust-me-type?: #t))
	   (nv-ref (make <ic-lex-ref>
			 var: nvar))
	   ;;
	   ;; construct the initialization call
	   ;;
	   (initer (make <ic-call>
			 function: (tl-ref-well-known 'initialize)
			 args: (make <ic-multi>
				     arg-list: (cons nv-ref leftover)
				     mode: 'value))))
      ;;
      ;; construct the binding envt, which is, in fact
      ;; the result of the entire "make" expression
      ;;
      (make <ic-bind>
	    envt: (make <lexical-contour>
			name->bindings: (list (cons 'new-instance
						    nvar))
			bindings: (list nvar)
			lexical-enclosing: '()
			dynamic-enclosing: dyn-envt)
	    inits: (make <ic-multi>
			 arg-list: (list allocer)
			 mode: 'value)
	    rest?: #f
	    body: (make <ic-seq>
			stmt-list: (list initer nv-ref))))))


;;
;; create intermediate code to allocate an instance
;; as a result of a (make ...) form
;;
;; slots-inits ==> vector of initial-value exprs
;; alloc-area ==> icode to evaluate to an allocation-area
;;                or #f if allocation-area: not specified
;; class-ic ==> icode that evaluates to the <<class>> being made

(define (gen-maker slot-inits class-ic alloc-area)
  (if alloc-area
      (make <ic-call>
	    function: (tl-ref-well-known 'area-make-gvec)
	    args: (make <ic-multi>
			arg-list: (cons alloc-area
					(cons class-ic
					      (vector->list slot-inits)))
			mode: 'value)
	    mode: 'value)
      (make <ic-call-prim>
	    function: (well-known '%make)
	    args: (make <ic-multi>
			arg-list: (cons class-ic
					(vector->list slot-inits))
			mode: 'value)
	    return-types: '(<obj>)
	    mode: 'value)))


(define (runtime-instance-maker class-ic initializers lex-envt dyn-envt mode)
  (if *tl-report*
      (format #t "*** runtime make of ~s\n" class-ic))
  (make <ic-call>
	function: (tl-ref-well-known 'make-instance)
	mode: mode
	args: (make <ic-multi>
		    arg-list: (cons class-ic initializers))))

;; this function is responsible for locating bindings for the special
;; names that are known to the compiler, which are usually primops
;; or special cooperation between the compiler and the runtime system,
;; like make-instance

(define (tl-ref-well-known name)
  (let ((b (well-known name))
	(e (top-level-envt *current-module*)))
    (compile-ref (actual-bdg b) b e e 'value)))

(define *internals-module* #f)

(define (get-internals-module)
  (or *internals-module*
      (begin
	(set! *internals-module* (get-ct-module 'rs.lang.internal))
	*internals-module*)))

(define (well-known name)
  (if *current-module*
      ;; this is a temporary hack until we have modules
      ;; set up to import "private" bindings (because we
      ;; may have a need to refer to bindings inside the
      ;; module that aren't actually exported -- this is because,
      ;; in an important way, the compiler's special forms live
      ;; inside the special module, so it (we) are allowed to
      ;; see things we don't export)
      (or (lookup (top-level-envt *current-module*) name)
	  (lookup (top-level-envt (get-internals-module)) name)
	  (error/internal "no well-known binding for `~s'"
			  name))
      (abort 'well-known
	     "no current module in which to find `~s'"
	     name)))

;;
;; this should be common code...
;;

(define (class-instance-size c)
  (instance-size (actual-value c)))

(define (class-compute-slot-inits class)
  (append (map (lambda (sd)
		 (cons (init-keyword sd) sd))
	       (tclass-direct-slots class))
	  (if (null? (tclass-supers class))
	      '()
	      (class-compute-slot-inits
	       (actual-value (car (tclass-supers class)))))))
