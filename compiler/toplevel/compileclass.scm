#|------------------------------------------------------------*-Scheme-*--|
 | File:    compiler/toplevel/compileclass.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.25
 | File mod date:    2005-01-20 19:57:47
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  (rsc)
 |
 | Purpose:          `define-class' special form compiler
 `------------------------------------------------------------------------|#

;; nb:  going through a lambda in these special forms is simply
;; to help the ability to reload files and still work

(define (make-objsys-forms)
  '())

(define (objsys-compiler->proc description)
  #f)

(define (class-name->hash (name <symbol>))
  (string->hash (number->string (symbol->hash name))))

(define (make-objsys-forms)
  (let-syntax ((sf (syntax-form (n)
		     (make <special-form>
			   name: (mquote n)
			   compiler-proc: #f
			   compiler-description: (mquote n))))
	       (def (syntax-form (n)
		      (make <definer>
			    name: (mquote n)
			    compiler-proc: #f
			    compiler-description: (mquote n)))))
    (list (def define-class)
	  (def define-generic-function)
	  (def define-method)
	  (sf make)
          (sf %slot-index))))

(define (objsys-compiler->proc description)
  (case description
    ((define-class) compile-tl-define-class)
    ((define-generic-function) compile-tl-define-generic-function)
    ((define-method) compile-tl-define-method)
    ((make) compile-make)
    ((%slot-index) compile/slot-index)
    (else #f)))

(define (compile-tl-define-class tl-form tl-envt d-envt)
  (let ((name (cadr tl-form))
	(supers (parse-supers (caddr tl-form) tl-envt d-envt))
	(new-slots (make-seq))
	(heap-type 0)
	(image-mode #f)
	(category #f)
	(prim-type #f))
    (if *tl-report*
	(format #t "compiling top-level class: ~s\n" name))
    (let loop ((i (cdddr tl-form)))
      (if (pair? i)
	  (if (symbol? (car i))
	      (case (car i)
		((:gvec) (set! heap-type 0)
			 (loop (cdr i)))
		((:bvec) (set! heap-type 1)
			 (loop (cdr i)))
		((:immob) (set! heap-type 2)
			  (loop (cdr i)))
		((:abstract) (set! heap-type 3)
			     (loop (cdr i)))
		((:weak1) (set! heap-type 4)
			  (loop (cdr i)))
		((heap-type:) (set! heap-type (cadr i))
			      (assert (fixnum? heap-type))
			      (loop (cddr i)))
		((image-mode:) (set! image-mode (cadr i))
			       (assert (fixnum? (cadr i)))
			       (loop (cddr i)))
		((prim-type:) (set! prim-type (cadr i))
			      (assert (symbol? (cadr i)))
			      (loop (cddr i)))
		((class-category:)
		 (set! category (cadr i))
		 (assert (fixnum? category))
		 (loop (cddr i)))
		(else
		 (seq-add! new-slots
			   (parse-slot-descriptor supers 
						  (car i) 
						  '() 
						  tl-envt
						  d-envt))
		 (loop (cdr i))))
	      (if (pair? (car i))
		  (begin
		    (assert (symbol? (caar i)))
		    (seq-add! new-slots
			      (parse-slot-descriptor supers 
						     (caar i) 
						     (cdar i) 
						     tl-envt
						     d-envt))
		    (loop (cdr i)))
		  (error/syntax "Illegal slot-descriptor: ~s" (car i))))))
    ;; make sure we only specify a category if there isn't one for
    ;; the parent; if not specified, inherit the parent's
    (if category
	(if (pair? supers)
	    (if (not (eq? (class-category (actual-value (car supers))) 0))
		(error/syntax "Parent class already has a category")))
	(if (null? supers)
	    (set! category 0)
	    (set! category (class-category (actual-value (car supers))))))
    ;; make image-mode: 1 be implicit for bvec's 
    (if (not image-mode)
	(if (eq? heap-type 1)
	    (set! image-mode 1)))
    (let* ((num-new-slots (length (seq->list new-slots)))
	   (base-size (if (null? supers)
			  0
			  (instance-size (actual-value (car supers)))))
	   (c (make <<target-class>>
		    class-name: name
		    heap-type: heap-type
		    image-mode: image-mode
		    superclasses: supers
		    class-category: category
		    class-hash: (class-name->hash name)
		    direct-slots: (seq->list new-slots)
		    instance-size: (+ num-new-slots base-size)
		    corresponding-primtype: prim-type
		    class-precedence-list: #f
		   ;; here is the sort of thing
		   ;; you can't do at compile time,
		   ;; because we can't have just 
		   ;; arbitrary pointers into
		   ;; imported modules' images
		    all-slots: #f)))
      ;; fix up the slots' indexes
      (for-each (lambda (slot i)
		  (set-index! slot (+ base-size i)))
		(seq->list new-slots)
		(range num-new-slots))
      ;; create the setters 'n' getters
      (for-each (lambda (slot)
		  (if (setter slot)
		      (set-setter! slot
				   (create-setter-method slot c tl-envt)))
		  (if (getter slot)
		      (set-getter! slot
				   (create-getter-method slot c tl-envt))))
		(seq->list new-slots))
      ;
      (set-module-classes! *current-module*
			   (cons c (module-classes *current-module*)))
      (ensure-new-tlb 
       name 
       tl-envt 
       (make <top-level-var>
	     name: name
	     value: c
	     write-prot: #t))
      #f)))

;; currently, only the first argument is dispatched, so 
;; that's the only one that's trusted

(define (trust-me-for-dispatched-args formals)
  (bind ((name type (parse-rqd-spec (car formals))))
    (cons (list name
		(if type
		    (car type)
		    '<object>)
		':trust-me)
	  (cdr formals))))

(define (dispatched-on-type arg-list)
  (bind ((name type (parse-rqd-spec (car arg-list))))
    (if type
	(car type)
	'<object>)))

(define (compile-the-method gf-name method-form envt dyn-envt)
  (let ((name gf-name)
	(formals (trust-me-for-dispatched-args (car method-form)))
	(dispatch-on (dispatched-on-type (car method-form)))
	(body (cdr method-form)))
    (if *tl-report*
	(format #t "compiling top-level method for ~s (~s)\n" 
		name dispatch-on))
    (bind ((cc (make-code-ctx (list (list 'function-scope
					  name
					  dispatch-on))))
	   ;; compile the procedure, and return the basic signature
	   ;; (ie, required args + optional #rest)
	   (ic rqa (compile/procedure name formals body envt dyn-envt))
	   (asm (procedure->aml ic '() cc)))
      (make <target-method>
	    template: (aml->template asm cc)
	    function-specializers: (compute-specializers rqa envt dyn-envt)
	    environment: #f))))

(define (make-binding-envt enclosing rest)
  (make-gvec* <binding-envt>
	      enclosing
	      rest))

;;=================================================================
;;  this code is due some substantial cleanup; I am storing the
;;  *name* of the getter/setter in the like-named slot of the
;;  <slot-descriptor> before this function is called, and then
;;  clobbering it upon return from the corresponding function
;;  (in compile-tl-define-class, above)  [96.07.05]
;; 

(define (create-getter-method (s <slot-descriptor>) 
			      (for <<target-class>>)
			      envt)
  (let ((tm (make <target-getter>
		  template: 'getter
		  environment: (make-binding-envt #f (list (index s)))
		  slot-descriptor: s
		  type-restriction: (type-restriction s)
		  index: (index s)
		  function-specializers: (list for))))
    (register-implicit-method tm 'getter-template)
    (provide-method (getter s) envt tm)))

;; these functions return the <method> created for them

(define (create-setter-method (s <slot-descriptor>) 
			      (for <<target-class>>)
			      envt)
  (let ((n (setter s))
	(t (type-restriction s))
	(check? (not (eq? (actual-value (type-restriction s))
			  (actual-value (xform (well-known '<object>)
					       'value))))))
    (if *tl-report*
	(if (not check?)
	    (format #t "setter method (~s) is unrestricted\n" n)
	    (format #t "setter method (~s) is restricted: ~s\n" n t)))
    (let ((tm (make <target-setter>
		    template: 'setter
		    environment: (make-binding-envt 
				  #f 
				  (if check?
				      (list (index s)
					    (type-restriction s))
				      (list (index s))))
		    slot-descriptor: s
		    type-restriction: (type-restriction s)
		    index: (index s)
		    function-specializers: (list for t))))
      (if check?
	  (register-implicit-method tm 'restricted-setter-template)
	  (register-implicit-method tm 'setter-template))
      (provide-method n envt tm))))

;;
;; this function arranges for a method to have it's 
;; `fn-template' pointer set later.  This is necessary
;; only because modules BEFORE objsys may want
;; setters & getters to be defined
;;
;; this should be optimized so that it set's the fn-template
;; to point directly to the value of the appropriate well-known
;; binding IFF such a well-known binding is available.

(define (register-implicit-method (m <target-method>) type)
  (let* ((ims (module-implicit-methods *current-module*))
	 (lst (assq type ims)))
    (if lst
	(set-cdr! lst (cons m (cdr lst)))
	(set-module-implicit-methods! *current-module*
				      (cons (list type m) ims)))
    (values)))

(define (provide-method gf-name envt this-method)
  (let ((bdg (lookup envt gf-name)))
    (if (not bdg)
	(set! bdg (create-default-gf gf-name envt)))
    ;;
    ;; make sure it's a GF
    ;;
    (if (or (not (instance? (actual-bdg bdg) 
			    <top-level-var>))
	    (not (instance? (value (actual-bdg bdg)) 
			    <target-gf1>)))
	(if (instance? (actual-bdg bdg) <top-level-var>)
	    (if (and (instance? bdg <top-level-var>)
		     (eq? (value bdg) '#unbound))
		(create-default-gf gf-name envt)
		(error/semantic "~s bound to ~s, not a generic function"
				gf-name
				(value (actual-bdg bdg))))
	    (error/semantic "binding for ~s is ~s, not a generic function"
			    gf-name (actual-bdg bdg))))
    ;;
    (let ((v (xform bdg 'value)))
      (if (instance? v <target-gf1>)
	  (target-add-method v this-method)      ;; local to module
	  (imported-add-method bdg this-method)) ;; imported into module
      this-method)))
    
;;
;; rewrite a `define-method' expr to supply some syntax for `next-method'
;; to the body of the method
;;

(define (install-next-method-syntax form)
  ;;
  ;; syntactically convert a formals list
  ;; into a call to an expr.  If the formals
  ;; contains a #rest, then the combo will involve 'apply*'
  ;;
  (define (recombo head formals)
    (let* ((apply? #f)
	   (f (process-formals$ formals
				(lambda () '())
				(lambda (last)
				  (set! apply? #t)
				  (list last))
				(lambda (item rest)
				  (cons (if (pair? item)
					    (car item)
					    item)
					rest)))))
      (if apply?
	  (append '(apply*) f (list head))
	  (cons head f))))
  ;;
  (let ((gf (cadr form))
	(args (caddr form))
	(body (cdddr form)))
    (let ((fnext (list 'find-next-method-1 gf (cadr (car args)))))
      `(define-method ,gf ,args
	 (let-syntax ((next-method (syntax-form () 
				     ,(recombo fnext args))
				   (syntax-form stuff
				     (,fnext . stuff))
				   (else
				    (lambda args
				      (if (null? args)
					  ,(recombo fnext args)
					  (apply* args ,fnext))))))
	   ,@body)))))

(define (compile-tl-define-method tl-form tl-envt d-envt)
  (let* ((tl-form (install-next-method-syntax tl-form))
	 (name (cadr tl-form)))
    (if *tl-report*
	(format #t "compiling top-level method on: ~s\n" name))
    (provide-method name
		    tl-envt
		    (compile-the-method name
					(cddr tl-form)
					tl-envt
					d-envt))
    #f)) ;; no initialization code

(define (create-default-gf name envt)
  (let ((gf (make <target-gf1>
		  ;;
		  ;; the fn-template will get filled in later
		  template: #f
;;		  environment: '()
		  generic-function-name: name
		  function-specializers: (xform (well-known '<object>) 'value)
		  generic-function-methods: '())))
    ;;
    ;; set up the envt of the GF to bind two things, the
    ;; first of which is a pointer back to the GF itself.
    ;; (the other can be used for caching things by the GF
    ;;  dispatcher)
    ;;
;;    (set-environment! gf (make-binding-envt #f (list gf '())))
    (set-module-generic-functions! *current-module*
				   (cons gf
					 (module-generic-functions
					  *current-module*)))
    (ensure-new-tlb name
		    envt
		    (make <top-level-var>
			  name: name 
			  value: gf
			  write-prot: #t))))

(define (compile-tl-define-generic-function tl-form tl-envt d-envt)
  (let ((name (cadr tl-form)))
    (if *tl-report*
	(format #t "compiling top-level generic function: ~s\n" name))
    (create-default-gf name tl-envt)
    #f)) ;; no initialization code

(define (parse-supers super-names envt d-envt)
  (map (lambda (n)
	 (parse-type-expr n envt d-envt))
       super-names))


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

(load "make.scm")
(define make-instance (with-module objsys make-instance))
(load "../../modules/repl/parsslot.scm")
(load "../../modules/repl/slotindx.scm")

(define (compile-make self form lex-envt dyn-envt mode)
  ;; determine the target class
  (let ((tc (compile (cadr form) lex-envt dyn-envt 'value)))
    (compile-instance-maker tc (cddr form) lex-envt dyn-envt mode)))

(define (find-slot-with-init-kwd kwd all-slots)
    (let loop ((i all-slots))
	(if (null? i)
	    #f
	    (if (eq? (init-keyword (car i)) kwd)
		(car i)
		(loop (cdr i))))))


;; note: the method may be specialized on classes
;; which are extern (ie, referred to through patches)

(define (target-add-method (gf <target-gf1>)
			   (method <target-method>))
  (assert (instance? gf <target-gf1>))
  (assert (instance? method <target-method>))
  (let ((methods (generic-function-methods gf)))
    ;; 
    ;; examine the function's specializers to see where to add it
    ;;
    (let-syntax ((insert-before (syntax-form (prev rest)
				  (if prev
				      (set-cdr! prev (cons method rest))
				      (set-generic-function-methods! 
				       gf 
				       (cons method rest)))))
		 (method-key-class (syntax-form (m)
				     (actual-value 
				      (car (function-specializers m))))))
      (let loop ((i methods)
		 (prev #f))
	(cond ((null? i)
	       (insert-before prev '())
	       (values))
	      ((eq? (method-key-class method)
		    (method-key-class (car i)))
	       ; this is a REPLACEMENT of an existing method
	       (set-car! i method)
	       (values))
	      ((target-subclass? (method-key-class method)
				 (method-key-class (car i)))
	       (insert-before prev i)
	       (values))
	      (else
	       (loop (cdr i) i)))))))

;; THIS HERE is what I spent almost all of this seperate
;; compilation work on -- the ability to add a method
;; to a generic function in another module

(define (imported-add-method bdg method)
  (let* (((lb <link-bdgs>) (link-command bdg))
	 ((im <imported-module>) (owner lb))
	 (lm (make <link-method>
		   owner: im
		   gf-bdg: bdg
		   methods: (list method))))
    (if *tl-report*
	(format #t "adding method to GF ~s from ~s\n"
		(name bdg)
		(name im)))
    ;; also add it to the <GF> itself so we can find this new method
    ;; as an inlining candidate
    (target-add-method (value (actual-bdg bdg)) method)
    ;;
    (set-link-commands! im (append (link-commands im) (list lm)))))
