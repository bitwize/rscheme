#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/repl/objforms.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.43
 | File mod date:    2005-04-11 15:06:39
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  repl
 |
 | Purpose:          Object system forms
 `------------------------------------------------------------------------|#

(define (class-name->hash (name <symbol>))
  (string->hash (number->string (symbol->hash name))))

;; nb:  going through a lambda in these special forms is simply
;; to help the ability to reload files and still work

(define (make-objsys-forms)
  (list (make <definer>
	      name: 'define-class
	      compiler-description: 'define-class
	      syntax-checker: check-define-class-syntax)
	(make <definer>
	      name: 'define-generic-function
	      compiler-description: 'define-generic-function
	      syntax-checker: check-define-generic-function-syntax)
	(make <definer>
	      name: 'define-method
	      compiler-description: 'define-method
	      syntax-checker: check-define-method-syntax)
	(make <special-form>
	      name: '%slot-index
	      compiler-description: '%slot-index)
	(make <special-form>
	      name: 'make
	      compiler-description: 'make)))

(define (objsys-compiler->proc description)
  (case description
    ((make) compile-make)
    ((%slot-index) compile/slot-index)
    ((define-class) compile-tl-define-class)
    ((define-method) compile-tl-define-method)
    ((define-generic-function) compile-tl-define-generic-function)
    (else #f)))

(%early-once-only (add-special-form-compiler! objsys-compiler->proc))


(define (compile-tl-define-class tl-form tl-envt dyn-envt)
  (let ((name (cadr tl-form))
	(supers (parse-supers (caddr tl-form) tl-envt tl-envt))
	(new-slots '())
	(heap-type 0)
	(image-mode #f)
	(category #f)
	(metaclass <<standard-class>>)
	(metainits '())
	(prim-type #f))
    (if *compile-verbose*
	(format #t "compiling top-level class: ~s\n" name))
    (let loop ((i (cdddr tl-form)))
      (cond
       ((null? i)
	(values))
       ((not (pair? i))
	(error/syntax "Improper list in class defn: ~s" tl-form))
       ((flag? (car i))
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
	  (else
	   (set! metainits (cons* #t
				  (symbol->keyword
				   (flag->symbol
				    (car i)))
				  metainits))
	   (loop (cdr i)))))
       ((keyword? (car i))
	(case (car i)
	  ((heap-type:)
	   (set! heap-type (cadr i))
	   (assert (fixnum? heap-type))
	   (loop (cddr i)))
	  ((image-mode:) 
	   (set! image-mode (cadr i))
	   (assert (fixnum? (cadr i)))
	   (loop (cddr i)))
	  ((prim-type:) 
	   (set! prim-type (cadr i))
	   (assert (symbol? (cadr i)))
	   (loop (cddr i)))
	  ((metaclass:) 
	   (set! metaclass (parse-type-expr (cadr i)
					    tl-envt
					    dyn-envt))
	   (loop (cddr i)))
	  ((class-category:)
	   (set! category (cadr i))
	   (assert (fixnum? category))
	   (loop (cddr i)))
	  (else
	   (set! metainits (cons* (parse-const-expr (cadr i) tl-envt dyn-envt)
				  (car i)
				  metainits))
	   (loop (cddr i)))))
       ((symbol? (car i))
	(set! new-slots 
	      (cons (parse-slot-descriptor supers 
					   (car i) 
					   '() 
					   tl-envt
					   dyn-envt)
		    new-slots))
	(loop (cdr i)))
       ((and (list? (car i))
	     (pair? (car i))
	     (symbol? (caar i)))
	(set! new-slots 
	      (cons (parse-slot-descriptor supers 
					   (caar i) 
					   (cdar i)
					   tl-envt
					   dyn-envt)
		    new-slots))
	(loop (cdr i)))
       (else
	(error/syntax "define-class: strange subform ~s" (car i)))))
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
    (let ((num-new-slots (length new-slots))
	  (base-size (if (null? supers)
			 0
			 (instance-size (actual-value (car supers))))))
      ;; fix up the slots' indexes
      (for-each (lambda (slot i)
		  (set-index! slot (+ base-size i)))
		(reverse new-slots)
		(range num-new-slots))
      ;;
      (let ((c (apply make-instance
		      metaclass
		      class-name: name
		      heap-type: heap-type
		      image-mode: image-mode
		      superclasses: supers
		      direct-slots: (reverse new-slots)
		      instance-size: (+ num-new-slots base-size)
		      corresponding-primtype: prim-type
		      class-precedence-list: '()
		      class-category: category
		      class-hash: (class-name->hash name)
		      all-slots: #f
		      ;; should we check to make sure the metainits are
		      ;; all `valid' in some sense (i.e., not override
		      ;; required built-in default values, if any)
		      (reverse metainits))))
	;;
	;; create the setters 'n' getters
	(for-each 
	 (lambda ((slot <slot-descriptor>))
           ;;
	   (if (setter slot)
               (let ((setter-name (setter slot)))
                 (set-setter! slot #f)
                 (let ((m (create-setter-method slot c)))
                   (set-setter! slot m)
                   (provide-method setter-name tl-envt dyn-envt m))))
           ;;
	   (if (getter slot)
               (let ((getter-name (getter slot)))
                 (set-getter! slot #f)
                 (let ((m (create-getter-method slot c)))
                   (set-getter! slot m)
                   (provide-method getter-name tl-envt dyn-envt m)))))
	 new-slots)
	;
	(finalize-class c) ;; `finalize' is a REALLY bad name (confused w/GC)
	(install-tl-def name 
			tl-envt
			dyn-envt 
			(make <top-level-var>
			      name: name
			      value: c
			      write-prot: #t))
	name))))

(define (dispatched-on-type arg-list)
  (bind ((name type (parse-rqd-spec (car arg-list))))
    (if type
	(car type)
	'<object>)))

;; at this point,
;; method-form is just the argument decls and the body
;; e.g.   (((self <pair>) port) 
;;         (format port "(~s . ~s)" (car self) (cdr self)))
;;

(define (compile-the-method gf-name method-form envt dyn-envt)
  (let ((name gf-name)
	(formals (trust-me-for-dispatched-args (car method-form)))
	(dispatch-on (dispatched-on-type (car method-form)))
	(body (cdr method-form)))
    (if *compile-verbose*
	(format #t "compiling top-level method for ~s (~s)\n" 
		name dispatch-on))
    (bind ((cc (make-code-ctx (append-source-property
                               (list (list 'function-scope
                                           name
                                           dispatch-on))
                               dyn-envt)))
	   (ic rqa (compile/procedure name formals body envt dyn-envt))
	   (asm (procedure->aml ic '() cc)))
      (if *show-aml*
	  (show-aml asm))
      (make <method>
	    template: (aml->template asm cc)
	    function-specializers: (compute-specializers rqa envt dyn-envt)
	    environment: #f))))

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


;;; look up the given name, with the idea that we want
;;; a binding whose value is a generic function

(define (lookup-gf-bdg gf-name envt dyn-envt)
  (let ((bdg (lookup-aliased gf-name envt dyn-envt)))
    ;;
    (if bdg
	;; must be a module variable, not imported...
	(if (and (instance? bdg <top-level-var>)
		 (eq? (value bdg) '#unbound))
	    ;; its a TLV but its really unbound
	    (let ((gf (make-default-gf gf-name)))
	      (set-value! bdg gf)
	      bdg)
	    ;; it is bound... make sure its a gf
	    (if (and (instance? (actual-bdg bdg) <top-level-var>)
		     (instance? (value (actual-bdg bdg))
				<generic-function>))
		bdg
		(error/semantic "binding for ~s is not a generic function"
				gf-name)))
	;; no binding exists
	(create-default-gf gf-name envt dyn-envt))))

(define (provide-method gf-name envt dyn-envt this-method)
  (let ((bdg (lookup-gf-bdg gf-name envt dyn-envt)))
    (target-add-method (xform bdg 'value) this-method)
    this-method))
    
(define (compile-tl-define-method tl-form tl-envt dyn-envt)
  (let* ((gf-name (cadr tl-form))
	 (gf-bdg (lookup-gf-bdg gf-name tl-envt dyn-envt))
	 (gf (xform gf-bdg 'value))
	 (tl-form (install-next-method-syntax gf tl-form)))
    (if *compile-verbose*
	(format #t "compiling top-level method on: ~s\n" gf-name))
    (let ((meth (compile-the-method gf-name
				    (cddr tl-form)
				    tl-envt
				    dyn-envt)))
      (target-add-method gf meth)
      gf-name)))

;;
;; rewrite a `define-method' expr to supply some syntax for `next-method'
;; to the body of the method
;;

(define-method install-next-method-syntax ((self <single-dispatch-gf>) form)
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
  (bind ((gf (cadr form))
	 (args (caddr form))
	 (arg-name arg-type (parse-rqd-spec (car args)))
	 (body (cdddr form)))
    (let ((fnext (list '(with-module objsys find-next-method-1)
		       gf
		       (if arg-type
			   (car arg-type)
			   '<object>))))
      `(define-method ,gf ,args
	 (let-syntax ((next-method (syntax-form () 
				     ,(recombo fnext args))
				   (syntax-form stuff
				     (,fnext . stuff))
				   (else
				    (lambda args
				      (if (null? args)
					  ,(recombo fnext args)
					  ((with-module corelib apply*)
					   args
					   ,fnext))))))
	   ,@body)))))

(define (make-default-gf name)
  (let ((gf (make <single-dispatch-gf>
		  ;;
		  ;; the fn-template will get filled in later
		  ;;
		  template: #f
		  generic-function-name: name
		  function-specializers: (xform (well-known '<object>) 'value)
		  generic-function-methods: '())))
    ;;
    ;; set up the envt of the GF to bind two things, the
    ;; first of which is a pointer back to the GF itself.
    ;; (the other can be used for caching things by the GF
    ;;  dispatcher)
    ;;
    (finalize-generic-function gf)
    gf))

(define (create-default-gf name envt dyn-envt)
  (let ((gf (make-default-gf name)))
    (install-tl-def name
		    envt
		    dyn-envt
		    (make <top-level-var>
			  name: name 
			  value: gf
			  write-prot: #t))))

(define (compile-tl-define-generic-function tl-form tl-envt dyn-envt)
  (let ((name (cadr tl-form)))
    (if *compile-verbose*
	(format #t "compiling top-level generic function: ~s\n" name))
    (create-default-gf name tl-envt dyn-envt)
    name))


(define (parse-supers super-names lex-envt dyn-envt)
  (map (lambda (n)
	 (parse-type-expr n lex-envt dyn-envt))
       super-names))


;; split a keywords list into two parts,
;; the first part is the list of slot initializing keywords,
;; in order of appearance in the class.
;; the second part is all the rest, in the order of initial occurrence
;;
;; the kw-list is in <icode> form, as it has been returned from
;; compile-keyword-list

(define (split-keywords-list the-class kw-list)
    (let ((inits (make-vector (instance-size the-class) #f))
    	  (all-slots (slot-descriptors the-class))
	  (others '()))
	(let loop ((kw kw-list))
	    (if (null? kw)
	    	;; make sure required slots are supplied
		;; and check slot types where possible
		(begin
		    (finalize-initial-values inits all-slots)
		    (cons (vector->list inits)
			  (reverse others)))
		;; we know that (car kw) is an <ic-const>
		;; and (cadr kw) exists and is some expr
		(let ((nm (value (car kw))))
		    (let ((sd (find-slot-with-init-kwd nm all-slots)))
			(if sd
			    (if (vector-ref inits (index sd))
				(error/semantic
				    "slot ~s multiply initialized"
				    nm)
				(vector-set! inits (index sd) (cadr kw)))
			    (set! others (cons (cadr kw) 
					       (cons (car kw) others))))
			(loop (cddr kw))))))))
				
(define (find-slot-with-init-kwd kwd all-slots)
    (let loop ((i all-slots))
	(if (null? i)
	    #f
	    (if (eq? (init-keyword (car i)) kwd)
		(car i)
		(loop (cdr i))))))

(define (finalize-initial-values inits all-slots)
  (for-each
   (lambda ((sd <slot-descriptor>))
     (let ((initer (vector-ref inits (index sd)))
	   (mode (initialization-mode sd)))
       (if initer
	   (if (eq? mode 'prohibited)
	       (error/semantic
		"prohibited slot specified: ~s"
		(name sd))
	       (vector-set! inits
			    (index sd)
			    (coerced-expr initer
					  (type-restriction sd))))
	   (if (eq? mode 'required)
	       (error/semantic
		"required slot not specified: ~s"
		(name sd))
	       (vector-set!
		inits
		(index sd)
		(init-value sd))))))
   all-slots))


;; this function is responsible for locating bindings for the special
;; names that are known to the compiler, which are usually primops
;; or special cooperation between the compiler and the runtime system,
;; like make-instance

(define (tl-ref-well-known name)
  (let ((b (well-known name))
        (e (get-compiler-basis)))
    (compile-ref (actual-bdg b) b e e 'value)))


;;

(define (target-expr-value expr lex-envt dyn-envt)
  (eval-in-envt expr lex-envt))
