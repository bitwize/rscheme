;;;
;;;   provide `#key' and `#optional' in argument formals
;;;
;;;   For example,
;;;
;;;     (define (foo x y #optional z #key color)
;;;        ...)
;;;
;;;   this implementation permits composing `#rest' with `#key'
;;;   a la common lisp
;;;
;;;   keyword arguments come after all required, #optional, and 
;;;   #rest arguments, so the formals list looks like:
;;;
;;;         <required-arg-spec> ... 
;;;           [#optional <optional-arg-spec> ...]
;;;           [#rest <rest-arg-spec>]
;;;           [#key <keyword-arg-spec> ...]
;;;
;;;   where <keyword-arg-spec> and <optional-arg-spec> is one of:
;;;
;;;         name
;;;         (name [type: TYPEEXPR] [default: DEFAULTEXPR])
;;;
;;;   For <keyword-arg-spec>'s, if DEFAULTEXPR is not supplied, 
;;;   then the keyword is required and the associated keyword is "name:"
;;;
;;;   For <optional-arg-spec>'s, if DEFAULTEXPR is not supplied,
;;;   the default value is #f
;;;
;;;   <rest-arg-spec> may be one of:
;;;         name
;;;         (name COLLECTION_TYPE_EXPR)
;;;
(add-unique-object! "#optional" #f)

(define-module-extend compiler ()

(define (check-all-keywords-used (v <vector>) fn-name)
  (let loop (((i <fixnum>) 0))
    (if (eq? i (vector-length v))
	(values)
	(if (vector-ref v i)
	    (error "~s: excess keywords supplied; at least `~s'"
		   fn-name
		   (vector-ref v i))
	    (loop (fixnum+ i 2))))))


(define (vector->values v)
  (list->values (vector->list v)))

(define $leadins '#(#optional 1 #rest 2 #key 3))

(define (parse-formals lst)
  (let ((accums (make-vector (+ 1 (quotient (vector-length $leadins) 2)) #f)))
    (vector-set! accums 0 '())
    (let loop ((l lst)
	       (i 0)
	       (current-leadin #f))
      (cond
       ((null? l)
	;; validate the #rest arg
	(let ((rest (vector-ref accums 2)))
	  (if (and (list? rest) (not (= (length rest) 1)))
	      (error "#rest must specify exactly one variable")))
	(vector->values (vector-map (lambda (l)
				      (and l (reverse l)))
				    accums)))
       ((symbol? l)
	;; special case support for plain Scheme-style rest arg
	(if current-leadin
	    (error "cannot have scheme-style #rest after leadin ~s"
		   current-leadin)
	    (loop (list '#rest l) i current-leadin)))
       ((pair? l)
	(let ((li (vassq (car l) $leadins)))
	  (if li
	      ; its a leadin
	      (let ((new-i (vector-ref $leadins li)))
		(if (< new-i i)
		    (error "leadin ~s not valid after ~s" 
			   (car l)
			   current-leadin)
		    (begin
		      (vector-set! accums new-i '())
		      (loop (cdr l) new-i (car l)))))
	      ; not a leadin
	      (begin
		(vector-set! accums i (cons (car l) (vector-ref accums i)))
		(loop (cdr l) i current-leadin)))))
       (else
	(error "invalid form in formals: ~s" l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  
;;;  insert special processing of argument formals
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (expand-special-args-if-any name forml body)
  (bind ((required-args optional-args rest-arg key-args (parse-formals forml)))
    (if (or optional-args rest-arg key-args)
	(bind ((bod rv (expand-special-args name
					    body
					    optional-args
					    rest-arg
					    key-args)))
	  (values (list bod) (append required-args (list '#rest rv))))
	(values body required-args))))

(define (expand-special-args name 
			     body
			     optional-args
			     rest-arg
			     key-args)
  (if (pair? optional-args)
      (bind ((var-name type dflt (parse-arg-spec (car optional-args)))
	     (put-in (gensym))
	     (bod g (expand-special-args name body 
					 (cdr optional-args)
					 rest-arg
					 key-args)))
	(values `(bind ((,(gen-rscheme-spec var-name type)
			 ,g
			 (if (null? ,put-in)
			     (values ,(if dflt (car dflt) #f) '())
			     (values (car ,put-in) (cdr ,put-in)))))
		   ,bod)
		put-in))
      (bind ((g u? (if rest-arg
		       (values (car rest-arg) #t)
		       (values (gensym) #f))))
	(values (expand-key-args name body key-args g u?) g))))

(define (expand-key-args fn-name 
			 body
			 key-args
			 rest-v
			 user-rest?)
  (if key-args
      (let ((kv (gensym)))
	`(let ((,kv (with-module objsys (keyword-value-list->vector ,rest-v))))
	   ,(expand-key-args* fn-name body key-args kv user-rest?)))
      (if user-rest?
	  (cons 'begin body)
	  `(begin
	     (if (not (null? ,rest-v))
		 (signal "~s: too many arguments" ',fn-name))
	     ,@body))))

(define (expand-key-args* name 
			  body
			  key-args
			  kvv
			  user-rest?)
  (if (pair? key-args)
      (expand-1-key-arg name
			(car key-args)
			kvv
			(expand-key-args* name
					  body
					  (cdr key-args) 
					  kvv
					  user-rest?))
      (if user-rest?
	  `(begin
	     ,@body)
	  `(begin
	     (',check-all-keywords-used ,kvv ',name)
	     ,@body))))

(define *unspecified-keywords-are-required?* #t) ;; set to #f for Kawa compat

(define (expand-1-key-arg fn-name key-arg kvv body)
  (bind ((k-name k-type k-dflt (parse-arg-spec key-arg))
	 (kwd (symbol->keyword k-name))
	 (dflt-thunk (if k-dflt
			 `(lambda ()
			    ,(car k-dflt))
			 (if *unspecified-keywords-are-required?*
			     `(lambda ()
				(signal 
				 "~s: required keyword `~s' not supplied"
				 ',fn-name
				 ',kwd))
			     `(lambda () #f))))
	 (getter `(with-module objsys
		    (using-keyword-value ',kwd
					 ,kvv
					 (lambda (item) item)
					 ,dflt-thunk))))
    `(let ((,(gen-rscheme-spec k-name k-type) ,getter))
       ,body)))

(define (gen-rscheme-spec name type)
  (if type
      (list name (car type))
      name))

;;;---------------------------------------------------------------------
;;; Kawa (and DSSSL) arg specs are either `name' or `(name initializer)'

(define (parse-arg-spec/kawa spec)
  (if (pair? spec)
      (values (car spec) #f (cdr spec))
      (values spec #f #f)))

(define (parse-arg-spec/rscheme spec)
  (define (slurp-kwd kwd)
    (let ((t (memq kwd spec)))
      (if t
	  (cdr t)
	  #f)))
  (if (pair? spec)
      (values (car spec) (slurp-kwd 'type:) (slurp-kwd 'default:))
      (values spec #f #f)))

(define (parse-arg-spec spec)
  ;; automatically detect a formal arg in DSSSL format
  (if (and (pair? spec)
	   (pair? (cdr spec))
	   (not (keyword? (cadr spec))))
      (parse-arg-spec/kawa spec)
      (parse-arg-spec/rscheme spec)))

;;;

(define (compile/procedure name formals-spec body lex-envt dyn-envt)
  (fluid-let ((*procedure-name* name)
	      (*place* (cons name (fluid-ref *place* '())))
	      (*num-lambdas* 0))
    (bind ((new-body new-args (expand-special-args-if-any name
							  formals-spec
							  body))
	   (new-envt (make-lexical-envt
		      (make-lex-vars new-args lex-envt dyn-envt)
		      lex-envt 
		      dyn-envt)))
      (make <ic-procedure>
	    name: name
	    body: (make <ic-bind>
			envt: new-envt
			inits: #f	;; special
			rest?: (compute-has-rest new-args)
			body: (compile/body new-body 
					    new-envt 
					    new-envt 
					    'tail))))))

)
