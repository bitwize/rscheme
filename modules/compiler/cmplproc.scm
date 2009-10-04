#|------------------------------------------------------------*-Scheme-*--|
 | File:	    modules/compiler/cmplproc.scm
 |
 |          Copyright (C)1998, 2001 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |	    See <http://www.rscheme.org/> for the latest info.
 |
 | File version:     1.8
 | File mod date:    2004-02-24 09:29:27
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  compiler
 |
 | Purpose:          compile procedural abstractions as in `lambda'
 +------------------------------------------------------------------------|
 | Note:
 |	Most of the work is in building code to handle the 
 |	procedure formals.
 |
 `------------------------------------------------------------------------|#

(define *leadins* '#(#optional 1 #rest 2 #key 3))
(define *parsed-formals-proto* '#(() #f #f #f))

#|
(define (add-leadin! leadin which)
  (set! *leadins*
	(vector-append
	 *leadins*
	 (vector leadin
		 (case which
		   ((optional) 1)
		   ((rest) 2)
		   ((key) 3)
		   (else 
		    (error "add-leadin!: invalid leadin code: ~s" which)))))))
|#

(define (parse-formals lst)
  (let ((accums (clone *parsed-formals-proto*)))
    (let loop ((l lst)
	       (i 0)
	       (current-leadin #f))
      (cond
       ((null? l)
	;; validate the #rest arg
	(let ((rest (vector-ref accums 2)))
	  (if (and (list? rest) (not (= (length rest) 1)))
	      (error/syntax* lst "#rest must specify exactly one variable")))
	;(print accums)
	(vector->values (vector-map (lambda (l)
				      (and l (reverse l)))
				    accums)))
       ((symbol? l)
	;; special case support for plain Scheme-style rest arg
	(if current-leadin
	    (error/syntax* lst
			   "cannot have scheme-style #rest after leadin ~s"
			   current-leadin)
	    (loop (list '#rest l) i current-leadin)))
       ((pair? l)
	(let ((li (vassq (car l) *leadins*)))
	  (if li
	      ; its a leadin
	      (let ((new-i (vector-ref *leadins* li)))
		(if (< new-i i)
		    (error/syntax* lst "leadin ~s not valid after ~s" 
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
	(error/syntax* lst "invalid formals at: ~s" l))))))

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
	`(let ((,kv (with-module low-scheme
		      (keyword-value-list->vector ,rest-v))))
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
	     ((with-module low-scheme check-all-keywords-used) ,kvv ',name)
	     ,@body))))

(define *unspecified-keywords-are-required?* #t) ;; set to #f for Kawa compat

(define (expand-1-key-arg fn-name key-arg kvv body)
  (bind ((k-name k-type k-dflt k-kwd (parse-arg-spec key-arg))
	 (kwd (symbol->keyword k-kwd))
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
	 (getter `(with-module low-scheme
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
;;; Dylan notation (see p.44) is:
;;;
;;;  <symbol>
;;;  (<symbol> <value>)
;;;  (<keyword> <symbol>)
;;;  (<keyword> <symbol> <value>)
;;;
;;; At least the first two are consistent with Kawa and DSSSL notation

(define (parse-arg-spec/dylan spec)
  (if (pair? spec)
      (if (keyword? (car spec))
          (values (cadr spec) #f (cddr spec) (keyword->symbol (car spec)))
          (values (car spec) #f (cdr spec) (car spec)))
      (values spec #f #f spec)))

(define (parse-arg-spec/rscheme spec)
  (define (slurp-kwd kwd)
    (let ((t (memq kwd spec)))
      (if t
	  (cdr t)
	  #f)))
  (if (pair? spec)
      (values (let ((alt (slurp-kwd 'parameter:)))
                (if alt
                    (car alt)
                    (car spec)))
              (slurp-kwd 'type:) 
              (slurp-kwd 'default:)
              (car spec))
      (values spec #f #f spec)))

(define (parse-arg-spec spec)
  ;; automatically detect a formal arg in DSSSL format
  (if (and (pair? spec)
	   (pair? (cdr spec))
	   (not (keyword? (cadr spec))))
      (parse-arg-spec/dylan spec)
      (parse-arg-spec/rscheme spec)))

;;;
;;;  requires arguments (and the #rest arg, if any)
;;;  look like:
;;;     NAME
;;;     (NAME)
;;;     (NAME TYPE)
;;;     (NAME type: TYPE)
;;;

(define (parse-rqd-spec arg-spec)
  (define (rats)
    (error/syntax* arg-spec "required or #rest arg should be\n   NAME, (NAME), (NAME TYPE), or (NAME type: TYPE)"))
  (cond
   ((symbol? arg-spec)
    (values arg-spec #f))
   ((not (list? arg-spec))
    (rats))
   ((not (symbol? (car arg-spec)))
    (rats))
   ((null? (cdr arg-spec))
    (values (car arg-spec) #f))
   ((null? (cddr arg-spec))
    (values (car arg-spec) (list (cadr arg-spec))))
   ((not (eq? (cadr arg-spec) 'type:))
    (rats))
   ((null? (cdddr arg-spec))
    (values (car arg-spec) (list (caddr arg-spec))))
   (else
    (rats))))

;;;

(define (compile/procedure name formals-spec body lex-envt dyn-envt)
  (thread-let ((*procedure-name* name)
               (*place* (cons name *place*))
               (*num-lambdas* 0))
    (bind ((new-body new-args (expand-special-args-if-any name
							  formals-spec
							  body))
	   (new-envt (make-lexical-envt
		      (make-lex-vars new-args lex-envt dyn-envt)
		      lex-envt 
		      dyn-envt)))
      (values
       (make <ic-procedure>
	     name: name
	     body: (make <ic-bind>
			 envt: new-envt
			 inits: #f	;; special
			 rest?: (compute-has-rest new-args)
			 body: (compile/body new-body
					     new-envt
					     new-envt
					     'tail)))
       new-args))))

       

