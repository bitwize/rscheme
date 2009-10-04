;; translate top-level forms

(define (xlate-top tops)
  (let ((methods '())
	(variables '())
	(constants '())
	(exprs '())
	(classes '())
	(generic-functions '()))
    (for-each
     (lambda (f)
       (show-xlation "top level ~s => ~@*#40s\n" (car f) f)
       (case (car f)
	 ((expr)
	  (set! exprs (cons (xlate (cadr f)) exprs)))
	 ((defmeth)
	  (let ((name (caadr f))
		(body (cadadr f)))
	    (show-xlation "method ~s\n" name)
	    (set! methods
		  (cons (cons name (cdr (xlate body)))
			methods))))
	 ((defclass)
	  (let ((name (cadr f))
		(supers (caddr f))
		(slot-specs (cadddr f)))
	    (show-xlation "class ~s\n" name)
	    (set! classes
		  (cons (cons name 
			      (cons (map xlate supers)
				    (map xlate slot-specs)))
			classes))))
	 ((defgf)
	  (let ((name (cadr f))
		(args (caddr f)))
	    (show-xlation "generic ~s\n" name)
	    (set! generic-functions
		  (cons (cons name (xlate args))
			generic-functions))))
	 ((defvar)
	  (let ((bdgs (xlate (cadr f)))
		(init (xlate (caddr f))))
	    (show-xlation "vars ~s\n" bdgs)
	    (if (not (eq? (length bdgs) 1))
		(abort 'xlate-top
		       "top-level multiple bdgs not ok"))
	    (set! variables (cons (cons (car bdgs) init) 
				  variables))))
	 ((defconst)
	  (let ((bdgs (xlate (cadr f)))
		(init (xlate (caddr f))))
	    (show-xlation "constants ~s\n" bdgs)
	    (if (not (eq? (length bdgs) 1))
		(abort 'xlate-top
		       "top-level multiple bdgs not ok"))
	    (set! constants (cons (cons (car bdgs) init)
				  constants))))
	 (else
	  (abort 'xlate-top
		 "unknown top-level form: ~s" f))))
     tops)
    (vector (reverse methods) 
	    (reverse variables) 
	    (reverse constants) 
	    (reverse generic-functions) 
	    (reverse classes)
	    (reverse exprs))))

#|
    collate top-level methods & generic functions
    by making each implicitly declared generic function 
    with only one method be a function
|# 
    
(define $preexisting-gfs
    '(size write-object display-object element set-element!
      initialize))

(define (collate-top envt methods generic-functions)
    (let ((t (make-table eq? symbol->hash)))
	(for-each
	    (lambda (gf)
		(table-insert! t (car gf) 2))
	    generic-functions)
	(for-each
	    (lambda (m)
		(let ((name (car m)))
		    (table-insert! t name (+ 1 (or (table-lookup t name) 0)))))
	    methods)
	(append
	    (map (lambda (gf)
		    `(define-generic-function ,(car gf)))
		    generic-functions)
	    (apply 
	     append
	     (map (lambda (m)
		    (let ((name (car m))
			  (args (cadr m)))
			(if (and (> (table-lookup t name) 1)
			         (not (assq name generic-functions))
				 (not (memq name $preexisting-gfs)))
			    `((define-generic-function ,name))
			    '())))
		    methods))
	    (map (lambda (m)
		   ;;
		   ;; a severe problem:  How to distinguish 
		   ;; `define method' forms that are defining
		   ;; generic function methods vs. defining
		   ;; "regular" functions
		   ;;
		   ;; we do it by looking to see if it's bound and
		   ;; to a GF
		   ;;
		   (let ((name (car m))
			 (args (cadr m))
			 (body (cddr m)))
			(if (recognize-is-gf? envt name args)
			    `(define-method ,name ,args ,@body)
			    `(define (,name ,@args) ,@body))))
		    methods))))
				
(define (recognize-is-gf? envt name args)
  (let ((bdg (lookup envt name)))
    (if bdg
	(if (instance? (value bdg) <generic-function>)
	    #t
	    #f)
	;; not bound
	;; if they are giving a specializer, assume gf
	(if (and (pair? args)
		 (pair? (car args)))
	    (begin
	      (format $console-error-port
		      "WARNING: assuming '~s' is intended to be a generic fn\n"
		      name)
	      #t)
	    #f))))
  
	  
(define (xlate-forms envt forms)
  (let ((v (xlate-top forms)))
    (show-xlation "forms: ~s\n" (vector-map length v))
    (append
     (map (lambda (c)
	    	    (cons 'define-class c))
	  (vector-ref v 4))
     (collate-top envt (vector-ref v 0) (vector-ref v 3))
     (map (lambda (v)
	    (if (pair? (car v))
		`(define ,(caar v) ,(cdr v))
		`(define ,(car v) ,(cdr v))))
	  (vector-ref v 1))
     (map (lambda (v)
	    (if (pair? (car v))
		`(define ,(caar v) ,(cdr v))
		`(define ,(car v) ,(cdr v))))
	  (vector-ref v 2))
     (vector-ref v 5))))

