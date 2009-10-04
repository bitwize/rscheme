;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-req-keyword-value (kvv <vector>) kwd)
  (bind ((val has-val? (get-opt-keyword-value kvv kwd)))
    (if has-val?
	val
	(error "required keyword `~a' missing" kwd))))

(define (get-opt-keyword-value (kvv <vector>) kwd)
  (using-keyword-value
   kwd
   kvv
   (lambda (item)
     (values item #t))
   (lambda ()
     (values #f #f))))

(define (p1xa a)
  (if (list? a)
      (bind ((n (car a))
	     (v (keyword-value-list->vector (cdr a)))
	     (dflt has-dflt? (get-opt-keyword-value v 'default:))
	     (type has-type? (get-opt-keyword-value v 'type:))
	     (tx (if has-type?
		     type
		     '<object>)))
        (list (list n tx)
	      (if has-dflt?
		  `(get-keyword-value %keys ',(symbol->keyword n) ,dflt)
		  `(get-req-keyword-value %keys ',(symbol->keyword n)))))
      (list (list a '<object>)
	    `(get-req-keyword-value %keys ',(symbol->keyword a)))))

(define (pxa args)
  (map p1xa args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (define-display-macro (name key! . kwds) . body)
  (if (memq '=> kwds)
      (let ((pre (reverse (cdr (memq '=> (reverse kwds)))))
	    (post (cdr (memq '=> kwds))))
	(make-display-macro make-reply-dpy-macro name pre post body 
			    (make-reply-class-and-methods name post)))
      (make-display-macro make-no-reply-dpy-macro name kwds '() body '())))

(define (make-reply-class-and-methods name post)
  (let ((n (symbol-append "<x-" name "-reply>")))
    (let loop ((s post)
	       (methods '())
	       (i 0))
      (if (null? s)
	  `((define-class ,n (<x-reply>) :bvec)
	    ,@(reverse methods)
	    (&module (export ,n)))
	  (loop (cddr s)
		(if (eq? (cadr s) '-)
		    methods
		    (cons (make-reply-field-method n i (car s) (cadr s)) methods))
		(+ i (unit-length (car s))))))))

(define (make-reply-field-method class-name offset type field-name)
  (if (pair? field-name)
      (case (cadr field-name)
	((:zero->false)
	 `(define-method ,(car field-name) ((self ,class-name))
	    (let ((temp (,(name-for-unit-reader type) self ,offset)))
	      (if (eq? temp 0)
		  #f
		  temp))))
	((enum:)
	 ;; SLOW!
	 `(define-method ,(car field-name) ((self ,class-name))
	    (let* ((temp (,(name-for-unit-reader type) self ,offset))
		   (a (assq temp ',(caddr field-name))))
	      (if a
		  (cadr a)
		  #f))))
	(else
	 (error "~s: invalid reply field specifier" field-name)))
      `(define-method ,field-name ((self ,class-name))
	 (,(name-for-unit-reader type) self ,offset))))

(define (make-display-macro when-dpy name request reply body more)
  (let ((request-main (if (memq '+ request)
			  (reverse (cdr (memq '+ (reverse request))))
			  request))
	(request-extension (if (memq '+ request)
			       (cdr (memq '+ request))
			       '())))
    `(begin
       ,@more
       (define-macro (,name . kwds)
	 (let* ((%keys (keyword-value-list->vector kwds))
		(dpy (get-keyword-value %keys 'display: #f)))
	   (if dpy
	       ,(when-dpy name reply)
	       (bind (,@(pxa request-main)
		      ,@(mxa request-extension))
		 ,(make-make-buf name request-main request-extension body))))))))

(define (make-no-reply-dpy-macro name reply-form)
  (deep-replace
   '`(let* (((display <x-display>) ,dpy)
	    (buf (<*name*> ,@(remainder->list %keys))))
       (x/send display buf)
       (values))
   (list (cons '<*name*> name))))

(define (make-reply-dpy-macro name reply-form)
  (deep-replace
   '(let ((async (get-keyword-value %keys 'async: #f)))
      `(let* (((display <x-display>) ,dpy)
	      (buf (<*name*> ,@(remainder->list %keys)))
	      (rpc (x-rpc/send display buf <*reply-class*>)))
	 ,(if async
	      `rpc
	      `(let (((temp <*reply-class*> :trust-me) 
		      (x-rpc/get-reply rpc '<*name*>)))
		 temp))))
   (list (cons '<*name*> name)
	 (cons '<*reply-class*> (symbol-append "<x-" name "-reply>")))))

;;;

(define (make-make-buf name kwds extn body)
  (list 'quasiquote
	(cons* 'let-syntax
	       (make-mkbuf-sx-bdgs kwds)
	       body)))

(define (make-mkbuf-sx-bdgs kwds)
  (map (lambda (k)
	 (let ((n (if (pair? k)
		      (car k)
		      k)))
	   (list n (list 'else (list 'unquote n)))))
       kwds))

;;;

(define (mxa lst)
  (if (null? lst)
      '()
      (make-extension-args lst)))

(define (make-extension-args lst)
  (let ((has-lst (map (lambda (xa)
			(symbol-append "has-" xa "?"))
		      lst)))
    (append (map (lambda (xa has-xa?)
		   (list xa
			 has-xa?
			 `(get-opt-keyword-value %keys ',(symbol->keyword xa))))
		 lst
		 has-lst)
	  `((value-bitmask (bitwise-or*
			    ,@(map (lambda (i has-xa?)
				     `(if ,has-xa?
					  ,(logical-shift-left 1 i)
					  0))
				   (range (length has-lst))
				   has-lst)))
	    (list-of-values (append ,@(map (lambda (xa has-xa?)
					     `(if ,has-xa?
						  (list 'u4: ,xa)
						  '()))
					   lst
					   has-lst)))
	    (num-values (length (select identity (list ,@has-lst))))))))
