(define *arg-envt* (make-table string=? string->hash))

(define (literal-or-stdin a)
  (if (string=? a "-")
      (let loop ((l '()))
	(let ((a (read-line *console-input-port*)))
	  (if (eof-object? a)
	      (reverse l)
	      (loop (cons a l)))))
      (list a)))

(define (add-attribute-arg name descr bdg single? proc)
  (let ((b (make <attrib-descriptor>
		 name: name
		 value-description: descr
		 attrib-binding: bdg
		 single-valued?: single?
		 processor: proc)))
    (table-insert! *arg-envt*
		   name
		   b)
    b))

(define-rewriter (define-attribute-arg form)
(define (symbol-append . args)
  (string->symbol 
   (apply string-append 
	  (map (lambda (a)
		 (if (symbol? a)
		     (symbol->string a)
		     a))
	       args))))

  (format #t "(define-attribute-arg ~s ...)\n" (cadr form))
  (let ((name (symbol->string (cadr form)))
	(descr (caddr form))
	(init-value (cadddr form))
	(proc (list-ref form 4)))
    (let ((var-name (string->symbol (string-append "*" name "*"))))
      `(begin
	 (define ,var-name ,init-value)
	 (define ,(symbol-append "arg-descr-" (cadr form))
	   (add-attribute-arg ,(string-append "-" name)
			      ,descr
			      (& ,var-name)
			      ,(not (equal? init-value ''()))
			      ,proc))))))


(define (add-flag-arg name bdg val)
  (let ((b (make <flag-descriptor>
		 name: name
		 flag-binding: bdg
		 use-value: val)))
    (table-insert! *arg-envt*
		   name
		   b)
    b))

(define-rewriter (define-flag-arg form)
(define (symbol-append . args)
  (string->symbol 
   (apply string-append 
	  (map (lambda (a)
		 (if (symbol? a)
		     (symbol->string a)
		     a))
	       args))))

  (format #t "(define-flag-arg ~s ...)\n" (cadr form))
  (if (eq? (length form) 4)
      (let ((name (symbol->string (cadr form)))
	    (plus-value (caddr form))
	    (minus-value (cadddr form)))
	(let ((var-name (string->symbol (string-append "*" name "*"))))
	  `(begin
	     (define ,var-name ,minus-value)
	     (add-flag-arg ,(string-append "-" name) 
			   (& ,var-name)
			   ,minus-value)
	     (define ,(symbol-append "arg-descr-" (cadr form))
	       (add-flag-arg ,(string-append "+" name)
			     (& ,var-name)
			     ,plus-value)))))
      (let ((name (symbol->string (cadr form)))
	    (supply-value (caddr form)))
	(let ((var-name (string->symbol (string-append "*" name "*"))))
	  `(begin
	     (define ,var-name #f)
	     (define ,(symbol-append "arg-descr-" (cadr form))
	       (add-flag-arg ,(string-append "-" name)
			     (& ,var-name)
			     ,supply-value)))))))


(define (add-action name proc require optional default?)
  (let ((a (make <action-descriptor>
		 name: name
		 required-items: require
		 optional-items: optional
		 action-procedure: proc
		 default?: default?)))
    (table-insert! *arg-envt* name a)
    a))

(define (add-action-var name proc require optional descr single? valid default?)
  (let ((a (make <action-attrib>
		 name: name
		 required-items: require
		 optional-items: optional
		 action-procedure: proc
		 value-description: descr
		 singled-valued?: single?
		 default?: default?
		 processor: valid)))
    (table-insert! *arg-envt* name a)
    a))

;; proc is "do-action-foo"

(define-rewriter (define-action form)
(define (symbol-append . args)
  (string->symbol 
   (apply string-append 
	  (map (lambda (a)
		 (if (symbol? a)
		     (symbol->string a)
		     a))
	       args))))

  (format #t "(define-action ~s ...)\n" (cadr form))
  (let* ((name (cadr form))
	 (default? (if (eq? (caddr form) ':default)
		       (begin
			 (set! form (cdr form))
			 #t)
		       #f))
	 (req (cons 'list (map (lambda (n)
				 (symbol-append "arg-descr-" n))
			       (caddr form))))
	 (opt (cons 'list (map (lambda (n)
				 (symbol-append "arg-descr-" n))
			       (cadddr form))))
	 (arg (string-append "-"
			     (symbol->string
			      (if (pair? name)
				  (car name)
				  name))))
	 (actn-fn (symbol-append "do-action-"
				 (if (pair? name)
				     (car name)
				     name))))
    ;;
    (if (pair? name)
	;;
	;; it's an action with arguments
	;;
	(if (eq? (caddr name) '...)
	    ;;
	    ;; variable # args
	    ;;
	    `(add-action-var ,arg
			     ,actn-fn
			     ,req
			     ,opt
			     ,(cadr name)
			     #f
			     ,(cadddr name)
			     ,default?)
	    `(add-action-var ,arg 
			     ,actn-fn 
			     ,req 
			     ,opt 
			     ,(cadr name)
			     #t
			     ,(caddr name)
			     ,default?))
	;;
	;; it's a simple action
	;;
	`(add-action ,arg ,actn-fn ,req ,opt ,default?))))

;;
;;  install unique prefixes as abbreviations
;;

(define (install-abbrevs)
  (let ((base-names (sort (table-keys->list *arg-envt*) string<?)))
    (for-each (lambda (canonical-name abbrevs)
		(let ((v (table-lookup *arg-envt* canonical-name)))
		  (for-each (lambda (abbrev)
			      (table-insert! *arg-envt* abbrev v))
			    abbrevs)))
	      base-names
	      (find-abbreviations base-names 1))))

(define (parse-std-args args . fixer)
  (bind ((a b (parse-cmvc-style *arg-envt* args)))
    (if (pair? fixer)
	((car fixer)))
    (apply (action-procedure a) b)))
