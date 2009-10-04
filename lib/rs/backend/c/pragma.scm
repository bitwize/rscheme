
(define-class <pragma-context> (<object>)
  (properties type: <vector> init-value: '#()))

(define-thread-var *pragma-context* 
  (make <pragma-context>
	properties: '#(features (rscheme))))

(define (get-pragma key dflt)
  (get-property *pragma-context* key dflt))

;;;
;;;   usage:
;;;
;;;     (pragma (<cfg> ...) [body ...])
;;;
;;;  where
;;;    <cfg> ::= (KEY = VALUE)
;;;           |  (KEY + VALUE ...)   ;; prepend
;;;           |  (KEY - VALUE ...)   ;; delete
;;;

(define (compile-pragma sf form lex-envt dyn-envt mode)
  (let ((pc (if (null? (cddr form))
		*pragma-context*
		(make <pragma-context>
		      properties: (properties *pragma-context*)))))
    (for-each
     (lambda (cfg)
       (let ((key (car cfg))
	     (op (cadr cfg))
	     (vals (cddr cfg)))
	 (case op
	   ((=)
	    (set-property! pc key (car vals)))
	   ((+)
	    (set-property! pc key (append vals (get-property pc key '()))))
	   ((-)
	    (let loop ((kill vals)
		       (lst (get-property pc key '())))
	      (if (pair? kill)
		  (loop (cdr kill)
			(delq (car kill) lst))
		  (set-property! pc key lst))))
	   (else
	    (error "pragma: invalid op `~s'" op)))))
     (cadr form))
    ;;
    (if (pair? (cddr form))
	(thread-let ((*pragma-context* pc))
	  (compile/body (cddr form) lex-envt dyn-envt mode))
	(make-no-values mode))))

;;;

#|
(define *pragma* (make <special-form>
		       name: 'pragma
		       compiler-proc: compile-pragma
		       compiler-description: 'pragma))
|#