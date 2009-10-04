#|------------------------------------------------------------*-Scheme-*--|
| File:    modules/compiler/macros.scm
|
|          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
|          as part of the RScheme project, licensed for free use.
|          See <http://www.rscheme.org/> for the latest information.
|
| File version:     1.12
| File mod date:    2005-04-11 15:08:57
| System build:     v0.7.3.4-b7u, 2007-05-30
| Owned by module:  compiler
|
| Purpose:          lexically hygenic (but simple) macros
`------------------------------------------------------------------------|#

(define-syntax (binding-isa-subst binding)
  (let ((t binding))
    (and t
	 (instance? t <substitution>))))

(define (match-context-lookup (name <symbol>) lex-envt context)
  (lookup-no-alias name lex-envt))

(define (match-context-cancel context)
  ;(format #t "cancelling: ~s\n" context)
  ((vector-ref context 0) #f))

(define (match-context-dyn-envt context)
  (vector-ref context 3))

(define (match-context-add-subst context symbol expr envt)
  (vector-set! context 
	       1
	       (cons (make <substitution> 
			   name: symbol
			   expr: expr
			   envt: envt)
		     (vector-ref context 1))))

;; NOTICE:  This algorithm does not work
;;          if you expect the type of an argument to
;;          change because of intentional capturing
;;  (define-syntax foo
;;	(syntax-form (a (b isa: <pair>))
;;	    (let (((a <pair>) (cons 1 1)))
;;		b)))
;;  (foo x x) ==> no match, because x is not obviously
;;                a <pair> at this point, even though
;;		  the actual binding that's important for the
;;		  second x would be a <pair>

(define (match-type? actual-expr actual-envt formal-type)
  (let ((r (return-types (compile actual-expr 
				  actual-envt 
				  actual-envt 
				  'value))))
    (if (pair? r)
	(let ((first-type (car r)))
	  (target-subclass? (if (symbol? first-type)
				(prim-type->class first-type)
				first-type)
			    formal-type))
	#f)))

(define (check-restricted-pattern actual 
				  actual-envt
				  restrict
				  lex-envt
				  dyn-envt)
  (if (null? (cdr restrict))
      (match-type? actual
		   actual-envt
		   (parse-type-expr (car restrict) lex-envt dyn-envt))
      (case (car restrict)
	;;
	((predicate)
	 (case (cadr restrict)
	   ((keyword?) (keyword? actual))
	   ((flag?) (flag? actual))
	   ((odd?) (odd? actual))
	   ((even?) (even? actual))
	   (else (error "unknown predicate: ~s" (cadr restrict)))))
	;;
	((instance?)
	 (case (cadr restrict)
	   ((<fixnum>) (fixnum? actual))
	   ((<string>) (string? actual))
	   (else
	    (error "unknown special class: ~s" (cadr restrict)))))
	(else
	 (error "unknown restriction mode: ~s" restrict)))))

(define (match formal actual actual-envt context)
  (cond
   ((symbol? formal)
    (match-context-add-subst context formal actual actual-envt))
   ((and (pair? formal)
	 (pair? (cdr formal))
	 (or (eq? (cadr formal) '::)
	     (eq? (cadr formal) 'isa:))) ;; `isa:' is old-style
    (if (not (check-restricted-pattern actual
				       actual-envt
				       (cddr formal)
				       (vector-ref context 2)
				       (match-context-dyn-envt context)))
	(match-context-cancel context))
    (match-context-add-subst context (car formal) actual actual-envt))
   ((and (pair? formal) (eq? (car formal) 'quote))
    (if (not (eq? (cadr formal) actual))
	;; see if it's a substitution that matches
	(if (symbol? actual)
	    (let ((bdg (match-context-lookup
			actual 
			actual-envt
			context)))
	      (if (binding-isa-subst bdg)
		  (match formal (expr bdg) (envt bdg) context)
		  (match-context-cancel context)))
	    (match-context-cancel context))))
   ((pair? formal)
    (if (pair? actual)
	(begin
	  (match (car formal) (car actual) actual-envt context)
	  (match (cdr formal) (cdr actual) actual-envt context))
	;;
	;; the last chance for this match is if actual is
	;; a symbol which is bound to a substitution
	;;
	(if (symbol? actual)
	    (let ((bdg-info (match-context-lookup
			     actual
			     actual-envt
			     context)))
	      (if (binding-isa-subst bdg-info)
		  (match formal
			 (expr bdg-info)
			 (envt bdg-info)
			 context)
		  (match-context-cancel context)))
	    (match-context-cancel context))))
   ((null? formal)
    (if (null? actual)
	#t
	;; there is the possibility that actual is a
	;; substitution name that is bound to the empty list...
	(if (symbol? actual)
	    (let ((bdg-info (match-context-lookup
			     actual
			     actual-envt
			     context)))
	      (if (binding-isa-subst bdg-info)
		  (match formal
			 (expr bdg-info)
			 (envt bdg-info)
			 context)
		  (match-context-cancel context)))
	    (match-context-cancel context))))
   (#t (error/internal "Bad macro formal" formal))))

(define (match-args formals actuals lex-envt dyn-envt defn-envt)
  (call-with-current-continuation
   (lambda (exit)
     (let ((ctx (vector exit '() defn-envt dyn-envt)))
       (match formals actuals lex-envt ctx)
       (make-lexical-envt (vector-ref ctx 1) defn-envt dyn-envt)))))

(define (compile-syntax-form form)
  (make <macro-form> 
	args: (cadr form)
	body: (cddr form)))

(define (compile-macro macro-name definition lexical-envt)
  (let ((else-bdg #f))
    (let loop ((s definition) ;; source forms
	       (fs '())      ;; dest <macro-form>'s
	       (sfs '()))
      (if (null? s)
	  (make <macro> 
		name: macro-name
		forms: (reverse fs)
		setter-forms: (reverse sfs)
		envt: lexical-envt
		else-bdg: else-bdg)
	  (if (pair? s)
	      (let ((d (car s)))
		(cond
		 ;;
		 ((eq? (car d) 'else)
		  (if else-bdg
		      (error/syntax 
		       "Multiple else parts in macro ~s's defn" 
		       macro-name)
		      (begin
			(set! else-bdg 
			      (make <substitution> 
				    name: macro-name
				    expr: (cadr d)
				    envt: lexical-envt))
			(loop (cdr s) fs sfs))))
		 ;;
		 ((and (pair? d)
		       (eq? (car d) 'syntax-form))
		  (loop (cdr s)
			(cons (compile-syntax-form d) fs)
			sfs))
		 ;;
		 ((and (pair? d)
		       (eq? (car d) 'setter-form))
		  (loop (cdr s)
			fs
			(cons (compile-syntax-form d) sfs)))
		 ;;
		 (else
		  (error/syntax "Bad syntax-form in macro ~s at: ~s"
				macro-name
				s))))
	      (error/syntax "Excess forms in syntax-form role in macro ~s"
			    macro-name))))))



(define (find-match macro-binding actuals lexical-envt dynamic-envt)
  (let loop ((f (forms macro-binding)))
    (if (pair? f)
	(let* ((specific-form (car f))
	       (result (match-args (args specific-form) 
				   actuals 
				   lexical-envt 
				   dynamic-envt
				   (envt macro-binding))))
	  (if result
	      (cons specific-form result)
	      (loop (cdr f))))
	#f)))

(define (find-setter-match macro-binding actuals lexical-envt dynamic-envt)
  (let loop ((f (setter-forms macro-binding)))
    (if (pair? f)
	(let* ((specific-form (car f))
	       (result (match-args (args specific-form) 
				   actuals 
				   lexical-envt 
				   dynamic-envt
				   (envt macro-binding))))
	  (if result
	      (cons specific-form result)
	      (loop (cdr f))))
	#f)))
