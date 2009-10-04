;;;

(define-class <patterns> (<object>)
  (pre-patterns type: <list> init-value: '())
  (name type: <symbol>)
  (compile-time-constants type: <list> init-value: '())
  (rule-by-head type: <symbol-table> init-function: make-symbol-table))

(define-method to-string ((self <patterns>))
  (symbol->string (name self)))

(define-class <rule> (<object>)
  pattern
  translation
  (condition init-value: #t)
  (extras init-value: '()))

(define (make-rule pat xlat #key (where default: '())
                                 (envt default: #f)
		                 (when default: #t))
  (make <rule>
	pattern: pat
	translation: xlat
	condition: (if (eq? when #t)
		       #t
		       (expr->pattern-proc when envt))
	extras: (map (lambda (w)
                       (if (procedure? (cadr w))
                           (cons (car w) (cadr w))
                           (cons (car w) (expr->pattern-proc (cadr w) envt))))
		     where)))

(define (parse-rule form)
  (apply make-rule form))

(define (add-constant (set <patterns>) const val)
  (set-compile-time-constants! 
   set
   (cons (cons const val) (compile-time-constants set)))
  (values))

(define (add-rule (set <patterns>) rule)
  (let* ((rule (if (instance? rule <rule>)
		   rule
		   (parse-rule rule)))
	 (tbl (rule-by-head set))
	 (head (car (pattern rule))))
    (table-insert! tbl head (append (or (table-lookup tbl head) '())
				    (list rule)))
    (values)))

(define (make-patterns name)
  (make <patterns> name: name))

(define-macro (define-patterns name . pat-list)
  (let* ((pats (symbol-append "*" name "-patterns*"))
	 (pre (if (and (pair? pat-list)
		       (eq? (car pat-list) 'pre:))
		  (let ((p (cadr pat-list)))
		    (set! pat-list (cddr pat-list))
		    (map (lambda (n)
			   (symbol-append "*" n "-patterns*"))
			 p))
		  '())))
    `(begin
       (define ,pats (make <patterns> 
			   pre-patterns: (list ,@pre)
			   name: ',name))
       (define (,name arg)
	 (transform-using-patterns arg ,pats))
       (for-each (lambda (p)
		   (add-rule ,pats p))
		 ',pat-list))))

(define-macro (define-pattern name pat)
  (let ((pats (symbol-append "*" name "-patterns*")))
    `(add-rule ,pats ',pat)))

;;;
;;;  build a pattern procedure out of a pattern expression
;;;
;;;  a pattern expression is an expression that includes
;;;  pattern variables like ?x and ?y
;;;
;;;  a pattern procedure is a procedure of one argument
;;;  (a bindings structure, as created by (make-bindings))
;;;  which evaluates the given expression

(define (expr->pattern-proc expr #optional envt)
  (let* ((patvars (vector->list (collect-pattern-vars expr)))
         (texpr `(lambda ($b)
                   (let (($a (',binding-lookuper $b)))
                     (let ,(map (lambda (v)
                                  `(,v ($a ',v)))
                                patvars)
                       ,expr)))))
    (if envt
        (eval texpr envt)
        (eval texpr))))

(define (collect-pattern-vars expr)
  (collect-select expr (lambda (item)
			 (and (symbol? item)
			      (pattern-var? item)))))

(define (collect-gensym-vars expr)
  (collect-select expr (lambda (item)
			 (and (symbol? item)
			      (gensym-var? item)))))

(define (collect-select expr p?)
  (if (p? expr)
      (vector expr)
      (if (pair? expr)
	  (vector-append (collect-select (car expr) p?)
			 (collect-select (cdr expr) p?))
	  '#())))
