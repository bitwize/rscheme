;;;
;;;  RScheme prelude
;;;

(define-macro (include file)
  (if (equal? file "myenv.scm")
      '(values)
      `(load ,file)))

(define-macro (declare . whatever)
  '(values))

; assert the truth of an expression (or of a sequence of expressions)
;
; syntax: assert ?expr ?expr ... [report: ?r-exp ?r-exp ...]
;
; If (and ?expr ?expr ...) evaluates to anything but #f, the result
; is the value of that expression.
; If (and ?expr ?expr ...) evaluates to #f, an error is reported.
; The error message will show the failed expressions, as well
; as the values of selected variables (or expressions, in general).
; The user may explicitly specify the expressions whose
; values are to be printed upon assertion failure -- as ?r-exp that
; follow the identifier 'report:'
; Typically, ?r-exp is either a variable or a string constant.
; If the user specified no ?r-exp, the values of variables that are
; referenced in ?expr will be printed upon the assertion failure.

(define-macro (assert expr . others)
			; given the list of expressions or vars,
			; make the list appropriate for cerr
  ;;
  (define (make-print-list lst)
    (cond
     ((null? lst) '())
     ((symbol? (car lst))
      (cons* (symbol->keyword (car lst))
             (car lst) 
             (make-print-list (cdr lst))))
     (else 
      (cons (car lst) (make-print-list (cdr lst))))))
  ;;
			; return the list of all unique "interesting"
			; variables in the expr. Variables that are certain
			; to be bound to procedures are not interesting.
  (define (vars-of expr)
    (let loop ((expr expr) (vars '()))
      (cond
       ((not (pair? expr)) vars)	; not an application -- ignore
       ((memq (car expr) 
	      '(quote let let* letrec let*-values lambda cond quasiquote
		      case define do assert))
	vars)				; won't go there
       (else				; ignore the head of the application
	(let inner ((expr (cdr expr)) (vars vars))
	  (cond 
	   ((null? expr) vars)
	   ((symbol? (car expr))
	    (inner (cdr expr)
		   (if (memq (car expr) vars) vars (cons (car expr) vars))))
	   (else
	    (inner (cdr expr) (loop (car expr) vars)))))))))

  (cond
   ((null? others)		; the most common case
    `(or ,expr (signal-ssax-assertion-failure
                ',expr
                ,@(make-print-list (vars-of expr)))))
   ((eq? (car others) 'report:) ; another common case
    `(or ,expr (signal-ssax-assertion-failure
                ',expr
                ,@(make-print-list  (cdr others)))))
   ((not (memq 'report: others))
    `(or (and ,expr ,@others)
	 (signal-ssax-assertion-failure
          '(,expr ,@others)
          ,@(make-print-list
             (vars-of (cons 'and (cons expr others)))))))
   (else			; report: occurs somewhere in 'others'
    (let loop ((exprs (list expr)) (reported others))
      (cond
       ((eq? (car reported) 'report:)
	`(or (and ,@(reverse exprs))
	     (signal-ssax-assertion-failure
              ',(reverse exprs)
              ,@(make-print-list (cdr reported)))))
       (else (loop (cons (car reported) exprs) 
                   (cdr reported))))))))

(define-class <ssax-assertion-failure> (<condition>)
  expr
  bindings)

(define (signal-ssax-assertion-failure expr . bindings)
  (signal (make <ssax-assertion-failure>
                expr: expr
                bindings: bindings)))

(define-method display-object ((self <ssax-assertion-failure>) port)
  (format port "*** SSAX assertion failure ***\n")
  (format port "    ~#@*60s\n" (expr self))
  (let loop ((l (bindings self)))
    (if (pair? l)
        (if (keyword? (car l))
            (begin
              (format port "    ~s ~#@*60s\n" (car l) (cadr l))
              (loop (cddr l)))
            (begin
              (format port "    ~#*@60s\n" (car l))
              (loop (cdr l)))))))

;;;

(define-macro (assure exp error-msg) `(assert ,exp report: ,error-msg))
            
(define (identify-error msg args . disposition-msgs)
  (let ((port (current-error-port)))
    (newline port)
    (display "ERROR " port)
    (display msg port)
    (for-each (lambda (msg) (display msg port))
	      (append args disposition-msgs))
    (newline port)))

; like cout << arguments << args
; where argument can be any Scheme object. If it's a procedure
; (without args) it's executed rather than printed (like newline)

(define (cout . args)
  (for-each (lambda (x)
              (if (procedure? x) (x) (display x)))
            args))

(define (cerr . args)
  (for-each
   (lambda (x)
     (if (procedure? x)
         (x (current-error-port)) 
         (display x (current-error-port))))
   args))

(define nl "\n")

; Some useful increment/decrement operators

				; Mutable increment
(define-macro (++! x) `(set! ,x (+ 1 ,x)))

				; Read-only increment
(define-macro (++ x) `(+ 1 ,x))

				; Mutable decrement
(define-macro (--! x) `(set! ,x (- ,x 1)))

				; Read-only decrement
(define-macro (-- x) `(- ,x 1))

; Some useful control operators

			; if condition is true, execute stmts in turn
			; and return the result of the last statement
			; otherwise, return #f
(define-macro (when condition . stmts)
  `(and ,condition (begin ,@stmts)))
  

			; if condition is false execute stmts in turn
			; and return the result of the last statement
			; otherwise, return #t
			; This primitive is often called 'unless'
(define-macro (whennot condition . stmts)
  `(or ,condition (begin ,@stmts)))


			; Execute a sequence of forms and return the
			; result of the _first_ one. Like PROG1 in Lisp.
			; Typically used to evaluate one or more forms with
			; side effects and return a value that must be
			; computed before some or all of the side effects happen.
(define-macro (begin0 form . forms)
  (let ((var (gensym)))
    `(let ((,var ,form)) ,@forms ,var)))

			; Prepend an ITEM to a LIST, like a Lisp macro PUSH
			; an ITEM can be an expression, but ls must be a VAR
(define-macro (push! item ls)
  `(set! ,ls (cons ,item ,ls)))

			; Is str the empty string?
			; string-null? str -> bool
			; See Olin Shiver's Underground String functions
(define-macro (string-null? str) `(zero? (string-length ,str)))

(define-macro (let*-values bindings . body)
  `(bind ,(map (lambda (b)
                 (append (car b) (list (cadr b))))
               bindings)
     ,@body))

(define-macro (assq-def key alist . default-action-arg)
  (let ((default-action
        (if (null? default-action-arg)
          `(error "failed to assq key '" ,key "' in a list " ,alist)
          (let ((defact-symb (car default-action-arg)))
	    (if (or (symbol? defact-symb) (pair? defact-symb))
		`(if (procedure? ,defact-symb) (,defact-symb) ,defact-symb)
		defact-symb)))))
    `(or (assq ,key ,alist) ,default-action)))

(define-macro (assv-def key alist . default-action-arg)
  (let ((default-action
        (if (null? default-action-arg)
          `(error "failed to assv key '" ,key "' in a list " ,alist)
          (let ((defact-symb (car default-action-arg)))
	    (if (or (symbol? defact-symb) (pair? defact-symb))
		`(if (procedure? ,defact-symb) (,defact-symb) ,defact-symb)
		defact-symb)))))
    `(or (assv ,key ,alist) ,default-action)))

(define-macro (assoc-def key alist . default-action-arg)
  (let ((default-action
        (if (null? default-action-arg)
          `(error "failed to assoc key '" ,key "' in a list " ,alist)
          (let ((defact-symb (car default-action-arg)))
	    (if (or (symbol? defact-symb) (pair? defact-symb))
		`(if (procedure? ,defact-symb) (,defact-symb) ,defact-symb)
		defact-symb)))))
    `(or (assoc ,key ,alist) ,default-action)))

(define (call-with-input-string str proc)
  (proc (open-input-string str)))

(define (error . $args)
  (with-module corelib
    (error "*ERROR* ~j" $args)))

(define (thunk-failed? thunk)
  (handler-case
   (begin 
     (thunk)
     #f)
   ((<condition>) #t)))

(define-macro (failed? . stmts)
  `(thunk-failed? (lambda () ,@stmts)))

(define (values* . items)
  (list->values items))

(define ascii->char integer->char)

(define-macro (define-opt bindings body . body-rest)
  (let loop ((curr bindings) (reqd '()))
    (cond
      ((not (pair? curr))			; No optional bindings,
	`(define ,bindings ,body ,@body-rest))  ; regular define
      ((and (pair? (car curr)) (eq? 'optional (caar curr)))
	`(define ,(append (reverse (cons '#optional reqd))
		    (cdar curr) (cdr curr))
	   ,body ,@body-rest))
      (else (loop (cdr curr) (cons (car curr) reqd))))))
