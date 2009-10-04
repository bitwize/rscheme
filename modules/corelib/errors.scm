#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/corelib/errors.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.26
 | File mod date:    2003-10-22 18:04:19
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  corelib
 |
 | Purpose:          centralized error reporting and signalling
 `------------------------------------------------------------------------|#

;;
;; error and signal are of two forms
;;
;; the first form takes a single <condition> argument
;; the second form takes a format string and format args

(define-method display-object ((self <condition>) port)
  (__format port "** Error: ~a\n" (class-name (object-class self))))

(define-class <condition-stack> (<object>)
  vm-literals-reg
  vm-continuation-reg
  vm-dynamic-state-reg
  vm-thread-state-reg
  vm-envt-reg)
  
(define *capture-stack-on-conditions* #t)

(define-method initialize ((self <condition>))
  (if *capture-stack-on-conditions*
      (set-properties! self (cons (cons 'stack (make-exception-stack))
				  (properties self))))
  self)



(define-method display-object ((self <os-error>) port)
  (__format port "error in system call ~a(" (system-call self))
  (let ((v (arguments self)))
    (let loop (((i <fixnum>) 0))
      (if (fixnum<? i (gvec-length v))
	  (begin
	    (__format port (if (eq? i 0) "~s" ",~s") (gvec-ref v i))
	    (loop (add1 i)))
	  (__format port ") (~a)\n" (errno-message (error-number self)))))))

;;

(define-method display-object ((self <type-check-failed>) port)
  (__format port "type check failed: expected a ~s, saw a ~s: ~s\n"
	    (type-check-required-type self)
            (class-name (object-class (type-check-actual-value self)))
	    (type-check-actual-value self))
  (__format port "  in: ~a\n" (name (template (place self)))))

;;


(define-method display-object ((self <argument-type-error>) port)
  (__format port "*** Error in ~a:\n"
	    (argument-error-function-name self))
  (__format port "   argument `~a' expected an instance of ~a\n"
	    (argument-error-bad-arg self)
	    (argument-required-type self))
  (if (pair? (argument-error-arguments self))
      (__format port "   got: ~#*@60s\n" 
		(car (argument-error-arguments self)))))

;;;

(define-class <argument-count-error> (<argument-error>)
  (argument-count-error-min-args type: <fixnum> init-value: 0)
  (argument-count-error-max-args init-value: #f)) ;; #f=>no max

;;;

(define (arg-type-error (fn <symbol>) (args <list>) bad rqd)
  (error (make <argument-type-error>
	       argument-error-function-name: fn
	       argument-error-arguments: args
	       argument-error-bad-arg: bad
	       argument-required-type: rqd)))

(define (arg-count-error (fn <symbol>) (args <list>) min max)
  (error (make <argument-count-error>
	       argument-error-function-name: fn
	       argument-error-arguments: args
	       argument-count-error-min-args: min
	       argument-count-error-max-args: max)))

;;

(define-class <handler-context> (<object>)
  condition-class
  ;; `handler-proc' is a procedure of two arguments:
  ;;    (1) the condition being signalled
  ;;    (2) a thunk whose invocation calls the next
  ;;        handler in the chain
  handler-proc)

#| pending resolution of CR 494 to put type restrictions on slots...
(define-class <handler-context> (<object>)
  (condition-class type: <<class>>)
  (handler-proc type: <function>))
|#

(define-thread-var *handler-chain* '())

(define-syntax (handler-bind (class fn) . body)
  (thread-let ((*handler-chain* (cons 
				(make <handler-context>
				      condition-class: class
				      handler-proc: fn)
				*handler-chain*)))
    (begin . body)))

(define-syntax mk-handler-context 
  (syntax-form (exit ((class) . body))
    (make <handler-context>
	  condition-class: class
	  handler-proc: (lambda (c next-handler)
			  (apply-before-continuing
			   exit
			   (lambda ()
			     (begin . body))))))
  (syntax-form (exit ((class 'condition: var) . body))
    (make <handler-context>
	  condition-class: class
	  handler-proc: (lambda (var next-handler)
			  (apply-before-continuing
			   exit
			   (lambda ()
			     (begin . body)))))))


(define-syntax (handler-case expr . cases)
  (call-with-current-continuation
   (lambda (exit)
     (letrec-syntax ((setup-handlers (syntax-form ()
				       *handler-chain*)
				     (syntax-form (handler-spec . more)
				       (cons (mk-handler-context 
					      exit
					      handler-spec)
					     (setup-handlers . more)))))
       (thread-let ((*handler-chain* (setup-handlers . cases)))
	 expr)))))

(define (default-signal-handler (c <condition>))
  (let loop ((chain *handler-chain*))
    (if (pair? chain)
	(let (((h <handler-context>) (car chain)))
	  (if (instance? c (condition-class h))
	      ((handler-proc h) 
	       ;; first arg to handler function is the condition
	       c
	       ;; second arg is the next-handler proc
	       (lambda ()
		 ;; work around bug in compiler that will think `loop'
		 ;; is optimizable because it's only ever called from
		 ;; tail position -- never mind it's in a different proc!
		 loop
		 ;; keep looking...
		 (loop (cdr chain))))
	      (loop (cdr chain))))
	(process-abort "no handler in place for condition" 
		       (class-name (object-class c))))))

(define-fluid *signal-handler* default-signal-handler)

;;; `signal' may return, if the signal handler continues
;;; execution.  Note that a `handler-case' construct will
;;; never cause signal to return, but a `handler-bind'
;;; construct might.

(define (signal . args)
  (let ((c (if (null? args)
	       (make <simple-warning>)
	       (if (string? (car args))
		   (make <simple-warning>
			 simple-condition-msg: (car args)
			 simple-condition-args: (cdr args))
		   (if (instance? (car args) <condition>)
		       (if (null? (cdr args))
			   (car args)
			   (arg-count-error 'signal args 1 1))
		       (arg-type-error 'signal 
				       args
				       (car args) 
				       <condition>))))))
    ((fluid-ref *signal-handler*) c)))

;; error never returns

(define (error . args)
  (let ((c (if (null? args)
	       (make <simple-error>)
	       (if (string? (car args))
		   (make <simple-error>
			 simple-condition-msg: (car args)
			 simple-condition-args: (cdr args))
		   (if (instance? (car args) <condition>)
		       (if (null? (cdr args))
			   (car args)
			   (arg-count-error 'error args 1 1))
		       (arg-type-error 'error 
				       args
				       (car args) 
				       <condition>))))))
    ((fluid-ref *signal-handler*) c)
    (error-protocol-failure c)))

(define error-protocol-failure 
  (lambda (c)
    (error "error protocol failure; handler returned")))

(define (set-error-protocol-failure-handler! proc)
  (set! error-protocol-failure proc)
  (values))

(define (type-error in-function argn-in-error value description)
  (error "type-error: bad arg[~d] to ~s: ~s is ~a"
	 argn-in-error
	 in-function
	 value
	 description))

#|
  (signal (make <type-error>
		erroneous-arg-num: argn-in-error
		in-function: in-function
		erroneous-arg-value: value
		description: (format "bad arg[~d] to ~s: ~s is ~a"
				     argn-in-error
				     (function-name in-function)
				     value
				     description)))
|#

(define (range-error in-function argn-in-error min lim val)
  (error "range-error: bad arg[~d] to ~s: ~s not in [~d,~d)"
	 argn-in-error
	 in-function
	 val min lim))

(define (limit-check op (index <fixnum>) (limit <fixnum>))
  (error "~a: index ~d out of bounds 0..~d" 
	 op
	 index
	 (sub1 limit)))

(define __format #f)
(define (set__format f) (set! __format f))

(define (abort place msg . args)
  (error "abort in ~s: ~a\n*** NOTE: abort is obsolete ***\n"
	 place
	 (apply* #f msg args __format)))

(define-syntax (assert expr)
  (if (not expr)
      (error "assertion failed in ~s: ~s"
	     (*FUNCTION*)
	     (mquote expr))))

;;;
;;;  Collection Conditions
;;;  =====================
;;;

(define-class <no-such-key> (<condition>)
  (collection type: <collection>)
  (key type: <object>))

(define-method display-object ((self <no-such-key>) port)
  (__format port "Collection `~#*@40s'\n  has no key: ~#@50s\n"
	    (collection self)
	    (key self)))

(define (signal-no-such-key collection key)
  (signal
   (make <no-such-key>
	 collection: collection
	 key: key)))

;;;

(define-class <no-last-element> (<condition>)
  collection)

(define-method display-object ((self <no-last-element>) port)
  (__format port "Collection `~#*@40s'\n  has no last element\n"
	    (collection self)))

;;;

(define-class <improper-list> (<condition>)
  collection
  improper-part)

(define-method display-object ((self <improper-list>) port)
  (__format port "Improper list at ~#*@20s\n  in: ~#*@40s\n" 
	    (improper-part self)
	    (collection self)))

(define (signal-improper-list lst at)
  (signal (make <improper-list>
		collection: lst
		improper-part: at)))

;;;

(define-class <no-properties> (<condition>)
  object)

(define-method display-object ((self <no-properties>) port)
  (__format port "Cannot set a property on: ~#@*50s\n" (object self))
  (__format port "   The class ~s does not have a properties slot\n"
	    (class-name (object-class (object self)))))

(define-method properties ((self <object>))
  '#())

(define-method set-properties! ((self <object>) value)
  (signal (make <no-properties>
		object: self)))

		

