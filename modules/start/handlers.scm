#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/start/handlers.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.16
 | File mod date:    1999-01-12 16:20:37
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  start
 |
 | Purpose:          standard exception handlers and dispatcher
 `------------------------------------------------------------------------|#

;; a `place' is a way of identifying where an error happened
;; currently, there are exactly two forms:
;;  (1) a template
;;  (2) a vector of three elements: [0] a template in the BCI
;;                                  [1] a BCI program counter
;;                                  [2] a BCI eval stack
;;         (the BCI eval stack will be #f if the info is unavailable)
;;

(define (place-name place)
  (if (vector? place)
      (set! place (vector-ref place 0)))
  (let ((info (assq 'function-scope (gvec-ref place 2))))
    (if info
	(cdr info)
	'(unknown))))

(define-class <runtime-error> (<error>) :abstract
  (runtime-error-place init-value: #f)) ;; #f, a <template> or a <vector>

(define-class <simple-runtime-error> (<runtime-error>)
  (runtime-error-msg type: <string>)
  (runtime-error-args type: <list>))

(define-class <formals-match-error> (<runtime-error>)
  (formals-min-argc type: <fixnum>)
  formals-max-argc ;; a <fixnum> or #f
  actual-argc)

(define-class <type-match-error> (<runtime-error>)
  actual-object
  (required-types type: <list>))  ;; implicit union-type

(define-method display-object ((self <runtime-error>) port)
  (let ((pl (runtime-error-place self)))
    (if (vector? pl)
	(format port "in: ~j\n" (template-place (vector-ref pl 0)))
	(if (instance? pl <template>)
	    (format port "in: ~j\n" (template-place pl))))))

(define-method display-object ((self <type-match-error>) port)
  (format port "type check failed: ~#@*30s\n" (actual-object self))
  (let ((rqn (map class-name (required-types self))))
    (if (= (length rqn) 1)
	(format port "is not a ~a\n" (car rqn))
	(format port "is not one of: ~j\n" rqn))
    (next-method)))

(define-method display-object ((self <simple-runtime-error>) port)
  (write-string port "runtime error: ")
  (apply format port (runtime-error-msg self) (runtime-error-args self))
  (newline port)
  (next-method))

(define-method display-object ((self <formals-match-error>) port)
  (format port "function called with ~d args\n\texpected " (actual-argc self))
  (if (formals-max-argc self)
      (if (eq? (formals-min-argc self) (formals-max-argc self))
	  (format port "exactly ~d\n" (formals-min-argc self))
	  (format port "between ~d and ~d\n" 
		  (formals-min-argc self)
		  (formals-max-argc self)))
	(format port "at least ~d\n"
		(formals-min-argc self)))
  (next-method))

;; called from scheme_error()

(define (scheme-error-handler literals-reg envt-reg msg arg-list)
  (error (make <simple-runtime-error>
	       runtime-error-place: literals-reg
	       runtime-error-msg: msg
	       runtime-error-args: arg-list)))

;; called from TLREF() and possibly TLSET()

(define (tlv-not-bound-handler place tlv)
  (error (make <simple-runtime-error>
	       runtime-error-place: place
	       runtime-error-msg: "top-level-var `~s' not bound"
	       runtime-error-args: (list (name tlv)))))

(define (call-to-non-function-handler place non-fn args)
  (error (make <simple-runtime-error>
	       runtime-error-place: place
	       runtime-error-msg: "call to non-function `~s'"
	       runtime-error-args: (list non-fn))))

(define (wrong-num-args-handler place argc min max)
  (error (make <formals-match-error>
	       runtime-error-place: place
	       formals-min-argc: min
	       formals-max-argc: max
	       actual-argc: argc)))

(define (type-check-failed-handler place val lst)
  (error (make <type-match-error>
	       runtime-error-place: place
	       actual-object: val
	       required-types: lst)))

(define (out-of-bounds-handler place object offset)
  (error (make <simple-runtime-error>
	       runtime-error-place: place
	       runtime-error-msg: "~d is an invalid offset into: ~#@*30s"
	       runtime-error-args: (list offset object))))

(define (os-glue-error-handler place rc)
  (error (make <simple-runtime-error>
	       runtime-error-place: place
	       runtime-error-msg: "operating system error code ~d"
	       runtime-error-args: (list rc))))

(define (type-assert-failed-handler place)
  (error (make <simple-runtime-error>
	       runtime-error-place: place
	       runtime-error-msg: "type assertion failed"
	       runtime-error-args: '())))

;;
;; this will remain true until the system is patched and saved
;; (it is set to #f just before the image is saved by the repl)
;;

(define *exceptions-abort-process* #t)

(define (set-exceptions-abort-process! flag)
  (set! *exceptions-abort-process* flag))

(define (exception-handler type . args)
  (if *exceptions-abort-process*
      ;; prevent recursion if `machine-panic' signals an error
      ;; by setting and checking a flag which tells us to
      ;; abort the process FIRST (ie, before calling `machine-panic')
      (if (eq? *exceptions-abort-process* 'first)
	  (process-abort)
	  (begin
	    (set! *exceptions-abort-process* 'first)
	    (machine-panic type args)))
      (case type
	((0) ;; scheme_error called from C
	 (apply* args scheme-error-handler))
	((1) ;; tlv not bound
	 (apply* args tlv-not-bound-handler))
	((2) ;; apply to non-function
	 (apply* args call-to-non-function-handler))
	((3) ;; out-of-bounds access
	 (apply* args out-of-bounds-handler))
	((4) ;; type-check failed
	 (apply* args type-check-failed-handler))
	((5) ;; os-glue-error
	 (apply* args os-glue-error-handler))
	((6) ;; assert-type
	 (apply* args type-assert-failed-handler))
	((7) ;; error object
	 (error (car args)))
	(else
	 (range-error exception-handler 0 0 5 type)))))

(define (dump-dynamic-state p)
  (write-string p "*** Dynamic state **\n")
  (let loop ((i 0) 
	     (f (get-dynamic-state-reg)))
    (if (pair? f)
	(begin
	  (format p "  [~d] ~s\n" i (car f))
	  (loop (+ i 1) (cdr f))))))

#|
(register-interrupt-handler! 'control-c 
			     (lambda ()
			       (fwrite/str (stderr) "** Interrupt! **\n")
			       (dump-dynamic-state
				(make <std-output-port>
				      file-stream: (stderr)))
			       (process-exit 2)))
|#

(define (machine-panic type args)
  ;;
  ;; #1 thing is to get a message out, before things get
  ;; worse trying to say more
  ;;
  (fwrite/str (stderr) "** Scheme Panic! **\n")
  (let ((p (make <std-output-port>
		 name: "stderr"
		 file-stream: (stderr))))
    (format p "** Exception type: ~a\n" (vector-ref '#("scheme-error"
						       "tlv-not-bound"
						       "apply-to-non-function"
						       "out-of-bounds"
						       "type-check-failed"
						       "os-error"
						       "assert-failed"
						       "<condition>")
						    type))
    (do ((i 0 (add1 i))
	 (a args (cdr a)))
	((null? a) (process-abort))
      (format p "** Exception data[~d]\n   ~#*70s\n" i (car a))
      (if (instance? (car a) <condition>)
	  (with-output-to-port
	      p
	    (lambda ()
	      (print-gvec (car a))))))))

;;;

(define *num-gc-flips* 0)

(define (handle-gc-flip)
  (set! *num-gc-flips* (add1 *num-gc-flips*)))

(define (handle-user-intr))

(define (ignore-timer))
(define (ignore-child-exited pid exit-type exit-code))

(define (handle-finalization lst)
  (let loop ((i lst))
    (if (pair? i)
	(begin
	  (finalize (car i))
	  (loop (cdr i))))))

(define *c-signal-handlers* '#())

(define (register-c-signal-handler! signame thunk)
  (if (eq? signame 'sigint)
      (register-interrupt-handler! 'user-intr thunk)
      (let ((i (vassq signame *c-signal-handlers*)))
	(if i
	    (vector-set! *c-signal-handlers* i thunk)
	    (begin
	      (setup-c-signal-handler! signame)
	      (set! *c-signal-handlers* (vinsert2 *c-signal-handlers*
						  signame
						  thunk))))
	(values))))

(define (handle-c-signal signame)
  (let ((h (vassq signame *c-signal-handlers*)))
    (if h
	((vector-ref *c-signal-handlers* h)))))

(%early-once-only
 (register-interrupt-handler! 'gc-flip handle-gc-flip)
 (register-interrupt-handler! 'finalize handle-finalization)
 (register-interrupt-handler! 'timer ignore-timer)
 (register-interrupt-handler! 'c-signal handle-c-signal)
 (register-interrupt-handler! 'child-exited ignore-child-exited)
 (register-interrupt-handler! 'user-intr handle-user-intr))

;;;
;;; this is called when no other handler is in place

(define (recursive-exception (c <condition>) next-handler)
  ;; forget `at-exit' cleanup, too
  (process-exit* 6))

(define (backstop-handler-proc (c <condition>) next-handler)
  (handler-bind
   (<condition> recursive-exception)
   (begin
     (fwrite/str (stderr) "** Uncaught condition\n")
     (display-object c $standard-error-port)
     (process-exit 2))))

(%early-once-only
 (define *backstop-handler-chain*
   (list (make <handler-context>
	       condition-class: <condition>
	       handler-proc: backstop-handler-proc))))

;; set up a brand new backstop chain
(set-direct-thread-init! 3 *backstop-handler-chain*)
