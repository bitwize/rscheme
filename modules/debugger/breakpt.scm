#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/debugger/breakpt.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.11
 | File mod date:    1998-12-18 21:56:03
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  debugger
 |
 | Purpose:          break-points and traces
 `------------------------------------------------------------------------|#

(define-class <breakpoint> (<object>)
  name
  output-port
  (breakpoint-for type: <function>)
  (backup-template init-value: #f)
  breakpoint-num)

(define-class <tracepoint> (<breakpoint>)
  (call-count type: <fixnum> init-value: 0))

#|
(define-class <timepoint> (<breakpoint>)
  (total-calls type: <fixnum> init-value: 0)
  (total-time init-value: 0))
|#

(define-method installed? ((self <breakpoint>))
  (if (backup-template self)
      #t
      #f))

(define-generic-function gen-wrapper-template)

(define-method install-breakpoint ((b <breakpoint>))
  (if (not (installed? b))
      (let ((f (breakpoint-for b)))
	(set-backup-template! b (template f))
	(set-template! f (gen-wrapper-template b))))
  b)

(define-method deinstall-breakpoint ((b <breakpoint>))
  (if (installed? b)
      (begin
	(set-template! (breakpoint-for b) (backup-template b))
	(set-backup-template! b #f)))
  b)

(define-method write-object ((self <breakpoint>) port)
  (format port "#[~a #~d]" 
	  (class-name (object-class self))
	  (breakpoint-num self)))

(define *breakpoint-counter* 0)
(define *all-breakpoints* '())

(define (search-for-breakpoint query key)
  (let loop ((i *all-breakpoints*))
    (if (null? i)
	#f
	(if (eq? (query (car i)) key)
	    (car i)
	    (loop (cdr i))))))

(define (get-breakpoint-by-fn fn)
  (search-for-breakpoint breakpoint-for fn))

(define (get-breakpoint-by-id id)
  (search-for-breakpoint breakpoint-num id))

(define (breakpoint-on (f <function>))
  (get-breakpoint-by-fn f))
  
;;

(define (lookup-proc for-whom name envt)
  (let ((bdg (lookup envt name)))
    (if bdg
	(if (instance? bdg <top-level-var>)
	    (if (procedure? (value bdg))
		(value bdg)
		(signal ",(~s ~s): not a procedure\n"
			for-whom name))
	    (signal ",(~s ~s): not a top-level var\n"
		    for-whom name))
	(signal ",(~s ~s): not bound\n" for-whom name))))

(define (get-breakpoint for-whom id envt)
  (cond
   ((and (symbol? id) envt)
    ;; do a function lookup
    (let ((proc (lookup-proc for-whom id envt)))
      (or (get-breakpoint-by-fn proc)
	  (error ",(~s ~s): no breakpoint set on ~s\n" for-whom id proc))))
   ;;
   ((fixnum? id)
    ;; do a number lookup
    (or (get-breakpoint-by-id id)
	(error ",(~s ~s): breakpoint #~d not defined\n"
	       for-whom id id)))
   ;; error
   (else
     (error ",(~s ~s): expected breakpoint #~a"
	    for-whom id
	    (if envt " or fn name")))))

(define (make-breakpoint (fn <function>) name b-class)
  (let ((b (make b-class
		 name: name
		 output-port: (current-output-port)
		 breakpoint-for: fn
		 breakpoint-num: *breakpoint-counter*)))
    (set! *all-breakpoints* (cons b *all-breakpoints*))
    (set! *breakpoint-counter* (+ *breakpoint-counter* 1))
    b))

(define-method delete-breakpoint ((b <breakpoint>))
  (if (breakpoint-for b)
      (begin
	(deinstall-breakpoint b)
	(set! *all-breakpoints* (delq! b *all-breakpoints*))
	(set-breakpoint-for! b #f)
	b)
      (signal "(delete-breakpoint ~d): already deleted" (breakpoint-num b))))

(define (mk-standalone proc)
  (let ((t (clone standalone-template)))
    (gvec-set! t 3 proc)
    t))

(define (notify-break (self <breakpoint>) args prefix)
  (let ((port (output-port self)))
    (format port "~a ~s: (~d arguments)\n" prefix (name self) (length args))
    (for-each (lambda (i a)
		(format port "   arg[~d] := ~#*@60s\n" i a))
	      (range (length args))
	      args)))

(define (make-continue-cmd-proc (self <breakpoint>))
  (list 'continue
	end-of-repl
	(list ",continue"
	      (format #f "execute body of ~a and return result" (name self)))))

(define (make-return-cmd-proc (self <breakpoint>) exit)
  (list 'return
	(lambda (envt args)
	  (apply exit
		 (map (lambda (expr)
			(eval-in-envt expr envt))
		      (or args '()))))
	(list ",(return expr...)"
	      (format #f "return exprs from ~a" (name self)))))

(define-method gen-wrapper-template ((self <breakpoint>))
  (mk-standalone
   (lambda (proc args)
     (let ((port (output-port self))
	   (bak (backup-template self)))
       ;;
       (notify-break self args "break in")
       (format port "(,continue to complete the call\n ,(return expr ...) to return exprs instead)\n")
       (call-with-current-continuation
	(lambda (exit)
	  (with-command-procs
	   (list
	    (make-return-cmd-proc self exit)
	    (make-continue-cmd-proc self))
	   (lambda ()
	     (sub-loop *cmd-loop* "BRK[~d]=>")
	     (apply-template bak proc args)))))))))

(define-method gen-wrapper-template ((self <tracepoint>))
  (mk-standalone
   (lambda (proc args)
     (let ((port (output-port self))
	   ((n <fixnum>) (call-count self)))
       (set-call-count! self (add1 n))
       (notify-break self args (format #f "entry[~d] to" n))
       (bind ((#rest result (apply-template (backup-template self) proc args)))
	 (format port "return[~d] from ~s:" n (name self))
	 (case (length result)
	   ((0) (format port "--void--\n")
		(values))
	   ((1) (format port " ~#*@50s\n" (car result))
		(car result))
	   (else
	    (newline port)
	    (trace-return-values result port)
	    (list->values result))))))))

(define (trace-return-values v port)
  (let loop ((i 0) (v v))
    (if (not (null? v))
	(begin
	  (format #t "    return value[~d] := ~#*@50s\n" i (car v))
	  (loop (add1 i) (cdr v))))))


(define (cmd-proc/no-break envt args)
  (if args
      (for-each (lambda (i)
		  (let ((b (get-breakpoint 'no-break i envt)))
		    (if b
			(deinstall-breakpoint b))))
		args)
      (error ",no-break: expected function name or breakpt # arguments\n")))

(define (cmd-proc/trace envt args)
  (brkpt-install envt args <tracepoint> 'trace))

(define (cmd-proc/break envt args)
  (brkpt-install envt args <breakpoint> 'break))

(define (cmd-proc/breaks envt)
  (for-each 
   print-breakpoint
   (sort *all-breakpoints*
	 (lambda (a b)
	   (< (breakpoint-num a)
	      (breakpoint-num b))))))

(define (install-one-brkpt target-fn target-fn-name b-class cmd)
  (let ((b (get-breakpoint-by-fn target-fn)))
    (if (or (not b) (eq? (object-class b) b-class))
	(let ((b (or b (make-breakpoint target-fn target-fn-name b-class))))
	  (install-breakpoint b)
	  (print-breakpoint b))
	(begin
	  (delete-breakpoint b)
	  (install-one-brkpt target-fn target-fn-name b-class cmd)))))

(define (brkpt-install envt args b-class cmd)
  (for-each (lambda (n)
	      (cond
	       ((symbol? n)
		(install-one-brkpt (lookup-proc cmd n envt)
				   n
				   b-class
				   cmd))
	       ((fixnum? n)
		(let ((b (get-breakpoint-by-id n)))
		  (if b
		      (install-one-brkpt (breakpoint-for b)
					 (name b)
					 b-class
					 cmd)
		      (error ",(~s ~s): ~s not a current breakpoint id"
			     cmd n n))))
	       (else
		(error ",(~s ~s): expected a function name\n" 
		       cmd n))))
	    args))

(define (print-breakpoint b)
  (format #t "~s: break on ~s~a\n" 
	  b 
	  (name b)
	  (if (installed? b)
	      " (installed)"
	      " (disabled)")))

(%early-once-only
(define-command-proc breaks
  cmd-proc/breaks
  ((",breaks" "list active breakpoints")))

(define-command-proc (break) 
  cmd-proc/break
  ((",(break proc...)" "install breakpoints on procs")))

(define-command-proc (trace) 
  cmd-proc/trace
  ((",(trace proc...)" "install tracepoints on procs")))

(define-command-proc (no-break) 
  cmd-proc/no-break
  ((",(no-break proc/id...)" "remove a breakpoint or trace")))
)
