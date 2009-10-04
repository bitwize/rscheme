#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/repl/cmdprocs.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.16
 | File mod date:    1999-01-28 10:03:58
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  repl
 |
 | Purpose:          Special command-loop commands (`comma' commands)
 `------------------------------------------------------------------------|#

(define *command-procs* '())
(define *installed-default-command-procs?* #f)

(define (command-proc-bdg name)
  (let ((b (assq name *command-procs*)))
    (if b
	(cadr b)
	#f)))

(define (with-command-procs proc-descriptors thunk)
  (let loop ((cp-list (fluid-ref *command-procs*))
	     (pds proc-descriptors))
    (if (null? pds)
	(fluid-let ((*command-procs* (append proc-descriptors cp-list)))
	  (thunk))
	(let ((a (assq (car (car pds)) cp-list)))
	  (loop (if a
		    (delq a cp-list)
		    cp-list)
		(cdr pds))))))

;;;
;;; returns #f if the specified form was not handled
;;;

(define (run-special-command form envt)
  (if (and (pair? form)
	   (eq? (car form) 'unquote))
      (if (and (pair? (cdr form))
	       (null? (cddr form)))
	  (let ((body (cadr form)))
	    (if (not *installed-default-command-procs?*)
		(begin
		  (set! *installed-default-command-procs?* #t)
		  (make-default-command-procs)))
	    (cond
	     ((symbol? body)
	      ;;
	      ;; form 1.  ,FOO
	      ;;
	      (let ((bdg (command-proc-bdg body)))
		(if bdg
		    (bdg envt #f)
		    (format #t "`,~s' is not a recognized ,-command\n"
			    body))))
	     ((and (pair? body)
		   (symbol? (car body)))
	      ;;
	      ;; form 2.  ,(FOO ...)
	      ;;
	      (let ((bdg (command-proc-bdg (car body))))
		(if bdg
		    (bdg envt (cdr body))
		    (format #t "`,~s' is not a recognized ,-command\n"
			    body))))
	     (else
	      ;; 
	      ;; unrecognized form
	      ;;
	      (display "misformed `,'-command -- use `,help' for help\n")))
	    #t)
	  (display  "misformed `,'-command -- use `,help' for help\n"))
      #f))

(define (cmd-proc name thunk)
  (lambda (envt args)
    (if args
	(format #t ",~s used with arguments; expected ,~s\n"
		(cons name args)
		name)
	(thunk envt))))

(define (cmd-func proc)
  (lambda (envt args)
    (proc envt (or args '()))))

(define *trace-bci* #f)
(define *trace-apply* #f)

(define-syntax define-command-proc
  (syntax-form ((name) func doc)
    (set! *command-procs*
	  (cons (cons (mquote name)
		      (cons (cmd-func func) (mquote doc)))
		*command-procs*)))
  (syntax-form (name thunk doc)
    (set! *command-procs*
	  (cons (cons (mquote name)
		      (cons (cmd-proc (mquote name) thunk) (mquote doc)))
		*command-procs*)))
  (syntax-form ((name) func)
    (set! *command-procs*
	  (cons (cons (mquote name)
		      (cons (cmd-func func) '()))
		*command-procs*)))
  (syntax-form (name thunk)
    (set! *command-procs*
	  (cons (cons (mquote name)
		      (cons (cmd-proc (mquote name) thunk) '()))
		*command-procs*))))
    

(define (make-default-command-procs)
  (define-command-proc 
	h 
	show-help 
	((",h ,? ,help" "print this help info")))
  (define-command-proc 
	? 
	show-help)
  (define-command-proc 
	help 
	show-help)
  (define-command-proc 
	(up) 
	go-up 
	((",up" "exit to outer REPL (alt: ,(up n))")))
  (define-command-proc 
	top 
	go-top 
	((",top" "exit to outer-most REPL")))
  (define-command-proc 
    bt
    show-bt
    ((",bt" "show continuation chain")))
  (define-command-proc 
    warranty 
    show-warranty 
    ((",warranty" "show (lack of) warranty")))

  (bind ((opts (get-compile-options)))
    ;;
    (if (memq 'apply-backtrace opts)
	(begin
	  (define-command-proc 
	    abt 
	    abt-cmd
	    ((",abt" "show APPLY back trace")))
	  (define-command-proc
	    fg-abt
	    (lambda (envt)
	      (set! *trace-apply* #t))
	    ((",fg-abt" "turn on APPLY tracing")))
	  (define-command-proc
	    no-fg-abt
	    (lambda (envt)
	      (set! *trace-apply* #f)))))
    ;;
    (if (memq 'bci-trace opts)
	(begin
	  (define-command-proc 
	    bci 
	    (lambda (envt)
	      (set! *trace-bci* #t))
	    ((",bci" "trace bytecode interpretation")))
	  (define-command-proc 
	    no-bci 
	    (lambda (envt) (set! *trace-bci* #f))))))

  (define-command-proc 
	aml 
	(lambda (envt) (set! *show-aml* #t)) 
	((",aml" "show generated AML code")))
  (define-command-proc no-aml (lambda (envt)
				(set! *show-aml* #f)))
  (define-command-proc 
	(use) 
	cmd-proc/use 
	((",(use m ...)" "import variables from modules")))

  (define-command-proc
        (export)
        cmd-proc/export
        ((",(export var ...)" "export variables from module")))

  (define-command-proc 
	(width)
	cmd-proc/width
	((",(width w)" "set display width limit")))
  (define-command-proc 
	(apropos) 
	cmd-proc/apropos 
	((",(apropos str)" "find bindings with given substring")))
  (define-command-proc 
	(exit) 
	cmd-proc/exit 
	((",exit" "exit process")
	 (",(exit code)" "exit process with code"))))

(define (cmd-proc/exit envt args)
  (if (null? args)
      (process-exit 1)
      (if (fixnum? (car args))
	  (process-exit (car args))
	  (format #t 
		  ",(exit ~s) is invalid; expected a fixnum\n"
		  (car args)))))

(define (cmd-proc/width envt args)
  (if (or (not (= (length args) 1))
	  (not (fixnum? (car args))))
      (format #t ",~s is invalid; expected ,(width <fixnum>)\n"
	      (cons 'width args))
      (set-display-limit! (car args))))

(define (cmd-proc/export envt args)
  (let* ((e (the-top-level envt))
         (m (owner e))
         (x (module-exports m)))
    (for-each 
     (lambda (n)
       (table-insert! x n (lookup e n)))
     args)))

(define (cmd-proc/use envt args)
  (for-each (lambda (n)
	      (if (symbol? n)
		  (use-in n (the-top-level envt))
		  (format #t 
			  ",(use ~s) is invalid; expected a symbol\n"
			  n)))
	    args))

(define (end-of-repl envt args)
  (apply (cmd-loop-exit *cmd-loop*)
	 (map (lambda (expr)
		(eval-in-envt expr envt))
	      (or args '()))))

(define (go-up-n n)
  (let loop ((n n) (cl *cmd-loop*))
    (if (eq? n 0)
	((cmd-loop-restart cl))
	(if (outer-loop cl)
	    (loop (and n (- n 1)) (outer-loop cl))
	    (loop 0 cl)))))

(define (go-up envt args)
  (if (null? args)
      (go-up-n 1)
      (if (fixnum? (car args))
	  (go-up-n (car args))
	  (format #t ",(up ~s) invalid; expected a fixnum\n"))))

(define (go-top envt)
  (go-up-n #f))

(define (cmd-proc/apropos envt args)
  (if (null? args)
      (format #t "usage: ,(apropos /key-string/)\n")
      (let* ((arg1 (car args))
	     (arg-as-string (if (string? arg1)
				arg1
				(if (symbol? arg1)
				    (symbol->string arg1)
				    #f))))
	(if arg-as-string
	    (apropos* arg-as-string
		      (list (cons '*self* (table envt))))
	    (format #t "usage: ,(apropos /key-string/)\n")))))

(define (show-help envt)
  (display "Help for `,' commands:\n")
  (display-help-table (apply append (map cddr *command-procs*)))
  (display "(Note: a `no-' prefix disables flags, ")
  (display "e.g. `,no-aml')\n"))

(define *warranty*
"RScheme comes with ABSOLUTELY NO WARRANTY.  Because it is free, and
made available for your use with no compensation due, I cannot warrant
it to be suitable to any purpose, to be free of defects, or to comply
with any standard, etc.

For the latest developments in RScheme, see <http://www.rscheme.org/>\n")

(define (show-warranty envt)
    (display *warranty*))

(define (display-help-table info)
  (let ((w (apply max (map string-length (map car info)))))
    (define (print-1 item)
	(format #t "    ~a~a -- ~a\n"
		(car item)
		(make-string (- w (string-length (car item)))
			    #\space)
		(cadr item)))   
    (for-each print-1 info)))

;;

(define (apropos* str altspace)
  (let* ((im (installed-modules))
	 (found (make-symbol-table))
	 (match-name-and-not-found? (lambda ((n <symbol>))
				      (and (string-search (symbol->string n)
							  str)
					   (not (table-lookup found n)))))
	 (match-name? (lambda ((name <symbol>))
			(and (string-search (symbol->string name) str)
			     #t))))
    (for-each 
     (lambda (module-name table is-module?)
       (let ((apropos-in-module (select (if is-module?
					    match-name?
					    match-name-and-not-found?)
					(key-sequence table))))
	 (if (pair? apropos-in-module)
	     (begin
	       (for-each (lambda (n)
			   (table-insert! found n #t))
			 apropos-in-module)
	       (format #t "~-11a :" module-name)
	       (let loop ((i 14)
			  (s (sort (map symbol->string apropos-in-module)
				   string<?)))
		 (if (pair? s)
		     (let (((n <string>) (car s)))
		       (if (< (+ i (string-length n)) 75)
			   (begin
			     (format #t " ~a" n)
			     (loop (+ i 1 (string-length n)) (cdr s)))
			   (begin
			     (format #t "\n              ~a" n)
			     (loop (+ 14 (string-length n)) (cdr s)))))
		     (newline)))))))
     (append (map car im) 
	     (map car altspace))
     (append (map module-exports (map cdr im))
	     (map cdr altspace))
     (append (map (lambda (x) #t) im)
	     (map (lambda (x) #f) altspace)))
    (values)))
