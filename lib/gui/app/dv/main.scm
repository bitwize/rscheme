
(define (my-backstop-error-handler condition nexth)
  (let ((e (current-error-port)))
    (format e "-- Backstop error --\n~a" condition)
    (process-exit 2)))

(define (bail-out condition next-handler)
  (let ((p (current-error-port)))
    (format p "Fatal (uncaught) error:\n~a" condition)
    (format p "~a\n" (make-string 72 #\-))
    (with-module repl (apply-backtrace condition))
    (format p "~a\n" (make-string 72 #\-))
    (process-exit 1)))

(define (make-dv-user-envt)
  (handler-case
   (with-module repl
     (with-module mlink
       (bind ((m e (make-module 'dv)))
	 (module-uses-module m 'rs.lang (get-module 'rs.lang))
	 (module-uses-module m 'gui.app.dv (get-module 'gui.app.dv))
	 e)))
   ((<condition> condition: c)
    ;; fallback to using THIS envt (e.g., if gui.app.dv is not defined)
    *self*)))

(define (dv-main #optional args)
  (format #t "dV -- An Extensible Graphics Editor\n")
  (format #t "      Copyright (C) 1999 Donovan Kolbly\n")
  ;;
  (set-handler-proc! (last *handler-chain*) my-backstop-error-handler)
  ;
  (let ((envt (make-dv-user-envt)))
    (with-module
	paths
      (if (file-exists? "~/.dvrc")
	  (with-module
	      repl
	    (load-into envt "~/.dvrc"))))
    ;
    (handler-bind (<condition> bail-out)
      (with-module editinp
	(with-edit-port
	 (current-input-port)
	 (current-output-port)
	 (current-error-port)
	 (lambda ()
	   (if (and (pair? args) (string=? (car args) "-2"))
	       (start-dv *self* #t (cdr args))
	       (start-dv envt #t args))))))))

(define (process-args envt args)
  (if (pair? args)
      (cond
       ((string=? (car args) "-e")
	(with-module
	    repl
	  (load-into envt (cadr args)))
	(process-args envt (cddr args)))
       ((string=? (car args) "-exit")
        (process-exit 0))
       ((string=? (car args) "-n")
        (make-new-doc)
        (process-args envt (cdr args)))
       (else
	(if (char=? #\- (string-ref (car args) 0) )
	    (begin
	      (wm "~s: unrecognized option" (car args))
	      (process-args envt (cdr args)))
	    (begin
	      (if (with-module paths (file-exists? (car args)))
		  (load-file (car args))
		  (wm "~a: file does not exist" (car args)))
	      (process-args envt (cdr args))))))))

(define (start-dv envt #optional interactive args)
  (with-client
   ;; `make-client' implicitly starts the event loop and
   ;; brings up the main menu
   (make-client (or (getenv "DISPLAY")
		    (em 193 "DISPLAY variable not set")))
   (lambda ()
     ;
     (if (pair? args)
	 (process-args envt args))
     ;
     (if interactive
	 (with-module repl
	   (cmd-loop envt "dv[~d]=>"))
	 (force (client-exit (current-client)))))))
