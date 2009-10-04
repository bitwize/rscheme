#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/repl/replcli.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.46
 | File mod date:    2004-07-02 08:05:09
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  repl
 |
 | Purpose:          REPL command-line interface
 `------------------------------------------------------------------------|#

(define-fluid *top-level-envt*)

;; there is a special protocol used by start/handler.scm,
;; where args == #f to suppress printing of the signal message
;; (because the handler has already printed it)

(define (fatal-condition-handler (arg <condition>) next-handler)
  ;; first, though, install another handler, in case we get
  ;; an error in the process of generating the error message
  ;; (this can happen if there's a bug in the system, or dynamicity
  ;; causes the user to supply some bad code indirectly in something
  ;; we call (like a write-object method, for example))
  (handler-bind (<condition> (lambda (a n)
			       (process-exit 66)))
    (let ((p (current-error-port)))
      (write-string p "Fatal error:\n")
      (display-object arg p)
      (if (getenv "RS_DEBUG")
	  ;; if RS_DEBUG is turned on, print out a stack trace
	  (begin
	    ;; display an apply backtrace, if there is one
	    (if (apply-backtrace-available?)
		(apply-backtrace))
	    ;
	    (ccbt)))
      ;; its still a fatal error...
      (process-exit 1))))

(define (apply-backtrace-available?)
  (and (memq 'apply-backtrace (get-compile-options))
       ; calling this fn w/o a #t or #f makes no change, but still returns val
       (set-apply-trace-flag! 0)))

(define (repl-condition-handler (arg <condition>) next-handler)
  (call-with-markup
   *console-output-port*
   '(repl-condition)
   (lambda ()
     (display arg *console-output-port*)))
  ;;
  (let ((r *cmd-loop*))
    (if r
	(let ((breaks-deep (cmd-loop-depth r)))
	  (sub-loop r
                    (if (eq? breaks-deep 0)
                        "break[~d]=>"
                        (string-append "break^"
                                       (number->string (+ 1 breaks-deep))
                                       "[~d]=>"))
                    condition: arg)
	  ((cmd-loop-restart r)))
	(begin
	  (cmd-loop (fluid-ref *top-level-envt*) 
                    "break[~d]=>"
                    condition: arg)
	  (process-exit 1)))))

(define (repl-interrupt)
  ;; we are called with RS signals disabled; re-enable them
  (os-set-sigenable #t)
  (let ((r *cmd-loop*))
    (if r
	(sub-loop r "interrupt[~d]=>")
	(cmd-loop (make-user-initial) "interrupt[~d]=>"))))

(define (restart-with path proc)
  (if (instance? path <file-name>)
      (restart-with (pathname->os-path path) proc)
      (begin
	(if (not (string? path))
	    (error "restart-with: path ~s not a string" path))
	(if (not (procedure? proc))
	    (error "restart-with: proc ~s not a procedure" proc))
	(let ((v (clone (rscheme-global-ref 0))))
	  (vector-set! v 4 proc)
	  (vector-set! (vector-ref v 0) 26 (rscheme-global-ref 26))
	  (generic-save-boot-image path v)
	  (values)))))

(define (generic-save-boot-image path vec)
  (let ((x (extension (string->file path))))
    (if (and x (string=? x "fas"))
	(if (module-loaded? 'fasl)
	    ((value (get-binding-from-module 'fasl 'save-fasl-image))
	     path
	     #f
	     (vector vec #f))
	    (error "~a: cannot save in fasl format" path))
	(save-image path vec '#() '#() #f))))

(define *default-user-initial* #f)


(%early-once-only
 (format #t "Initializing user modules...\n")
 (init-user-initial-modules)
 (set! *default-user-initial* (get-module 'user)))

(define (main args)
  (if (and *verbose* *license*)
      (repl-greeting))
  ;;
  (if (and (pair? args)
	   (pair? (cdr args))
	   (string=? (car args) "--in"))
      (let ((m (cadr args)))
	;; note: we want to explicitly change the default user initial
	;; so that if an image is saved, when it is restarted it will
	;; be in the same environment
	(set! *default-user-initial*
	      (handler-case
	       (get-module (string->symbol m))
	       ((<condition> condition: c)
		(format $standard-error-port
			"** error trying to access module ~s\n" m)
		(display c $standard-error-port)
		*default-user-initial*)))
	(set! args (cddr args))))
  (run-repl args *default-user-initial*)
  (process-exit 0))

(define (run-repl args module)
  (if *script*
      (run-repl* args module)
      (with-edit-port
       (current-input-port)
       (current-output-port)
       (current-error-port)
       (lambda ()
	 (run-repl* args module)))))

(define (run-repl* args module)
  (fluid-let ((*top-level-envt* (top-level-envt module)))
    ;; errors you get before the cmd-loop starts are FATAL
    ;; (this is sometimes annoying, but usually good.  This
    ;; way scripts and makefiles and the like don't jump into
    ;; break loops at the slightest provocation, which can
    ;; be obnoxious for automated builds)
    (handler-bind (<condition> fatal-condition-handler)
      (interpret-repl-args args (top-level-envt module)))))

;; after all the command-line arguments are processed, we call this function
;; to actually start the REPL command loop

(define (basic-start-repl envt)
  (handler-bind (<condition> repl-condition-handler)
    (register-interrupt-handler! 'control-c repl-interrupt)
    (cmd-loop envt $default-prompt)))

;; the threaded shell installs its own start-repl proc

(define start-repl basic-start-repl)

(define (set-start-repl-proc! proc)
  (set! start-repl proc))

;; made a part of the build-bootable process...
;;
;; (searches for a binding for main & sticks it in an rscheme-global)
;;
;;(set-main! repl-main)

(define repl-greeting
  (lambda ()
    (display
     "type \",warranty\" for details; type \",help\" for some help\n")))

(%early-once-only
 (define *rc-files*
   (vector (string->file "~/.R7.scm")
	   (string->file "[resource]/R7.scm")))) ;; [install]/resource

(define (load-rc-files envt)
  (let (((n <fixnum>) (vector-length *rc-files*)))
    (let loop (((i <fixnum>) 0))
      (if (fixnum<? i n)
          (begin
            (load-if-exists (vector-ref *rc-files* i) envt)
            (loop (add1 i)))))))

(define (load-if-exists (path <file-name>) envt)
  (if (file-exists? path)
      (fluid-let ((*verbose* #f))  ;; [historical marker] this is the first
	                           ;; place where we used the new style of
                                   ;; fluid-let to it's advantage -- 
                                   ;; *verbose* was never a "fluid variable"
			           ;; but we can fluidly bind it anyway!
	(load-1 path envt))))

(define $default-prompt "top[~d]=>")

(define (run-script script envt args)
  (let* ((result (load-1 script envt))
	 (b (lookup envt 'main)))
    (if (and b (procedure? (value b)))
	((value b) args)
	result)))

;;
;;  this function processes the command line arguments to
;;  the REPL.  As its final action, it invokes a cmd-loop
;;  if one is to be run.
;;
;;  Among other things, it sets *args* the appropriate
;;  list of command-line arguments
;;

(define (interpret-repl-args args envt)
  (if (and *script*
	   (pair? args))
      ;;
      ;; if we're in script mode, all of the arguments get bound to *args*
      ;; and the rc file does NOT get loaded
      ;;
      (begin
	(set-app-args! (cdr args))
	(run-script (car args) envt (cdr args)))
      ;;
      ;; otherwise, interpret the arguments
      ;;
      (begin
	;;
	;; load the ".rc" file, if it's not disabled
	;;
	(if (and (pair? args)
		 (string=? (car args) "-norc"))
	    (set! args (cdr args))
	    (load-rc-files envt))
	;;
	;; get the args following "--" first
	;;
	(let ((m (member "--" args)))
	  (if m
	      (set-app-args! (cdr m))))
	;;
	;; process the other command line arguments
	;;
	(let ((save-later #f))
	  (let loop ((args args))
	    (if (null? args)
		(if save-later
 		    (save-later) ;; if "-c" was specified
		    (start-repl envt))
		(let (((a <string>) (car args)))
		  (cond
		   ;;
		   ((string=? a "--")
		    ;;
		    ;; this has already been processed!, just go
		    ;; straight to the end
		    ;;
		    (loop '()))
		   ;;
		   ((string=? a "-exit")
		    (values))
		   ;;
		   ((string=? a "-e")
		    (eval-string (cadr args) envt)
		    (loop (cddr args)))
		   ;;
		   ;; -c[.repl] <path> [file...]
		   ;;
		   ;;  make a bootable image in <path>, with file... 
		   ;;  loaded.  bootable image will call main in
		   ;;  target image, or REPL main if -c.repl is specified
		   ;;
		   ;;  exits after generating the output file
		   ;;
		   ((and (>= (string-length a) 2)
			 (string=? (substring a 0 2) "-c"))
		    (if (not (pair? (cdr args)))
			(error "-c[.repl] <image-file> missing image file argument."))
		    (set! save-later 
			  (lambda ()
			    (do-save-image a (cadr args) envt)))
		    (loop (cddr args)))
		   ;;
		   ((string=? a "-v") ;; compile things verbosely
		    (set! *compile-verbose* #t)
		    (loop (cdr args)))
		   ;;
		   ((string=? a "-m")
		    (if (pair? (cdr args))
			(begin
			  (load-module-from-path (cadr args))
			  (loop (cddr args)))
			(error "missing argument for '-m' option")))
		   ;;
		   ((and (> (string-length a) 1)
			 (eq? (string-ref a 0) #\+))
		    (use-in (string->symbol (substring a 1)) envt)
		    (loop (cdr args)))
		   ;;
		   (else
		    (load-1 a envt)
		    (loop (cdr args)))))))))))

;;; interpret a save image spec, which is one of:
;;;   ""     => main in current envt
;;;   ".FOO" => main in given module

(define (parse-main-spec (a <string>) envt)
  (if (and (> (string-length a) 0)
	   (eq? (string-ref a 0) #\.))
      (main-from-module (string->symbol (substring a 1)))
      (eval-in-envt 'main envt)))

(define (main-from-module n)
  (if (eq? n 'repl)
      main  ;; return our main if module is 'repl'
      (let* ((m (get-module n))
	     (v (table-lookup (module-exports m) 'main)))
	(cond
	 ((not v)
	  (error "main: not exported from module ~s" n))
	 ((not (instance? v <top-level-var>))
	  (error "main (in ~s): not a top-level variable (is ~s)" n v))
	 ((not (procedure? (value v)))
	  (error "main (in ~s): value is not a procedure (is ~s)" 
		 n (value v)))
	 (else
	  (value v))))))

;;;

(define (do-save-image opt file envt)
  (if *verbose*
      (format #t "saving to image: ~a\n" file))
  (run-hooks *image-save-hook*)
  (warn-about-unbound-vars-created)
  (set-exceptions-abort-process! #f)
  (clear-rewriter-envt) ;; object-table will be stale in output
  (let (((fn <function>) (if (string? opt)
                             (parse-main-spec (substring opt 2) envt)
                             opt)))
    (restart-with file fn)
    (values)))

;;;
;;;  usages:
;;;    -m FILE
;;;    -m NAME=FILE
;;;    -m +NAME

(define (load-module-from-path (path <string>))
  (if (eq? (string-ref path 0) #\+)
      (get-module (string->symbol (substring path 1)))
      (let ((i (string-search path #\=)))
	(bind ((module-name 
		module-path 
		(if i
		    (values (string->symbol
			     (substring path 0 i))
			    (substring path (+ i 1)))
		    (values (string->symbol
			     (filename 
			      (string->file path)))
			    path))))
	  (link-load-module module-name (string->file module-path))))))

(define (set-compiler-verbose-mode! flag)
  (set! *compile-verbose* flag))
