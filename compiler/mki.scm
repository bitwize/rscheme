#|------------------------------------------------------------*-Scheme-*--|
 | File:    compiler/mki.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.22
 | File mod date:    2002-11-05 21:30:34
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  (rsc)
 |
 `------------------------------------------------------------------------|#

,(use editinp)

;;; if we are in pre-0.7.3, add `#optional'

(if (not (string->unique-object "#optional"))
    (add-unique-object! "#optional" 9))

(define make-user-initial (exported-value repl make-user-initial))
(define cmd-loop (exported-value repl cmd-loop))
(define repl-condition-handler (exported-value repl repl-condition-handler))

(define *the-envt* (make-user-initial))

;;; if we are pre-0.7.3.1, then we don't have SRFI-0, so
;;; put in a stubby implementation of one

(if (not ((exported-value compiler lookup) *self* 'if-implements))
    (eval '(define-macro (if-implements fid t f) f) *the-envt*))

;; move some config defn's into the compiler's envt
;;

(define-syntax (push-value-into-compiler name)
  (eval `(define ,(mquote name) ',name) *the-envt*))

(push-value-into-compiler *dist-path*)
(push-value-into-compiler *rscheme-version*)
(push-value-into-compiler *rscheme-build*)
(push-value-into-compiler *rscheme-build-vlist*) ;; version as a num list
(push-value-into-compiler *save-image-signature*)
(push-value-into-compiler *relative-file-paths*)

;;

(eval '(load "c") *the-envt*)
(eval '(load "process") *the-envt*)

(define *history* '())

;; bootstrapping issue:
;;   if we're running from within < 0.7.3, then os-mkdir doesn't
;;   exist, and we have to use unixm and use `mkdir' instead

(if ((exported-value compiler lookup) *self* 'os-mkdir)
    (eval '(define my-mkdir-1 (lambda (p) (os-mkdir p) (values))) *self*)
    (eval '(define my-mkdir-1 (exported-value unixm mkdir)) *self*))

(define (my-mkdir path)
  (let loop ((p (dir-parents (string->dir path))))
    (if (null? p)
	(values)
	(begin
	  (if (not (file-exists? (car p)))
	      (my-mkdir-1 (pathname->os-path (car p))))
	  (loop (cdr p))))))

;; put eval and an envt-constructor
;;  (but not all the other crap that comes with it)
;;  in the envt

(eval `(define *self* ',*the-envt*) *the-envt*)
(eval `(define eval ',eval) *the-envt*)
(eval `(define mkdir ',my-mkdir) *the-envt*)

(define (terminate-interrupt-handler)
  (display "*** Interrupted\n" *console-error-port*)
  (process-exit 1))

(define (terminal-signal-handler args)
  (display "**HALT**\n" *console-error-port*)
  (cond
   ((and (pair? args)
	 (instance? (car args) <condition>))
    (display (car args) *console-error-port*))
   ((and (pair? args)
	 (string? (car args)))
    (write-string *console-error-port* "signal: ")
    (apply format *console-error-port* args)
    (newline *console-error-port*))
   ((instance? args <condition>)
    ;; 0.7-0.6 and later have decent condition handling...
    (display args *console-error-port*)
    (if (set-apply-trace-flag! #f)
        (with-module repl (apply-backtrace args)))))
  (process-exit 1))

(define (main args)
  ;;
  (for-each (lambda (h)
	      (format #f "                 (v0.7, ~a)\n" h))
	    *history*)
  ;;
  (fluid-let ((*signal-handler* terminal-signal-handler))
    ;; `within-directory' behaves differently in 0.7.3
    ;; compared to 0.7.2, but because 0.7.3 doesn't store
    ;; the *current-dir* persistently (in the saved image),
    ;; it works out that we can just use this here
    (within-directory
     $dot-dir
     (lambda ()
       ;;
       (register-interrupt-handler! 'control-c terminate-interrupt-handler)
       ;;
       (eval '($awake) *the-envt*)
       ;;
       ;; strip off a `-BC path' argument very early
       ;;
       (if (and (pair? args)
		(string=? (car args) "-BC"))
	   (begin
	     (eval `(load ,(cadr args)) *the-envt*)
	     (set! args (cddr args))))
       ;;
       ;; strip off a `-config-basis level' argument very early, too
       ;;
       (if (and (pair? args)
		(string=? (car args) "-config-basis"))
	   (begin
	     ((eval 'configure-basis-from-modules *the-envt*)
	      (cdr (assoc (cadr args)
			  '(("+precore" primops *scheme* precore)
			    ("+core" primops *scheme* precore corelib)
			    ("+low" primops *scheme* precore corelib low-scheme)
			    ("start" primops *scheme*)))))
	     (set! args (cddr args))))
       ;;
       (if (or (null? args)
	       (eq? (eval `(process-cmd-line ',args) *the-envt*) #t))
	   (do-cmd-line))))))

(define (do-cmd-line)
  (display "-- interactive interface no longer supported\n"))

#|
;; 0.7-0.6 & later version

(define (do-cmd-line)
  (fluid-let ((*signal-handler* default-signal-handler))
    (handler-bind (<condition> repl-condition-handler)
      (cmd-loop *the-envt* "rsc[~d]=>"))))
|#

(vector-set! (rscheme-global-ref 0) 
	     1
	     "RScheme Compiler (v0.7.3)")
