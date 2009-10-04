#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/start/start.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.18
 | File mod date:    2003-10-13 13:02:56
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  start
 |
 | Purpose:          Provide the entry point procedure
 `------------------------------------------------------------------------|#

(define *script* #f)

(define-hook after-image-load)  ;; first thing in start (BEFORE normal inits)
                                ;; even before interrupts are enabled, or
                                ;; any presentation from Scheme level
                                ;; (can't even use `display')

(define-hook before-main-call)  ;; just before calling main

;;

;; the args passed here are the ones with low-level flags
;; like -image, -q, -script
;; already parsed out (so we don't have to)
;;
;; the "raw" args are available in boot_args rscheme_global[1]

(define-syntax (get-boot-args)
  (rscheme-global-ref 1))

(define-syntax (get-boot-image)
  (rscheme-global-ref 0))

(define (die)
  (fwrite/str (stderr) "** Interrupted\n")
  (process-exit 1))

;;;
;;;  note that before we run the initialization thunks, any
;;;  top-level defines with non-constant right-hand-sides
;;;  are not initialized, like $standard-output-port, for example
;;;

(define (start args verbose-flag script-flag)
  (run-hooks *after-image-load-hook*)
  ;;
  (register-interrupt-handler! 'control-c die)
  ;; now that we have a ^C handler, we can enable interrupts
  (os-set-sigenable #t)
  ;;
  (set-verbose! verbose-flag)
  (set! *script* script-flag)
  ;;
  (let ((info (get-boot-image)))
    ;;
    ;; greet the user if not running silently
    ;;
    (if verbose-flag
	(greeting))
    ;;
    ;; run the initialization thunks
    ;;
    (if (gvec-ref info 5)
	(begin
	  (fwrite/str (stdout) "Doing first-time initialization...\n")
	  (call-thunks (gvec-ref info 5))
	  (gvec-set! info 5 #f))
	(call-thunks (gvec-ref info 3)))
    ;;
    ;; call the "main" program
    ;; from within a threads context (if available)
    ;;
    (start-threads-hook
     (lambda ()
       (run-hooks *before-main-call-hook*)
       ((gvec-ref info 4) args)))))

(define (call-thunks (lst <pair>))
  (let loop (((inits <pair>) lst)
             ((i <fixnum>) 0))
    (let ((f (car inits)))
      #|
         This was useful in debugging a spin-loop-failure 
         during initialization...

      (let ((n (gvec-ref (gvec-ref f 0) 2)))
        (fwrite/str (stdout) " calling init thunk[")
        (fwrite/str (stdout) (fixnum->string i 10))
        (fwrite/str (stdout) "] @")
        (fwrite/str (stdout) (machine-bits->string n))
        (fwrite/str (stdout) "  (")
        (fwrite/str (stdout) (symbol->string (caddar n)))
        (fwrite/str (stdout) ")...\n"))
      |#
      (f)
      (if (pair? (cdr inits))
          (loop (cdr inits) (add1 i))))))

;;; initialize thread variables
;;;
;;; (note that this gets invoked as part of `call-thunks', above)
;;;
;;; It has to come after iolib gets initialized because the thread
;;; variable initial values are computed as part of the initialization
;;; thunks
;;;
;;; Also, we have to do this every time (not just init time) because
;;; how else would the thread-state-reg get set?

(set-thread-state-reg! (thread-var-initial-state))

;;;
;;;  provide a hook for a threads module to intercept the
;;;  call to the main procedure
;;;

(define start-threads-hook funcall)

(define (set-start-threads-hook! hook)
  (set! start-threads-hook hook))

