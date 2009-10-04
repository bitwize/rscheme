#|------------------------------------------------------------*-Scheme-*--|
 | File:    compiler/process.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.14
 | File mod date:    2003-12-15 09:13:31
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  (rsc)
 |
 `------------------------------------------------------------------------|#


;; this is called when we are re-awakened from a saved image

(define random-delta 0)

#|
(register-interrupt-handler! 
 6
 (lambda (pid t rc) 
   (format #t "process ~d: ~s ~d\n" pid t rc)))
|#

(define (my-random)
  (bitwise-and (+ (random) random-delta) #x1fffffff))

(define *process-dir* #f)
(define *initial-pragmas* '())

;;; These are used to control where output goes,
;;; overriding anything specified or inferred from 
;;; the module control form.  They are set using
;;; --target-dest-dir= and --target-image-dir= command-line
;;; options

(define *target-dest-dir* #f)
(define *target-image-dir* #f)

;;;
;;;  In `new-build' mode ("--new-build"), rsc generates only the
;;;  bare minimum of files:
;;;
;;;    (1) the .c files from the .scm files, without any number bumping
;;;    (2) the .h file representing the module itself
;;;    (3) the .mif, .mx, and .doc files

(define *new-build* #f)

;;;  Normally, `rsc' operates in "cross-compiling" mode, because
;;;  it is used to bootstrap the system by compiling all the sources
;;;  to C code.  Sometimes, it operates in "native" mode, when it
;;;  is used to compile code for its own install.  
;;;
;;;  In cross-compiling mode, we don't interpret system configuration
;;;  settings of the underlying system as applying to the target
;;;  environment (e.g., the install directory)
;;;

(define *native-mode* #f)

(define ($awake)
  (for-each (lambda (thunk) (thunk))
	    $init-thunks)
  (set! *process-dir* (process-directory))
  (let ((p (open-input-process "date")))
    (set! random-delta (string->hash (read-line p)))
    (close-input-port p))
  ;;
  (if (and *native-mode* (getenv "RS_INSTALL_DIR"))
      (set! *dist-install-resource-dir*
	    (string->dir (string-append (getenv "RS_INSTALL_DIR")
					"/resource"))))
  (add-special-root! "[dist]"
		     (make <root-dir>
			   root-name: "[dist]/"
			   expanded-name: (pathname->string *dist-path*))))

;; this is called when we get arguments
;; (but it is NOT called when we don't get arguments)


(define (process-cmd-line args)
  (if (null? args)
      #t  ;; start a REPL by default
      (process-cmd-line* (car args) (cdr args))))

(define (process-cmd-line* (sw <string>) cdr-args)
  (cond
   ((string=? sw "-?")
    (display "usage:\n")
    (display "  rsc file... -- process .mcf's\n")
    (display "  rsc -x -- reload changed files in compiler\n")
    (display "  rsc -o output extra-module... -- link bootable image\n")
    #f)
   ((string=? sw "-repl")
    #t)
   ((string=? sw "-e")
    (eval (with-input-from-string (car cdr-args) read) *self*)
    (process-cmd-line (cdr cdr-args)))
   ((string=? sw "-exit")
    #f)
   ((string=? sw "-I")
    (set! *dist-install-resource-dir*
	  (append-dirs (string->dir (car cdr-args))
		       (string->dir "resource")))
    (set! *dist-resource-path*
	  (cons *dist-install-resource-dir*
		*dist-resource-path*))
    (reinit-library-path)
    (process-cmd-line (cdr cdr-args)))
   ((or (string=? sw "-o")
	(string=? sw "-o.noconf"))
    (let ((target (car cdr-args)))
      (if (string=? target "-")
	  (set! target (pathname->string
			(install-resource-path "system.img"))))
      (set! *modules* (append *modules* (map string->symbol (cdr cdr-args))))
      (format #t "building: ~s\n" target)
      (format #t "modules: ~s\n" *modules*)
      (write-bootable-image target)
      ;;
      ;; construct the configuration files
      ;;
      (if (not (string=? sw "-o.noconf"))
	  (create-config-files (map get-ct-module *modules*)))
      #f))
   ((string=? sw "-p")
    ;; package mode
    (set! *native-mode* #t)             ; XXX is this right?
    (fluid-let ((*package-mode* #t))
      (set! $module-makefile
	    "$(INSTALL_DIR)/resource/buildenv/module.mak")
      (process-cmd-line cdr-args)))
   ;;
   ((string=? sw "-rsrc")
    (set! *dist-resource-path*
	  (cons (string->dir (car cdr-args)) *dist-resource-path*))
    (reinit-library-path)
    (process-cmd-line (cdr cdr-args)))
   ;;
   ((string=? sw "-P")
    ;; package mode, with a new dist dir
    (fluid-let ((*package-mode* #t))
      (set! *dist-path*
	    (string->dir (car cdr-args)))
      (set! *dist-resource-path*
	    (cons (append-dirs *dist-path* (string->dir "resource"))
		  *dist-resource-path*))
      (reinit-library-path)
      (set! $module-makefile
	    "$(INSTALL_DIR)/resource/buildenv/module.mak")
      (process-cmd-line (cdr cdr-args))))
   ;;
   ((string=? sw "-v")
    (set! *tl-report* #t)
    (process-cmd-line cdr-args))
   ((string=? sw "--new-build")
    (set! *new-build* #t)
    (process-cmd-line cdr-args))
   ((string=? sw "-make-recopies")
    (set! *make-copy-mode* #t)
    (process-cmd-line cdr-args))
   ((string=? sw "-ccode")
    (fluid-let ((*strategy* 'ccode))
      (process-cmd-line cdr-args)))
   ((string=? sw "-pragma")
    (set! *initial-pragmas* (cons (string->symbol (car cdr-args))
				  *initial-pragmas*))
    (process-cmd-line (cdr cdr-args)))
   ((and (> (string-length sw) 11)
	 (string=? (substring sw 0 11) "--makefile="))
    (set! *makefile-name* (substring sw 11))
    (process-cmd-line cdr-args))
   ((and (> (string-length sw) 18)
         (string=? (substring sw 0 18) "--target-dest-dir="))
    (set! *target-dest-dir* (string->dir (substring sw 18)))
    (process-cmd-line cdr-args))
   ((and (> (string-length sw) 19)
         (string=? (substring sw 0 19) "--target-image-dir="))
    (set! *target-image-dir* (string->dir (substring sw 19)))
    (process-cmd-line cdr-args))
   (else
    ;; normal compilation...
    (for-each (lambda (a)
		(format #t "processing: ~a\n" a)
		(let ((bcx (mcf-load a)))
		  (if *makefile-name*
		      (set-makefile-name! bcx *makefile-name*))
		  (write-module bcx)))
	      (cons sw cdr-args))
    #f))) ;; return #f to suppress the starting of a REPL

(define *makefile-name* #f)
