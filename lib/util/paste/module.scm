;;;  paste.scm -- (v1, 1997-09-26, dmk)
;;;
;;;   paste from X selection
;;;
;;; PREREQ:  `xselection' in your path
;;;
;;; (from the X FAQ:
;;;  [...] a program by Richard Hesketh (rlh2@ukc.ac.uk) specifically for
;;;  manipulating the selection will help; [...]
;;;  A version is on ftp.x.org.
;;;  Also available is ria.ccs.uwo.ca:pub/xget_selection.tar.Z, which can
;;;  be adapted to do this.)
;;;
;;; INSTALL by loading this file -- then ",paste" will eval from
;;; the xselection.
;;;
;;; can also put in ~/lib/rs/modules/paste/module.scm and then say
;;; ",(use paste)"
;;;

(define-module util.paste (unquote)
  ,(use usual-inlines repl)
  ;;
  (define (paste)
    (let* ((in (open-input-process "xselection PRIMARY"))
	   ;; note that we can't just use `read-string', because
	   ;; read-string will stop reading when no more input is
	   ;; ready, even if the pipe isn't closed
	   (data (port->string in)))
      (close-input-port in)
      data))
  ;;
  (define (do-paste envt)
    (with-objects-from-port
     (open-input-string (paste))
     (lambda (s-expr)
       (eval s-expr envt))))
  ;;
  (define-command-proc 
	paste 
	do-paste
	((",paste" "paste from xselection")))
  ;;
  ,(export paste))
